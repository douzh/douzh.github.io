# c8-2 多轮对话管理

## 1. 概述

多轮对话管理让 AI智能体能够维护上下文、理解指代、处理话题切换，实现自然流畅的连续对话。本章详解对话状态跟踪、上下文管理和澄清策略。

## 2. 对话状态跟踪

### 2.1 基于帧的状态表示

```javascript
class DialogState {
  constructor() {
    this.intent = null;           // 当前意图
    this.slots = new Map();       // 槽位值
    this.history = [];            // 对话历史
    this.context = {};            // 上下文信息
    this.lastUpdateTime = Date.now();
  }
  
  // 更新槽位
  updateSlot(slotName, value, confidence = 1.0) {
    this.slots.set(slotName, {
      value,
      confidence,
      updatedAt: Date.now(),
      source: 'user' // 'user', 'inferred', 'default'
    });
  }
  
  // 获取槽位值
  getSlot(slotName) {
    const slot = this.slots.get(slotName);
    return slot ? slot.value : null;
  }
  
  // 检查槽位是否已填充
  isSlotFilled(slotName) {
    return this.slots.has(slotName) && 
           this.slots.get(slotName).confidence > 0.5;
  }
  
  // 添加对话历史
  addHistory(role, content) {
    this.history.push({
      role,
      content,
      timestamp: Date.now()
    });
    
    // 保持最近 10 轮
    if (this.history.length > 20) {
      this.history = this.history.slice(-20);
    }
  }
  
  // 获取最近的对话
  getRecentHistory(rounds = 3) {
    return this.history.slice(-rounds * 2);
  }
  
  // 重置状态
  reset(keepContext = false) {
    this.intent = null;
    this.slots.clear();
    this.history = [];
    if (!keepContext) {
      this.context = {};
    }
  }
  
  // 序列化
  toJSON() {
    return {
      intent: this.intent,
      slots: Array.from(this.slots.entries()),
      history: this.history,
      context: this.context,
      lastUpdateTime: this.lastUpdateTime
    };
  }
}

// 使用示例
const state = new DialogState();

state.intent = 'send_email';
state.updateSlot('recipient', 'zhangsan@example.com');
state.updateSlot('subject', '会议通知');
state.addHistory('user', '帮我给张三发邮件');
state.addHistory('assistant', '好的，请问邮件主题是什么？');

console.log(state.getSlot('recipient')); // 'zhangsan@example.com'
console.log(state.isSlotFilled('body')); // false
```

### 2.2 对话状态追踪器

```javascript
class DialogStateTracker {
  constructor() {
    this.states = new Map(); // userId -> DialogState
    this.maxStates = 1000;
  }
  
  // 获取或创建用户状态
  getState(userId) {
    if (!this.states.has(userId)) {
      this.states.set(userId, new DialogState());
    }
    
    const state = this.states.get(userId);
    state.lastUpdateTime = Date.now();
    
    return state;
  }
  
  // 更新状态
  updateState(userId, intent, entities = []) {
    const state = this.getState(userId);
    state.intent = intent;
    
    // 提取并填充槽位
    for (const entity of entities) {
      state.updateSlot(entity.type, entity.value, entity.confidence);
    }
    
    console.log(`Updated state for ${userId}:`, state.toJSON());
    return state;
  }
  
  // 添加对话记录
  addTurn(userId, role, content) {
    const state = this.getState(userId);
    state.addHistory(role, content);
  }
  
  // 检查是否需要更多槽位
  needsMoreSlots(userId, requiredSlots) {
    const state = this.getState(userId);
    
    const missing = requiredSlots.filter(slot => 
      !state.isSlotFilled(slot)
    );
    
    return {
      needsMore: missing.length > 0,
      missingSlots: missing
    };
  }
  
  // 清理过期状态（30 分钟无活动）
  cleanup() {
    const now = Date.now();
    const timeout = 30 * 60 * 1000;
    
    for (const [userId, state] of this.states.entries()) {
      if (now - state.lastUpdateTime > timeout) {
        this.states.delete(userId);
        console.log(`Cleaned up stale state for ${userId}`);
      }
    }
  }
}

// 使用示例
const tracker = new DialogStateTracker();

// 第一轮
tracker.updateState('user_123', 'send_email', [
  { type: 'recipient', value: 'zhangsan@example.com', confidence: 0.95 }
]);
tracker.addTurn('user_123', 'user', '帮我给张三发邮件');

// 检查槽位
const check = tracker.needsMoreSlots('user_123', ['recipient', 'subject', 'body']);
console.log(check);
// { needsMore: true, missingSlots: ['subject', 'body'] }
```

## 3. 多轮对话流程

### 3.1 槽位填充对话

```javascript
class SlotFillingDialog {
  constructor(tracker, nlu) {
    this.tracker = tracker;
    this.nlu = nlu;
    
    // 定义每个意图所需的槽位
    this.slotDefinitions = {
      send_email: {
        required: ['recipient', 'subject', 'body'],
        prompts: {
          recipient: '请问要发给谁？',
          subject: '邮件主题是什么？',
          body: '请告诉我邮件正文内容'
        }
      },
      
      set_reminder: {
        required: ['time', 'content'],
        optional: ['repeat'],
        prompts: {
          time: '什么时候提醒您？',
          content: '提醒您做什么？',
          repeat: '需要重复提醒吗？（每天/每周）'
        }
      },
      
      query_weather: {
        required: ['location'],
        optional: ['date'],
        prompts: {
          location: '请问哪个城市？',
          date: '查询哪天的天气？（默认明天）'
        }
      }
    };
  }
  
  async processTurn(userId, userInput) {
    const state = this.tracker.getState(userId);
    
    // NLU 理解
    const nluResult = await this.nlu.understand(userInput);
    
    // 如果是新意图，重置状态
    if (nluResult.intent !== state.intent) {
      state.reset(true); // 保留上下文
      state.intent = nluResult.intent;
    }
    
    // 更新槽位
    this.tracker.updateState(userId, nluResult.intent, nluResult.entities);
    
    // 检查是否还有缺失槽位
    const slotDef = this.slotDefinitions[nluResult.intent];
    if (!slotDef) {
      return { response: '抱歉，我不理解这个请求' };
    }
    
    const check = this.tracker.needsMoreSlots(userId, slotDef.required);
    
    if (check.needsMore) {
      // 询问第一个缺失的槽位
      const missingSlot = check.missingSlots[0];
      const prompt = slotDef.prompts[missingSlot];
      
      return {
        response: prompt,
        action: 'ask_slot',
        slot: missingSlot
      };
    } else {
      // 所有槽位已填充，执行任务
      return await this.executeTask(userId, state);
    }
  }
  
  async executeTask(userId, state) {
    switch (state.intent) {
      case 'send_email':
        const emailData = {
          to: state.getSlot('recipient'),
          subject: state.getSlot('subject'),
          body: state.getSlot('body')
        };
        
        // 调用实际发送邮件的函数
        await sendEmail(emailData);
        
        return {
          response: `邮件已发送给${emailData.to}`,
          action: 'complete',
          data: emailData
        };
        
      case 'set_reminder':
        const reminderData = {
          time: state.getSlot('time'),
          content: state.getSlot('content'),
          repeat: state.getSlot('repeat') || 'once'
        };
        
        await createReminder(reminderData);
        
        return {
          response: `好的，将在${reminderData.time}提醒您${reminderData.content}`,
          action: 'complete',
          data: reminderData
        };
        
      default:
        return {
          response: '任务已完成',
          action: 'complete'
        };
    }
  }
}

// 使用示例
const dialog = new SlotFillingDialog(tracker, nlu);

// 第一轮
let response = await dialog.processTurn('user_123', '帮我发邮件');
console.log(response.response);
// "请问要发给谁？"

// 第二轮
response = await dialog.processTurn('user_123', '发给张三');
console.log(response.response);
// "邮件主题是什么？"

// 第三轮
response = await dialog.processTurn('user_123', '主题是会议通知');
console.log(response.response);
// "请告诉我邮件正文内容"

// 第四轮
response = await dialog.processTurn('user_123', '说明天开会的时间改到下午 3 点');
console.log(response.response);
// "邮件已发送给张三"
```

### 3.2 上下文指代消解

```javascript
class CoreferenceResolver {
  constructor() {
    this.pronouns = new Map([
      ['他', 'person'],
      ['她', 'person'],
      ['它', 'object'],
      ['他们', 'person_plural'],
      ['她们', 'person_plural'],
      ['它们', 'object_plural'],
      ['这', 'this'],
      ['那', 'that'],
      ['这里', 'location'],
      ['那里', 'location']
    ]);
  }
  
  resolve(text, dialogHistory) {
    // 查找代词
    const pronounMatches = [];
    for (const [pronoun, type] of this.pronouns.entries()) {
      const regex = new RegExp(pronoun, 'g');
      let match;
      while ((match = regex.exec(text)) !== null) {
        pronounMatches.push({
          pronoun,
          type,
          index: match.index,
          text: text
        });
      }
    }
    
    if (pronounMatches.length === 0) {
      return { resolvedText: text, references: [] };
    }
    
    // 从历史中寻找指代对象
    const references = [];
    let resolvedText = text;
    
    for (const match of pronounMatches) {
      const antecedent = this.findAntecedent(match, dialogHistory);
      
      if (antecedent) {
        references.push({
          pronoun: match.pronoun,
          antecedent,
          type: match.type
        });
        
        // 替换代词为实际指代对象
        resolvedText = resolvedText.replace(
          match.pronoun,
          antecedent
        );
      }
    }
    
    return { resolvedText, references };
  }
  
  findAntecedent(pronounMatch, history) {
    // 简化实现：寻找最近提到的匹配实体
    const recentHistory = history.slice(-6); // 最近 3 轮
    
    for (const turn of recentHistory.reverse()) {
      if (turn.role !== 'user' && turn.role !== 'assistant') continue;
      
      // 提取可能的先行词
      const entities = this.extractEntities(turn.content);
      
      for (const entity of entities) {
        if (this.matchesType(entity.type, pronounMatch.type)) {
          return entity.text;
        }
      }
    }
    
    return null;
  }
  
  extractEntities(text) {
    // 简化的实体提取
    const entities = [];
    
    // 人名
    const personRegex = /[张王李赵刘陈杨][三四五六七八九十]/g;
    let match;
    while ((match = personRegex.exec(text)) !== null) {
      entities.push({ type: 'person', text: match[0] });
    }
    
    // 地名
    const locationRegex = /(北京 | 上海 | 广州|深圳)/g;
    while ((match = locationRegex.exec(text)) !== null) {
      entities.push({ type: 'location', text: match[0] });
    }
    
    return entities;
  }
  
  matchesType(entityType, pronounType) {
    if (pronounType === 'person') {
      return entityType === 'person';
    }
    if (pronounType === 'location') {
      return entityType === 'location';
    }
    // ... 其他类型匹配
    return false;
  }
}

// 使用示例
const resolver = new CoreferenceResolver();

const history = [
  { role: 'user', content: '明天北京的天气怎么样？' },
  { role: 'assistant', content: '北京明天晴朗，温度 25 度' },
  { role: 'user', content: '那上海呢？' }
];

const result = resolver.resolve('那上海呢？', history);
console.log(result.resolvedText); // "那上海的天气呢？"

const result2 = resolver.resolve('他怎么样？', history);
console.log(result2.resolvedText); // 如果有提到人，会替换
```

---

**下一节：** [c8-3 意图识别与槽位填充](./c8-3-intent-recognition.md)  
**上一节：** [c8-1 自然语言理解设计](./c8-1-nlu-design.md)
