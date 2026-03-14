# c8 UX 设计其他主题概要

## c8-3 意图识别与槽位填充进阶

### 隐式参数推断

```javascript
class ImplicitParameterInference {
  constructor(knowledgeBase) {
    this.kb = knowledgeBase;
  }
  
  infer(userInput, context) {
    // 从上下文推断
    const inferred = {};
    
    // 示例："帮我给张三发邮件" → 需要推断张三的邮箱
    if (userInput.includes('给张三发邮件')) {
      // 查询知识库中张三的联系信息
      const zhangsanInfo = this.kb.getContact('张三');
      
      if (zhangsanInfo?.email) {
        inferred.recipient = zhangsanInfo.email;
        inferred.recipientSource = 'knowledge_base';
        inferred.confidence = 0.9;
      }
    }
    
    // 时间推断："下午 3 点" → 完整时间
    if (userInput.match(/下午\s*\d+点/)) {
      const match = userInput.match(/下午\s*(\d+)点/);
      const hour = parseInt(match[1]);
      
      // 如果是未来时间，用今天；否则用明天
      const now = new Date();
      const targetTime = new Date();
      targetTime.setHours(hour + 12, 0, 0, 0);
      
      if (targetTime < now) {
        targetTime.setDate(targetTime.getDate() + 1);
      }
      
      inferred.time = targetTime.toISOString();
      inferred.timeSource = 'inferred';
    }
    
    return inferred;
  }
}

// 使用示例
const inferrer = new ImplicitParameterInference(knowledgeBase);

const inferred = inferrer.infer('帮我给张三发邮件，说下午 3 点开会', {
  currentTime: new Date('2024-03-07 14:00')
});

console.log(inferred);
/*
{
  recipient: 'zhangsan@example.com',
  recipientSource: 'knowledge_base',
  confidence: 0.9,
  time: '2024-03-07T15:00:00Z',
  timeSource: 'inferred'
}
*/
```

## c8-4 错误恢复与引导

### 优雅的错误处理策略

```javascript
class ErrorRecoveryManager {
  constructor() {
    this.errorTemplates = {
      slot_missing: [
        '抱歉，我还需要知道{slot}才能继续',
        '请告诉我{slot}是什么',
        '您还没说{slot}呢'
      ],
      
      invalid_format: [
        '{slot}的格式好像不对，应该是{expectedFormat}',
        '这个{slot}不太对，请检查后重新输入',
        '{slot}无效，正确的格式是{expectedFormat}'
      ],
      
      ambiguous_input: [
        '您说的"{input}"有歧义，是指{option1}还是{option2}？',
        '我不太确定您的意思，能再说详细一点吗？',
        '请问您具体指的是什么？'
      ],
      
      system_error: [
        '抱歉，刚才出了点问题，能请您再说一遍吗？',
        '我遇到了一些技术问题，正在努力修复',
        '系统暂时不可用，请稍后再试'
      ]
    };
  }
  
  handleError(errorType, context = {}) {
    const templates = this.errorTemplates[errorType];
    if (!templates) {
      return '抱歉，出现了一个错误';
    }
    
    // 随机选择一个模板
    const template = templates[Math.floor(Math.random() * templates.length)];
    
    // 填充变量
    return template.replace(/{(\w+)}/g, (match, key) => {
      return context[key] || match;
    });
  }
  
  // 渐进式引导
  getProgressiveHint(missingSlots, conversationHistory) {
    const attemptCount = this.getAttemptCount(conversationHistory);
    
    if (attemptCount === 0) {
      // 第一次询问，直接问
      return `请问${missingSlots[0]}是什么？`;
    } else if (attemptCount === 1) {
      // 第二次，提供示例
      return `请告诉我${missingSlots[0]}，比如"example"`;
    } else {
      // 第三次，提供选项
      return `您还没提供${missingSlots[0]}，我可以帮您：
        1. 使用默认值
        2. 跳过这一步
        3. 人工协助
        请选择（1/2/3）`;
    }
  }
  
  getAttemptCount(history) {
    // 统计最近历史中询问同一槽位的次数
    let count = 0;
    for (let i = history.length - 1; i >= 0; i--) {
      if (history[i].role === 'assistant' && 
          history[i].content.includes('请问')) {
        count++;
      } else if (history[i].role === 'user') {
        break;
      }
    }
    return count;
  }
}

// 使用示例
const errorManager = new ErrorRecoveryManager();

console.log(errorManager.handleError('slot_missing', {
  slot: '收件人邮箱'
}));
// "抱歉，我还需要知道收件人邮箱才能继续"

console.log(errorManager.handleError('invalid_format', {
  slot: '电话号码',
  expectedFormat: '11 位手机号'
}));
// "电话号码的格式好像不对，应该是 11 位手机号"
```

## c8-5 任务进度跟踪

### 实时进度展示

```javascript
class TaskProgressTracker {
  constructor(taskId, totalSteps) {
    this.taskId = taskId;
    this.totalSteps = totalSteps;
    this.currentStep = 0;
    this.stepDetails = new Map();
    this.startTime = Date.now();
  }
  
  startStep(stepName, description = '') {
    this.currentStep++;
    this.stepDetails.set(this.currentStep, {
      name: stepName,
      description,
      startTime: Date.now(),
      status: 'in_progress'
    });
    
    return this.getProgressUpdate();
  }
  
  completeStep(result = null) {
    const step = this.stepDetails.get(this.currentStep);
    if (step) {
      step.endTime = Date.now();
      step.status = 'completed';
      step.result = result;
    }
    
    return this.getProgressUpdate();
  }
  
  failStep(errorMessage) {
    const step = this.stepDetails.get(this.currentStep);
    if (step) {
      step.endTime = Date.now();
      step.status = 'failed';
      step.error = errorMessage;
    }
    
    return this.getProgressUpdate();
  }
  
  getProgressUpdate() {
    const completed = Array.from(this.stepDetails.values())
      .filter(s => s.status === 'completed').length;
    
    const percentage = Math.round((completed / this.totalSteps) * 100);
    
    const currentStepInfo = this.stepDetails.get(this.currentStep);
    const eta = this.estimateRemainingTime();
    
    return {
      taskId: this.taskId,
      percentage,
      currentStep: this.currentStep,
      totalSteps: this.totalSteps,
      currentStepName: currentStepInfo?.name || '',
      estimatedTimeRemaining: eta,
      status: this.getStatus()
    };
  }
  
  estimateRemainingTime() {
    const completed = Array.from(this.stepDetails.values())
      .filter(s => s.status === 'completed');
    
    if (completed.length === 0) return null;
    
    const avgTimePerStep = completed.reduce((sum, s) => 
      sum + (s.endTime - s.startTime), 0) / completed.length;
    
    const remainingSteps = this.totalSteps - this.currentStep;
    
    return Math.round(avgTimePerStep * remainingSteps / 1000); // 秒
  }
  
  getStatus() {
    if (this.currentStep >= this.totalSteps) {
      return 'completed';
    }
    return 'in_progress';
  }
}

// 使用示例：处理复杂任务
async function processComplexTask(userId, request) {
  const tracker = new TaskProgressTracker(`task_${Date.now()}`, 5);
  
  // Step 1: 解析请求
  tracker.startStep('parsing', '正在理解您的需求');
  await sleep(1000);
  tracker.completeStep();
  
  // Step 2: 收集信息
  tracker.startStep('collecting', '正在收集必要信息');
  await sleep(2000);
  tracker.completeStep();
  
  // Step 3: 执行操作
  tracker.startStep('executing', '正在执行操作');
  await sleep(3000);
  tracker.completeStep();
  
  // Step 4: 验证结果
  tracker.startStep('validating', '正在验证结果');
  await sleep(1000);
  tracker.completeStep();
  
  // Step 5: 生成报告
  tracker.startStep('reporting', '正在生成报告');
  await sleep(1000);
  tracker.completeStep({ reportUrl: '/reports/xxx.pdf' });
  
  return tracker.getProgressUpdate();
}

// 用户看到的进度：
// "正在理解您的需求... (20%)"
// "正在收集必要信息... (40%)"
// "正在执行操作... (60%)"
// "正在验证结果... (80%)"
// "正在生成报告... (100%)"
```

## c8-6 主动通知策略

### 智能提醒系统

```javascript
class SmartNotificationSystem {
  constructor() {
    this.userPreferences = new Map();
    this.notificationQueue = [];
  }
  
  async scheduleNotification(userId, notification) {
    const prefs = this.getUserPreferences(userId);
    
    // 评估优先级
    const priority = this.evaluatePriority(notification);
    
    // 选择最佳通知时机
    const optimalTime = this.findOptimalTime(userId, notification, priority);
    
    // 选择通知渠道
    const channel = this.selectChannel(userId, priority, notification.type);
    
    const scheduledNotification = {
      id: generateId(),
      userId,
      content: notification.content,
      priority,
      scheduledTime: optimalTime,
      channel,
      status: 'scheduled'
    };
    
    this.notificationQueue.push(scheduledNotification);
    
    return scheduledNotification;
  }
  
  evaluatePriority(notification) {
    // 紧急且重要
    if (notification.urgency === 'high' && notification.importance === 'high') {
      return 'critical'; // 立即通知
    }
    
    // 紧急但不重要
    if (notification.urgency === 'high' && notification.importance === 'low') {
      return 'high'; // 尽快通知
    }
    
    // 不紧急但重要
    if (notification.urgency === 'low' && notification.importance === 'high') {
      return 'medium'; // 合适时间通知
    }
    
    // 普通
    return 'low';
  }
  
  findOptimalTime(userId, notification, priority) {
    const now = new Date();
    
    if (priority === 'critical') {
      return now; // 立即
    }
    
    // 获取用户的活跃时间段
    const activeHours = this.getUserActiveHours(userId);
    
    // 如果是工作时间外，推迟到下一个工作时间
    if (!this.isWithinActiveHours(now, activeHours)) {
      return this.getNextActiveTime(now, activeHours);
    }
    
    // 避免在会议时间打扰
    if (this.hasMeeting(userId, now)) {
      return this.getNextFreeTime(userId, now);
    }
    
    return now;
  }
  
  selectChannel(userId, priority, type) {
    const prefs = this.getUserPreferences(userId);
    
    switch (priority) {
      case 'critical':
        return 'push+sms'; // 推送 + 短信
      case 'high':
        return 'push'; // 推送
      case 'medium':
        return prefs.mediumPriorityChannel || 'app_notification';
      case 'low':
        return prefs.lowPriorityChannel || 'email';
    }
  }
  
  getUserActiveHours(userId) {
    // 基于历史行为学习
    return {
      weekday: { start: 9, end: 18 },
      weekend: { start: 10, end: 22 }
    };
  }
  
  isWithinActiveHours(time, activeHours) {
    const day = time.getDay();
    const hours = activeHours[day === 0 || day === 6 ? 'weekend' : 'weekday'];
    return time.getHours() >= hours.start && time.getHours() <= hours.end;
  }
}

// 使用示例
const notificationSystem = new SmartNotificationSystem();

// 紧急会议提醒
await notificationSystem.scheduleNotification('user_123', {
  type: 'meeting_reminder',
  content: '10 分钟后开始项目评审会议',
  urgency: 'high',
  importance: 'high'
});
// → 立即通过 push+sms 通知

// 普通新闻推送
await notificationSystem.scheduleNotification('user_123', {
  type: 'news_digest',
  content: '今日科技新闻摘要',
  urgency: 'low',
  importance: 'low'
});
// → 在用户活跃时间通过 email 通知
```

---

**c8 模块核心内容完成！**
