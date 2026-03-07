# c2-3 上下文窗口管理

## 1. 概述

上下文窗口（Context Window）是 LLM 单次处理能接受的最大 Token 数量。有效管理上下文对于控制成本、提升性能和保持对话连贯性至关重要。

## 2. 核心概念

### 2.1 Token 计算基础

```javascript
// Token 估算规则
const tokenEstimation = {
  // 英文：1 Token ≈ 4 个字符 或 0.75 个单词
  english: {
    charactersPerToken: 4,
    wordsPerToken: 0.75
  },
  
  // 中文：1 Token ≈ 1.5 个汉字
  chinese: {
    charactersPerToken: 1.5
  }
};

// 快速估算函数
function estimateTokens(text, language = 'chinese') {
  if (language === 'chinese') {
    return Math.ceil(text.length / 1.5);
  } else {
    return Math.ceil(text.length / 4);
  }
}

// 示例
const text = "人工智能正在改变世界";
console.log(`Token 数：${estimateTokens(text)}`); // 约 14 Tokens
```

### 2.2 主流模型上下文限制

| 模型 | 上下文窗口 | 适用场景 |
|------|-----------|---------|
| GPT-3.5-Turbo | 4K / 16K | 日常对话、简单任务 |
| GPT-4-Turbo | 128K | 长文档分析、复杂推理 |
| Claude-3-Opus | 200K | 书籍分析、法律文档 |
| Claude-3-Haiku | 200K | 快速处理长文本 |
| Qwen-Max | 32K | 中文长文本处理 |
| Gemini-Pro | 32K | 多模态任务 |

## 3. 上下文组成

### 3.1 完整的请求结构

```javascript
const fullRequest = {
  // System Prompt（系统指令）
  system: `你是一位专业的编程助手，名叫 CodeHelper。
你的职责是帮助用户解决编程问题。
要求：代码简洁、注释清晰、考虑边界情况。`,
  
  // Conversation History（对话历史）
  messages: [
    { role: 'user', content: '如何排序数组？' },
    { role: 'assistant', content: '可以使用 Array.sort()...' },
    { role: 'user', content: '那降序呢？' }
  ],
  
  // Current Input（当前输入）
  currentInput: '请用 TypeScript 实现一个快速排序'
};

// Token 分布估算
// System: ~30 Tokens
// History: ~50 Tokens  
// Current: ~15 Tokens
// 总计：~95 Tokens
```

### 3.2 Token 消耗分析

```javascript
class TokenAnalyzer {
  constructor() {
    this.breakdown = {
      systemPrompt: 0,
      conversationHistory: 0,
      currentInput: 0,
      reservedForOutput: 0
    };
  }
  
  analyze(messages, maxTokens = 4096) {
    // System Prompt
    const systemMsg = messages.find(m => m.role === 'system');
    this.breakdown.systemPrompt = this.countTokens(systemMsg?.content || '');
    
    // History (excluding system and last user message)
    const history = messages.filter(m => 
      m.role !== 'system' && m.role !== 'user' || 
      m !== messages[messages.length - 1]
    );
    this.breakdown.conversationHistory = history.reduce(
      (sum, msg) => sum + this.countTokens(msg.content), 0
    );
    
    // Current Input
    const lastUserMsg = messages.filter(m => m.role === 'user').pop();
    this.breakdown.currentInput = this.countTokens(lastUserMsg?.content || '');
    
    // Reserved for Response
    this.breakdown.reservedForOutput = maxTokens * 0.2; // 预留 20%
    
    return this.breakdown;
  }
  
  countTokens(text) {
    // 简化的 Token 计数
    return Math.ceil(text.length / 1.5);
  }
  
  report() {
    const total = Object.values(this.breakdown).reduce((a, b) => a + b, 0);
    console.log('Token 消耗分析:');
    console.table(this.breakdown);
    console.log(`总计：${total} Tokens`);
  }
}
```

## 4. 上下文压缩策略

### 4.1 滑动窗口法

只保留最近的 N 轮对话：

```javascript
class SlidingWindowMemory {
  constructor(maxRounds = 10) {
    this.maxRounds = maxRounds;
    this.messages = [];
  }
  
  addMessage(role, content) {
    this.messages.push({ role, content });
    
    // 保持最新的 N 轮对话（1 轮 = user + assistant）
    while (this.messages.length > this.maxRounds * 2) {
      this.messages.shift(); // 移除最早的
    }
  }
  
  getMessages() {
    return this.messages;
  }
  
  // 获取摘要版本（移除最早的消息）
  getCompressedMessages() {
    if (this.messages.length <= this.maxRounds * 2) {
      return this.messages;
    }
    
    // 保留系统消息和最后 N 轮
    const systemMsg = this.messages.filter(m => m.role === 'system');
    const recentMessages = this.messages.slice(-this.maxRounds * 2);
    
    return [...systemMsg, ...recentMessages];
  }
}

// 使用示例
const memory = new SlidingWindowMemory(5); // 保留 5 轮对话

memory.addMessage('user', '你好');
memory.addMessage('assistant', '你好！有什么可以帮您？');
// ... 继续对话

const context = memory.getCompressedMessages();
```

### 4.2 关键信息提取

识别并保留重要信息，丢弃冗余内容：

```javascript
async function extractKeyInformation(conversation) {
  const extractionPrompt = `
  请从以下对话中提取关键信息：
  
  ${conversation}
  
  提取以下内容：
  1. 用户的核心需求
  2. 已解决的问题
  3. 待解决的问题
  4. 重要的参数和约束条件
  
  用简洁的语言总结，控制在 200 字以内。
  `;
  
  const summary = await llm.generate(extractionPrompt);
  return summary;
}

// 在对话中的应用
class SmartMemoryManager {
  constructor(tokenLimit = 3000) {
    this.tokenLimit = tokenLimit;
    this.fullHistory = [];
    this.summary = '';
  }
  
  async addInteraction(userMsg, assistantMsg) {
    this.fullHistory.push({
      role: 'user',
      content: userMsg,
      timestamp: Date.now()
    });
    
    this.fullHistory.push({
      role: 'assistant',
      content: assistantMsg,
      timestamp: Date.now()
    });
    
    // 检查是否超出限制
    const currentTokens = this.estimateTotalTokens();
    if (currentTokens > this.tokenLimit) {
      await this.compressHistory();
    }
  }
  
  async compressHistory() {
    // 将早期对话压缩成摘要
    const earlyConversation = this.fullHistory
      .slice(0, -10) // 保留最后 5 轮
      .map(m => `${m.role}: ${m.content}`)
      .join('\n');
    
    this.summary = await extractKeyInformation(earlyConversation);
    
    // 只保留最后几轮完整对话
    this.compressedHistory = this.fullHistory.slice(-10);
  }
  
  getContext() {
    const summaryMsg = this.summary ? [
      { role: 'system', content: `【对话摘要】${this.summary}` }
    ] : [];
    
    return [...summaryMsg, ...this.compressedHistory];
  }
}
```

### 4.3 分层摘要法

对不同部分采用不同粒度的摘要：

```javascript
class HierarchicalSummary {
  constructor() {
    this.layers = {
      detailed: [],    // 最近 3 轮：完整保留
      summarized: [],  // 中间 10 轮：要点摘要
      meta: ''         // 更早的：元摘要
    };
  }
  
  async update(userMsg, assistantMsg) {
    // 添加到详细层
    this.layers.detailed.push({ user: userMsg, assistant: assistantMsg });
    
    // 当详细层超过 3 轮，移动最早的到摘要层
    if (this.layers.detailed.length > 3) {
      const oldest = this.layers.detailed.shift();
      const summary = await this.summarizeRound(oldest);
      this.layers.summarized.push(summary);
    }
    
    // 当摘要层超过 10 条，生成元摘要
    if (this.layers.summarized.length > 10) {
      const metaSummary = await this.generateMetaSummary(
        this.layers.summarized
      );
      this.layers.meta = metaSummary;
      this.layers.summarized = [];
    }
  }
  
  async summarizeRound(round) {
    const prompt = `
    总结这轮对话的关键点：
    用户：${round.user}
    助手：${round.assistant}
    
    用一句话总结核心内容。
    `;
    return await llm.generate(prompt);
  }
  
  async generateMetaSummary(summaries) {
    const prompt = `
    以下是多轮对话的摘要：
    ${summaries.join('\n')}
    
    请生成一个总体摘要，概括整个对话的主题和进展。
    限制在 150 字以内。
    `;
    return await llm.generate(prompt);
  }
  
  getFullContext() {
    return `
【整体摘要】${this.layers.meta}

【近期要点】
${this.layers.summarized.join('\n')}

【详细对话】
${this.layers.detailed.map((r, i) => 
  `第${i+1}轮:\n用户：${r.user}\n助手：${r.assistant}`
).join('\n\n')}
`;
  }
}
```

## 5. 实战优化技巧

### 5.1 精简 System Prompt

```javascript
// ❌ 冗长的 System Prompt
const verboseSystem = `
你是一位非常专业、友好、乐于助人的 AI助手。
你的任务是尽最大努力回答用户的各种问题，提供详细的解答。
无论什么问题，你都应该认真对待，给出准确的答案。
如果有不懂的地方，要诚实地告诉用户你不清楚。
请务必遵守法律法规，不提供任何违法的建议。
...（还有 200 字）
`;

// ✅ 精简版本
const conciseSystem = `
角色：专业 AI助手
原则：准确、简洁、友好
边界：不回答违法/有害问题
未知：诚实承认不清楚
`;

// Token 对比：
// 冗长版：~150 Tokens
// 精简版：~30 Tokens
// 节省：80%
```

### 5.2 消息格式优化

```javascript
// ❌ 低效格式
const inefficientFormat = {
  messages: [
    { role: 'user', content: '我想问一下关于 Python 的问题' },
    { role: 'assistant', content: '好的，请问您想了解 Python 的哪个方面呢？' },
    { role: 'user', content: '我想知道列表怎么排序' }
  ]
};

// ✅ 高效格式
const efficientFormat = {
  messages: [
    { role: 'user', content: 'Python 列表如何排序？' }
  ]
};

// 或者保留必要上下文
const contextualFormat = {
  messages: [
    { role: 'system', content: 'Python 编程助手' },
    { role: 'user', content: '列表排序：sort() vs sorted()' }
  ]
};
```

### 5.3 批量处理

```javascript
// ❌ 逐个提问（多次调用）
await ask('什么是机器学习？');
await ask('有哪些应用场景？');
await ask('需要什么数学基础？');

// ✅ 批量提问（单次调用）
await ask(`
请回答以下三个问题：
1. 什么是机器学习？
2. 有哪些典型应用场景？
3. 学习机器学习需要什么数学基础？

请按顺序逐一解答，每个问题 200 字左右。
`);

// Token 节省：减少重复的系统开销和客套话
```

### 5.4 延迟加载上下文

```javascript
class LazyContextLoader {
  constructor(fullDocuments) {
    this.documents = fullDocuments;
    this.loadedSections = new Set();
  }
  
  async getContext(query) {
    // 先只用基础摘要
    let context = this.documents.summary;
    
    // 根据查询动态加载相关章节
    const relevantSections = await this.findRelevantSections(query);
    
    for (const section of relevantSections) {
      if (!this.loadedSections.has(section.id)) {
        context += `\n\n【${section.title}】\n${section.content}`;
        this.loadedSections.add(section.id);
      }
    }
    
    return context;
  }
  
  async findRelevantSections(query) {
    // 使用向量搜索或关键词匹配
    return this.documents.sections.filter(s => 
      s.keywords.some(k => query.includes(k))
    );
  }
}
```

## 6. 长文档处理策略

### 6.1 分块处理（Chunking）

```javascript
async function processLongDocument(document, chunkSize = 2000) {
  // 按段落分割
  const paragraphs = document.split(/\n\s*\n/);
  const chunks = [];
  let currentChunk = '';
  
  for (const para of paragraphs) {
    if (currentChunk.length + para.length > chunkSize) {
      chunks.push(currentChunk);
      currentChunk = para;
    } else {
      currentChunk += '\n\n' + para;
    }
  }
  chunks.push(currentChunk);
  
  // 逐块处理
  const results = [];
  for (let i = 0; i < chunks.length; i++) {
    const prompt = `
    文档片段 ${i+1}/${chunks.length}:
    ${chunks[i]}
    
    任务：提取关键信息并总结（100 字以内）
    `;
    
    const summary = await llm.generate(prompt);
    results.push(summary);
  }
  
  // 合并所有摘要
  const finalPrompt = `
  整合以下文档摘要：
  ${results.join('\n\n')}
  
  生成完整的文档总结（300 字以内）。
  `;
  
  return await llm.generate(finalPrompt);
}
```

### 6.2 Map-Reduce 模式

```javascript
async function mapReduceAnalysis(documents, task) {
  // Map 阶段：并行处理每个文档
  const mapPromises = documents.map(async (doc, index) => {
    const prompt = `
    文档 ${index + 1}/${documents.length}:
    ${doc.content}
    
    任务：${task}
    输出：结构化结果（JSON 格式）
    `;
    
    return await llm.generate(prompt);
  });
  
  const mapResults = await Promise.all(mapPromises);
  
  // Reduce 阶段：整合结果
  const reducePrompt = `
  整合以下分析结果：
  ${mapResults.map((r, i) => `文档${i+1}: ${r}`).join('\n')}
  
  任务：生成综合分析报告
  要求：
  1. 找出共同点
  2. 识别差异
  3. 给出总体结论
  `;
  
  return await llm.generate(reducePrompt);
}

// 使用示例：分析多篇论文
const papers = [paper1, paper2, paper3, paper4];
const analysis = await mapReduceAnalysis(
  papers,
  '提取研究方法、数据来源、主要结论'
);
```

### 6.3 迭代精炼

```javascript
async function iterativeRefinement(longText, iterations = 3) {
  let result = longText;
  
  for (let i = 0; i < iterations; i++) {
    const prompt = `
    当前文本：
    ${result}
    
    第${i+1}轮优化任务：
    ${i === 0 ? '提取核心观点' : 
      i === 1 ? '补充关键细节' : 
      '精炼语言，去除冗余'}
    
    输出：优化后的文本（${500 * (i+1)}字以内）
    `;
    
    result = await llm.generate(prompt);
  }
  
  return result;
}
```

## 7. 成本控制

### 7.1 Token 预算管理

```javascript
class TokenBudgetManager {
  constructor(dailyBudget = 100000) {
    this.dailyBudget = dailyBudget;
    this.usedToday = 0;
    this.requestQueue = [];
  }
  
  async executeWithBudget(prompt, priority = 'normal') {
    const estimatedTokens = this.estimateTokens(prompt);
    
    if (this.usedToday + estimatedTokens > this.dailyBudget) {
      if (priority === 'high') {
        // 高优先级：压缩后执行
        const compressed = await this.compressPrompt(prompt);
        return await this.execute(compressed);
      } else {
        // 普通优先级：加入队列
        return new Promise(resolve => {
          this.requestQueue.push({ prompt, resolve });
        });
      }
    }
    
    return await this.execute(prompt);
  }
  
  async compressPrompt(originalPrompt) {
    const compressionPrompt = `
    在不改变原意的情况下，精简以下文字到原来的 50%：
    
    ${originalPrompt}
    `;
    
    return await llm.generate(compressionPrompt);
  }
  
  async execute(prompt) {
    const response = await llm.generate(prompt);
    const actualTokens = this.countTokens(response);
    
    this.usedToday += actualTokens;
    this.logUsage(actualTokens);
    
    // 处理队列
    if (this.requestQueue.length > 0 && 
        this.usedToday < this.dailyBudget * 0.9) {
      const next = this.requestQueue.shift();
      next.resolve(await this.execute(next.prompt));
    }
    
    return response;
  }
}
```

### 7.2 智能模型选择

```javascript
async function selectModel(task, inputLength) {
  // 简单任务 + 短文本 → 小模型
  if (inputLength < 500 && ['翻译', '总结', '分类'].includes(task)) {
    return 'gpt-3.5-turbo'; // 便宜、快速
  }
  
  // 复杂推理 + 长文本 → 大模型
  if (inputLength > 2000 || ['分析', '创作', '推理'].includes(task)) {
    return 'gpt-4-turbo'; // 强大、准确
  }
  
  // 默认选择
  return 'gpt-4';
}

// 使用示例
const model = await selectModel('合同审查', 5000);
const response = await llm.generate(prompt, { model });
```

## 8. 性能监控

### 8.1 关键指标追踪

```javascript
class PerformanceMonitor {
  constructor() {
    this.metrics = {
      totalRequests: 0,
      totalTokens: 0,
      averageLatency: 0,
      cacheHitRate: 0
    };
  }
  
  track(request, response, latency) {
    this.metrics.totalRequests++;
    this.metrics.totalTokens += response.usage.total_tokens;
    this.metrics.averageLatency = 
      (this.metrics.averageLatency * (this.metrics.totalRequests - 1) + latency) / 
      this.metrics.totalRequests;
    
    this.saveToDatabase({
      timestamp: Date.now(),
      tokens: response.usage.total_tokens,
      latency: latency,
      cost: this.calculateCost(response.usage.total_tokens)
    });
  }
  
  generateReport() {
    return `
今日统计:
- 总请求数：${this.metrics.totalRequests}
- 总 Token 消耗：${this.metrics.totalTokens}
- 平均响应时间：${this.metrics.averageLatency.toFixed(2)}ms
- 预估成本：$${this.estimateDailyCost()}
    `;
  }
}
```

---

**下一节：** [c2-4 Token 优化策略](./c2-4-token-optimization.md)  
**上一节：** [c2-2 Function Calling / Tool Calling](./c2-2-function-calling.md)
