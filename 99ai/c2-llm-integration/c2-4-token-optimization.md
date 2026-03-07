# c2-4 Token 优化策略

## 1. 概述

Token 优化是在保证输出质量的前提下，通过技术手段减少 Token 消耗，从而降低成本、提升响应速度。本章介绍实用的优化技巧和最佳实践。

## 2. Token 成本分析

### 2.1 主流模型定价（每 1K Tokens）

| 模型 | Input 价格 | Output 价格 | 性价比场景 |
|------|----------|-----------|-----------|
| GPT-3.5-Turbo | $0.0005 | $0.0015 | 日常对话、简单任务 |
| GPT-4-Turbo | $0.01 | $0.03 | 复杂推理、专业任务 |
| Claude-3-Haiku | $0.00025 | $0.00125 | 快速处理、大批量 |
| Claude-3-Opus | $0.015 | $0.075 | 高难度、高质量要求 |
| Qwen-Max | ¥0.02 | ¥0.06 | 中文场景 |
| ERNIE-Bot | ¥0.012 | ¥0.024 | 中文企业应用 |

### 2.2 实际成本计算

```javascript
class CostCalculator {
  constructor() {
    this.pricing = {
      'gpt-3.5-turbo': { input: 0.0005, output: 0.0015 },
      'gpt-4-turbo': { input: 0.01, output: 0.03 },
      'claude-3-haiku': { input: 0.00025, output: 0.00125 }
    };
  }
  
  calculate(model, inputTokens, outputTokens) {
    const price = this.pricing[model];
    if (!price) throw new Error(`Unknown model: ${model}`);
    
    const inputCost = (inputTokens / 1000) * price.input;
    const outputCost = (outputTokens / 1000) * price.output;
    
    return {
      input: inputCost,
      output: outputCost,
      total: inputCost + outputCost
    };
  }
  
  // 月度成本预估
  monthlyEstimate(dailyRequests, avgInputTokens, avgOutputTokens, model) {
    const dailyCost = this.calculate(model, avgInputTokens, avgOutputTokens).total;
    return dailyCost * dailyRequests * 30;
  }
}

// 使用示例
const calculator = new CostCalculator();

// 场景：每天 1000 次请求，平均输入 500 tokens，输出 200 tokens
const monthlyCost = calculator.monthlyEstimate(
  1000,     // 每日请求数
  500,      // 平均输入 tokens
  200,      // 平均输出 tokens
  'gpt-3.5-turbo'
);

console.log(`GPT-3.5 月度成本：$${monthlyCost.toFixed(2)}`);
// 输出：$37.50

const gpt4Cost = calculator.monthlyEstimate(1000, 500, 200, 'gpt-4-turbo');
console.log(`GPT-4 月度成本：$${gpt4Cost.toFixed(2)}`);
// 输出：$220.00
```

## 3. 输入端优化

### 3.1 Prompt 精简技巧

**技巧 1: 去除冗余词汇**

```javascript
// ❌ 冗长版本 (85 tokens)
const verbose = `
我希望你能够帮助我完成一个任务，这个任务是需要你帮我写一封邮件。
这封邮件是发给我的上司的，主要是想告诉他项目进展的情况。
请你用正式的语气来写，不要太随意，要表现出专业性。
`;

// ✅ 精简版本 (35 tokens)
const concise = `
任务：给上司写项目进展汇报邮件
语气：正式、专业
内容：当前进度、遇到的问题、下一步计划
`;

// 节省：59% tokens
```

**技巧 2: 使用缩写和符号**

```javascript
// 完整表达 vs 符号表达
const mappings = {
  '例如': 'eg.',
  '也就是说': 'i.e.',
  '因此': '∴',
  '因为': '∵',
  '第一、第二、第三': '1️⃣2️⃣3️⃣',
  '重要': '❗',
  '注意': '⚠️'
};

function compressText(text) {
  let compressed = text;
  for (const [full, abbr] of Object.entries(mappings)) {
    compressed = compressed.replace(new RegExp(full, 'g'), abbr);
  }
  return compressed;
}
```

**技巧 3: 结构化表达**

```javascript
// ❌ 散乱描述
const unstructured = `
首先你需要了解这个项目的背景，我们是一个电商团队，主要做海外市场。
然后你要知道我们的目标用户是欧美地区的年轻人，大概 18 到 35 岁之间。
接下来你要分析一下竞争对手的情况，看看他们都在做什么。
最后给出你的建议，我们应该如何制定营销策略。
`;

// ✅ 结构化 (节省 40%+ tokens)
const structured = `
【背景】电商团队，海外市场
【用户】欧美，18-35 岁
【任务】
1. 竞对分析
2. 营销策略建议

【输出】PPT 大纲格式
`;
```

### 3.2 上下文选择策略

```javascript
class SmartContextSelector {
  constructor(maxTokens = 3000) {
    this.maxTokens = maxTokens;
    this.conversationHistory = [];
  }
  
  selectContext(currentQuery) {
    // 1. 计算可用空间
    const queryTokens = this.estimateTokens(currentQuery);
    const reservedForResponse = 500;
    const availableSpace = this.maxTokens - queryTokens - reservedForResponse;
    
    // 2. 智能选择历史消息
    const selectedHistory = [];
    let usedTokens = 0;
    
    // 优先保留最近的对话
    for (let i = this.conversationHistory.length - 1; i >= 0; i--) {
      const msg = this.conversationHistory[i];
      const msgTokens = this.estimateTokens(msg.content);
      
      if (usedTokens + msgTokens <= availableSpace) {
        selectedHistory.unshift(msg);
        usedTokens += msgTokens;
      } else {
        break;
      }
    }
    
    // 3. 如果还有空间，添加相关的早期对话
    if (usedTokens < availableSpace * 0.8) {
      const relevantEarlyMessages = this.findRelevantMessages(
        currentQuery,
        this.conversationHistory.slice(0, -selectedHistory.length)
      );
      
      for (const msg of relevantEarlyMessages) {
        const msgTokens = this.estimateTokens(msg.content);
        if (usedTokens + msgTokens <= availableSpace) {
          selectedHistory.unshift(msg);
          usedTokens += msgTokens;
        }
      }
    }
    
    return selectedHistory;
  }
  
  findRelevantMessages(query, candidates) {
    // 简单的关键词匹配
    const queryKeywords = query.toLowerCase().split(/\s+/);
    
    return candidates.filter(msg => {
      const content = msg.content.toLowerCase();
      return queryKeywords.some(keyword => 
        content.includes(keyword) && keyword.length > 3
      );
    }).slice(0, 3); // 最多添加 3 条相关消息
  }
}
```

### 3.3 延迟加载技术

```javascript
class LazyLoadingContext {
  constructor(fullDocuments) {
    this.documents = fullDocuments;
    this.loadedSections = new Set();
    this.baseContext = fullDocuments.summary; // 只加载摘要
  }
  
  async getContext(query) {
    let context = this.baseContext;
    
    // 动态加载相关章节
    const relevantSections = await this.findRelevantSections(query);
    
    for (const section of relevantSections) {
      if (!this.loadedSections.has(section.id)) {
        context += `\n\n【${section.title}】\n${section.content.substring(0, 500)}...`;
        this.loadedSections.add(section.id);
      }
    }
    
    return context;
  }
  
  async findRelevantSections(query) {
    // 使用向量相似度或 BM25 算法
    return this.documents.sections
      .map(section => ({
        ...section,
        score: this.calculateRelevance(query, section)
      }))
      .filter(s => s.score > 0.6)
      .sort((a, b) => b.score - a.score)
      .slice(0, 5); // 最多加载 5 个相关章节
  }
  
  calculateRelevance(query, section) {
    // 简化的相关性计算
    const queryWords = query.toLowerCase().split(/\s+/);
    const content = section.content.toLowerCase();
    
    const matchCount = queryWords.filter(w => 
      w.length > 3 && content.includes(w)
    ).length;
    
    return matchCount / queryWords.length;
  }
}
```

## 4. 输出端优化

### 4.1 限制输出长度

```javascript
// ❌ 不限制，可能生成冗长回答
const openPrompt = `
请介绍一下人工智能。
`;

// ✅ 明确限制
const constrainedPrompt = `
请用 100 字以内介绍人工智能的核心概念。
聚焦：定义、主要应用领域、未来趋势
`;

// Token 对比：
// 开放版：可能生成 500+ tokens
// 限制版：严格控制在 100 tokens 左右
// 节省：80%+
```

### 4.2 格式化输出

```javascript
// 指定 JSON 格式（避免多余解释）
const jsonPrompt = `
提取以下文本的关键信息，直接输出 JSON：

${text}

格式：
{
  "title": string,
  "summary": string,
  "keywords": string[]
}

注意：只输出 JSON，不要其他文字。
`;

// 指定表格格式（紧凑呈现）
const tablePrompt = `
对比这三款产品，用表格形式：

产品 A: ${specsA}
产品 B: ${specsB}
产品 C: ${specsC}

表格列：特性 | 产品 A | 产品 B | 产品 C
只填关键参数，不解释。
`;
```

### 4.3 分步执行 vs 单次执行

```javascript
// ❌ 分多次提问（累积 tokens 多）
await ask('什么是机器学习？');           // ~300 tokens
await ask('有哪些应用场景？');          // ~250 tokens
await ask('需要什么数学基础？');         // ~280 tokens
await ask('如何开始学习？');            // ~260 tokens
总计：~1090 tokens

// ✅ 单次完整提问（节省 tokens）
await ask(`
请全面介绍机器学习，包括：
1. 定义和核心概念
2. 典型应用场景（3-5 个）
3. 所需数学基础
4. 学习路径建议

总字数：800 字以内
`);
总计：~600 tokens
节省：45%
```

## 5. 缓存策略

### 5.1 Prompt 结果缓存

```javascript
class PromptCache {
  constructor(ttlMinutes = 60) {
    this.cache = new Map();
    this.ttl = ttlMinutes * 60 * 1000;
  }
  
  generateKey(prompt, model, temperature) {
    // 创建 prompt 的哈希值作为 key
    const hash = require('crypto')
      .createHash('md5')
      .update(`${prompt}|${model}|${temperature}`)
      .digest('hex');
    return hash;
  }
  
  async getOrGenerate(prompt, model, temperature = 0.7) {
    const key = this.generateKey(prompt, model, temperature);
    const cached = this.cache.get(key);
    
    // 检查缓存是否有效
    if (cached && Date.now() - cached.timestamp < this.ttl) {
      console.log('Cache hit!');
      return cached.response;
    }
    
    // 调用 LLM
    console.log('Cache miss, generating...');
    const response = await llm.generate(prompt, { model, temperature });
    
    // 存入缓存
    this.cache.set(key, {
      response,
      timestamp: Date.now(),
      tokens: this.countTokens(response)
    });
    
    return response;
  }
  
  // 清理过期缓存
  cleanup() {
    const now = Date.now();
    for (const [key, value] of this.cache.entries()) {
      if (now - value.timestamp > this.ttl) {
        this.cache.delete(key);
      }
    }
  }
  
  // 统计缓存命中率
  getStats() {
    const total = this.cache.size;
    const valid = Array.from(this.cache.values())
      .filter(v => Date.now() - v.timestamp < this.ttl).length;
    
    return {
      totalEntries: total,
      validEntries: valid,
      estimatedSavings: `${((valid / Math.max(total, 1)) * 100).toFixed(1)}%`
    };
  }
}

// 使用示例
const cache = new PromptCache(120); // 2 小时 TTL

// 相同问题直接返回缓存
const answer1 = await cache.getOrGenerate('什么是 AI？', 'gpt-3.5-turbo');
const answer2 = await cache.getOrGenerate('什么是 AI？', 'gpt-3.5-turbo'); // Cache hit!
```

### 5.2 部分结果缓存

```javascript
class PartialResultCache {
  constructor() {
    this.componentCache = new Map();
  }
  
  async generateReport(data) {
    const components = {
      executiveSummary: await this.getOrGenerateComponent(
        'executive_summary',
        () => this.generateExecutiveSummary(data)
      ),
      
      marketAnalysis: await this.getOrGenerateComponent(
        'market_analysis',
        () => this.generateMarketAnalysis(data)
      ),
      
      recommendations: await this.getOrGenerateComponent(
        'recommendations',
        () => this.generateRecommendations(data)
      )
    };
    
    return this.assembleReport(components);
  }
  
  async getOrGenerateComponent(name, generator) {
    const cached = this.componentCache.get(name);
    
    if (cached && Date.now() - cached.timestamp < 3600000) {
      console.log(`Using cached ${name}`);
      return cached.content;
    }
    
    console.log(`Generating ${name}...`);
    const content = await generator();
    
    this.componentCache.set(name, {
      content,
      timestamp: Date.now()
    });
    
    return content;
  }
}
```

## 6. 批量优化

### 6.1 批处理请求

```javascript
async function batchProcess(items, batchSize = 10) {
  const results = [];
  
  for (let i = 0; i < items.length; i += batchSize) {
    const batch = items.slice(i, i + batchSize);
    
    // 合并成一个请求
    const batchPrompt = `
    处理以下${batch.length}个项目：
    
    ${batch.map((item, idx) => 
      `项目${idx + 1}: ${item}`
    ).join('\n\n')}
    
    要求：
    - 每个项目单独一段
    - 标注序号
    - 简洁明了
    `;
    
    const batchResult = await llm.generate(batchPrompt);
    results.push(batchResult);
  }
  
  return results;
}

// 对比：
// 逐个处理 100 项：100 次 API 调用，每次 overhead ~50 tokens
// 批处理（10 项/批）：10 次 API 调用，overhead 分摊
// 节省：~4500 tokens + 减少网络延迟
```

### 6.2 流式输出优化

```javascript
// 使用流式 API，提前终止不必要的生成
async function streamWithEarlyTermination(prompt, stopCondition) {
  const stream = await llm.generateStream(prompt);
  let accumulatedText = '';
  
  for await (const chunk of stream) {
    accumulatedText += chunk;
    
    // 检查是否可以提前终止
    if (stopCondition(accumulatedText)) {
      console.log('Early termination triggered');
      break;
    }
  }
  
  return accumulatedText;
}

// 使用场景：已经得到答案，停止后续生成
const answer = await streamWithEarlyTermination(
  '列出 10 个 Python 最佳实践',
  (text) => text.split('\n').length >= 10 // 已经有 10 行了
);
```

## 7. 模型选择策略

### 7.1 基于任务的模型路由

```javascript
class ModelRouter {
  constructor() {
    this.routes = {
      simple: ['翻译', '总结', '分类', '提取'],
      complex: ['分析', '推理', '创作', '规划'],
      code: ['编程', '调试', '优化', '测试'],
      chat: ['聊天', '问答', '建议']
    };
    
    this.modelMapping = {
      simple: 'claude-3-haiku',     // 便宜快速
      complex: 'gpt-4-turbo',        // 强大推理
      code: 'gpt-4',                 // 代码能力强
      chat: 'gpt-3.5-turbo'          // 性价比高
    };
  }
  
  selectModel(task, inputLength) {
    // 识别任务类型
    const taskType = Object.keys(this.routes).find(type =>
      this.routes[type].some(keyword => task.includes(keyword))
    ) || 'chat';
    
    // 超长文本强制使用大上下文模型
    if (inputLength > 10000) {
      return 'claude-3-200k';
    }
    
    return this.modelMapping[taskType];
  }
}

// 使用示例
const router = new ModelRouter();

const model1 = router.selectModel('翻译这段文字', 200);
// 返回：claude-3-haiku (便宜)

const model2 = router.selectModel('分析这个商业计划', 5000);
// 返回：gpt-4-turbo (强大)
```

### 7.2 降级策略

```javascript
async function generateWithFallback(primaryPrompt, options = {}) {
  const modelPriority = options.models || [
    'gpt-4-turbo',
    'gpt-4',
    'gpt-3.5-turbo',
    'claude-3-haiku'
  ];
  
  let lastError;
  
  for (const model of modelPriority) {
    try {
      console.log(`Trying ${model}...`);
      const result = await llm.generate(primaryPrompt, { 
        model,
        timeout: 30000 
      });
      
      console.log(`Success with ${model}`);
      return result;
      
    } catch (error) {
      lastError = error;
      console.warn(`${model} failed: ${error.message}`);
      continue;
    }
  }
  
  throw new Error(`All models failed. Last error: ${lastError.message}`);
}
```

## 8. 监控与分析

### 8.1 Token 使用追踪

```javascript
class TokenUsageTracker {
  constructor() {
    this.records = [];
  }
  
  record(request, response, metadata = {}) {
    const record = {
      timestamp: Date.now(),
      model: response.model,
      inputTokens: response.usage.prompt_tokens,
      outputTokens: response.usage.completion_tokens,
      totalTokens: response.usage.total_tokens,
      cost: this.calculateCost(response),
      category: metadata.category || 'uncategorized',
      latency: metadata.latency || 0
    };
    
    this.records.push(record);
    return record;
  }
  
  // 生成日报
  generateDailyReport() {
    const today = new Date().toDateString();
    const todayRecords = this.records.filter(
      r => new Date(r.timestamp).toDateString() === today
    );
    
    const summary = {
      totalRequests: todayRecords.length,
      totalTokens: todayRecords.reduce((sum, r) => sum + r.totalTokens, 0),
      totalCost: todayRecords.reduce((sum, r) => sum + r.cost, 0),
      averageLatency: todayRecords.reduce((sum, r) => sum + r.latency, 0) / todayRecords.length,
      byModel: this.groupBy(todayRecords, 'model'),
      byCategory: this.groupBy(todayRecords, 'category')
    };
    
    return summary;
  }
  
  // 识别优化机会
  identifyOptimizationOpportunities() {
    const longPrompts = this.records.filter(r => r.inputTokens > 1000);
    const expensiveCategories = this.groupBy(
      this.records.filter(r => r.cost > 0.1),
      'category'
    );
    
    return {
      longPromptsCount: longPrompts.length,
      suggestion: `发现${longPrompts.length}次长输入，建议精简 prompt`,
      expensiveCategories: Object.keys(expensiveCategories)
    };
  }
}
```

## 9. 最佳实践清单

✅ **DO - 推荐做法：**

1. **始终设置输出长度限制**
   ```
   ✅ "用 200 字以内总结"
   ❌ "请总结一下"
   ```

2. **使用缓存处理重复问题**
   ```javascript
   const answer = await cache.getOrGenerate(prompt);
   ```

3. **批量处理相似任务**
   ```
   ✅ 一次问 10 个问题
   ❌ 分 10 次各问 1 个问题
   ```

4. **选择合适的模型**
   ```
   简单任务 → Haiku/GPT-3.5
   复杂任务 → GPT-4/Claude-Opus
   ```

5. **定期分析和优化**
   - 每周审查 Token 使用报告
   - 识别并优化高成本场景
   - 建立 Token 预算制度

❌ **DON'T - 避免做法：**

1. 不要每次都从头开始对话（利用上下文）
2. 不要无限制输出（设置 max_tokens）
3. 不要忽略小模型的能力（它们很便宜）
4. 不要重复造轮子（建立 prompt 库）
5. 不要等到月底才发现超支（实时监控）

---

**下一节：** [c2-5 多模型切换方案](./c2-5-multi-model-switching.md)  
**上一节：** [c2-3 上下文窗口管理](./c2-3-context-window-management.md)
