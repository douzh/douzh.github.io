# c2-1 提示工程实践

## 1. 概述

提示工程（Prompt Engineering）是通过精心设计和优化输入提示，引导大模型生成高质量输出的关键技术。本章将介绍实用的 Prompt 设计模式、Few-shot 学习、思维链等核心方法。

## 2. Prompt 设计基本原则

### 2.1 CLEAR 原则

```
C - Clear（清晰明确）
L - Logical（逻辑连贯）
E - Explicit（具体明确）
A - Actionable（可执行）
R - Refined（精炼优化）
```

**示例对比：**

❌ **模糊的 Prompt：**
```
帮我写点关于 AI 的东西
```

✅ **清晰的 Prompt：**
```
你是一位 AI 技术专家。请写一篇 800 字的科普文章，介绍机器学习的基本概念。
目标读者是高中生，要求：
1. 用简单易懂的语言
2. 包含 3 个生活中的实际应用案例
3. 避免使用复杂的数学公式
4. 结尾给出学习建议
```

### 2.2 角色设定法

通过赋予 AI 特定角色，提升回答的专业性：

```javascript
// 模板
const prompt = `
你是一位${ROLE}，拥有${YEARS}年的${FIELD}经验。
你的任务是${TASK}。

背景信息：
${CONTEXT}

约束条件：
${CONSTRAINTS}

输出格式：
${FORMAT}
`;

// 实例
const systemPrompt = `
你是一位资深软件架构师，拥有 15 年的分布式系统设计经验。
你的任务是评审一个微服务架构方案。

背景信息：
- 项目规模：日活用户 100 万
- 技术要求：99.99% 可用性
- 团队情况：20 人开发团队

请从以下维度进行评审：
1. 架构合理性
2. 潜在风险点
3. 性能瓶颈预测
4. 成本优化建议

输出格式：Markdown 报告，包含问题描述和改进建议
`;
```

## 3. 核心设计模式

### 3.1 Zero-shot（零样本）

直接给出指令，不提供示例：

```javascript
// 适用场景：简单任务、通用知识

const zeroShotPrompt = `
请将以下中文翻译成英文：

"人工智能正在改变我们的生活方式"

翻译：
`;

// 输出："Artificial intelligence is changing our way of life."
```

### 3.2 Few-shot（少样本）

提供少量示例，帮助模型理解任务模式：

```javascript
// 情感分类任务
const fewShotPrompt = `
判断以下评论的情感倾向（正面/负面）：

评论："这部电影太棒了，演员演技在线，剧情紧凑"
情感：正面

评论："浪费时间，故事混乱，不值得看"
情感：负面

评论："画面精美，但剧情薄弱，一般般吧"
情感：中性

评论："配乐很棒，但是结局让人失望"
情感：
`;

// 输出：中性/负面（取决于模型理解）
```

**Few-shot 设计技巧：**

1. **示例数量**: 3-5 个为佳
2. **示例质量**: 覆盖典型场景和边界情况
3. **格式一致**: 保持示例结构统一
4. **标签清晰**: 明确标注输入和输出

### 3.3 CoT（Chain of Thought，思维链）

引导模型展示推理过程：

```javascript
// 标准 Prompt
const standardPrompt = `
小明有 5 个苹果，他给了小红 2 个，然后又买了 3 个。
现在小明有多少个苹果？

答案：6 个
`;

// CoT Prompt ⭐
const cotPrompt = `
小明有 5 个苹果，他给了小红 2 个，然后又买了 3 个。
现在小明有多少个苹果？

让我们一步步思考：
1. 初始状态：小明有 5 个苹果
2. 给小红 2 个：5 - 2 = 3 个
3. 又买 3 个：3 + 3 = 6 个
4. 最终结果：6 个苹果

答案：6 个
`;
```

**CoT 的关键短语：**
- "让我们一步步思考"
- "第一步...第二步..."
- "原因是..."
- "因此..."

### 3.4 ToT（Tree of Thoughts，思维树）

探索多种可能的解决路径：

```javascript
const totPrompt = `
问题：如何提升用户留存率？

请从以下三个角度分别思考：

【角度 1: 产品优化】
- 可能的方案 A: ...
- 可能的方案 B: ...
- 评估：...

【角度 2: 运营策略】
- 可能的方案 A: ...
- 可能的方案 B: ...
- 评估：...

【角度 3: 用户体验】
- 可能的方案 A: ...
- 可能的方案 B: ...
- 评估：...

【综合建议】
基于以上分析，最优的方案组合是：...
`;
```

## 4. 高级技巧

### 4.1 自我一致性（Self-Consistency）

生成多个答案并选择最优：

```javascript
// 实现代码
async function selfConsistency(prompt, n = 5) {
  const answers = [];
  
  // 生成 n 个不同的答案
  for (let i = 0; i < n; i++) {
    const response = await llm.generate(prompt, {
      temperature: 0.7 // 增加随机性
    });
    answers.push(response);
  }
  
  // 投票选择最常见答案
  const frequency = {};
  answers.forEach(answer => {
    frequency[answer] = (frequency[answer] || 0) + 1;
  });
  
  return Object.entries(frequency)
    .sort((a, b) => b[1] - a[1])[0][0];
}

// 使用示例
const mathProblem = `
计算：(123 × 456) ÷ 789 = ?
`;

const bestAnswer = await selfConsistency(mathProblem);
console.log("最一致的答案:", bestAnswer);
```

### 4.2 生成知识（Generated Knowledge）

先生成背景知识，再回答问题：

```javascript
// 两步法
async function answerWithKnowledge(question) {
  // Step 1: 生成相关知识
  const knowledgePrompt = `
  关于"${question}"这个话题，有哪些重要的背景知识和关键事实？
  请列出 5-10 个要点。
  `;
  
  const knowledge = await llm.generate(knowledgePrompt);
  
  // Step 2: 基于知识回答问题
  const answerPrompt = `
  背景知识：
  ${knowledge}
  
  基于以上知识，请回答这个问题：${question}
  `;
  
  return await llm.generate(answerPrompt);
}

// 使用示例
const question = "为什么海水是咸的？";
const answer = await answerWithKnowledge(question);
```

### 4.3 约束生成

通过明确的约束条件控制输出：

```javascript
const constrainedPrompt = `
请为一款智能手表撰写产品描述。

约束条件：
- 字数：100-150 字
- 语气：专业但友好
- 必须包含：健康监测、长续航、防水功能
- 禁止使用：夸张词汇（如"革命性"、"颠覆性"）
- 格式：分 3 段，每段一个小标题

目标受众：商务人士
核心卖点：高效、可靠、时尚
`;
```

## 5. 实战应用场景

### 5.1 代码生成

```javascript
const codeGenPrompt = `
你是一位资深的 Python 开发工程师。

任务：实现一个函数，用于验证邮箱地址格式。

要求：
1. 使用正则表达式
2. 考虑常见邮箱格式（Gmail, Outlook, QQ, 163 等）
3. 包含完整的错误处理
4. 添加类型注解
5. 编写单元测试示例

请先解释正则表达式的含义，然后给出完整代码。
`;

// 期望输出包含：
// - 正则表达式解析
// - 完整函数实现
// - 使用示例
// - 测试用例
```

### 5.2 文档撰写

```javascript
const docPrompt = `
你是一位技术文档工程师。

任务：为一个 RESTful API 编写接口文档。

API 信息：
- 端点：GET /api/users/:id
- 功能：获取指定用户的详细信息
- 认证：Bearer Token
- 响应格式：JSON

请包含以下内容：
1. 接口描述
2. 请求参数说明（表格形式）
3. 响应示例（成功和失败）
4. 错误码列表
5. 使用示例（curl 命令）

格式要求：Markdown
`;
```

### 5.3 数据分析

```javascript
const analysisPrompt = `
你是一位数据分析师。

销售数据：
月份 | 销售额 (万元) | 订单数 | 客单价 (元)
-----|------------|--------|-----------
1 月  | 120        | 1500   | 800
2 月  | 95         | 1200   | 792
3 月  | 150        | 1800   | 833
4 月  | 135        | 1650   | 818

任务：
1. 计算季度总销售额和增长率
2. 识别销售趋势和异常点
3. 分析可能的原因
4. 给出下季度的预测和建议

请用表格和文字结合的方式呈现分析结果。
`;
```

### 5.4 创意写作

```javascript
const creativePrompt = `
你是一位科幻作家。

创作要求：
- 主题：AI 与人类的共生关系
- 风格：赛博朋克
- 长度：800-1000 字
- 元素：神经网络、记忆移植、意识上传
- 情感基调：希望中带着忧伤

故事结构：
1. 开篇：设置场景（2087 年的上海）
2. 发展：主角遇到困境
3. 高潮：做出关键选择
4. 结尾：开放式结局

请注重环境描写和人物内心刻画。
`;
```

## 6. 常见问题与解决方案

### 6.1 幻觉问题

**现象：** 模型编造事实

**解决方案：**
```javascript
const antiHallucinationPrompt = `
请回答这个问题：量子纠缠是如何实现的？

重要说明：
- 只基于已知的科学事实
- 如果不确定，请明确说明"我不确定"
- 不要编造研究数据或引用不存在的论文
- 可以提及这是理论物理领域的前沿问题
`;
```

### 6.2 偏离主题

**现象：** 回答跑题或冗长

**解决方案：**
```javascript
const focusedPrompt = `
请简洁回答：什么是区块链？

要求：
- 限制在 200 字以内
- 聚焦核心概念（去中心化、分布式账本）
- 不要涉及加密货币投资相关内容
- 用一个生活化类比帮助理解
`;
```

### 6.3 格式不一致

**现象：** 输出格式不符合预期

**解决方案：**
```javascript
const structuredPrompt = `
请分析这家公司的财务状况。

【输出格式要求】
严格按照以下 JSON 格式输出：
{
  "revenue": {
    "value": number,
    "growth": string
  },
  "profit": {
    "margin": string,
    "trend": "increasing" | "stable" | "decreasing"
  },
  "risks": string[]
}

【财务数据】
${financialData}

注意：只输出 JSON，不要有其他文字。
`;
```

## 7. 评估与优化

### 7.1 Prompt 迭代流程

```
初始版本 → 测试 → 分析问题 → 优化 → 再测试 → 最佳版本
```

**迭代示例：**

```javascript
// Version 1
const v1 = "写一首关于春天的诗";

// Version 2 (添加角色)
const v2 = "你是一位诗人，写一首关于春天的诗";

// Version 3 (添加约束)
const v3 = `
你是一位现代诗人。
写一首关于春天的短诗，要求：
- 4 段，每段 4 行
- 包含意象：花朵、雨水、新芽
- 表达对生命力的赞美
`;

// Version 4 (进一步优化)
const v4 = `
你是一位擅长自然描写的现代诗人，曾获得诗歌大赛金奖。

创作背景：早春时节，雨后初晴

任务：创作一首抒情短诗

具体要求：
1. 结构：4 段，每段 4 行
2. 必选意象：含苞的花朵、清新的雨水、破土的新芽
3. 修辞手法：至少使用 2 个比喻、1 个拟人
4. 情感基调：充满希望和活力
5. 押韵方式：AABB 或 ABAB

请先简要说明创作灵感，然后呈现诗歌正文。
`;
```

### 7.2 A/B 测试

```javascript
async function abTestPrompt() {
  const promptA = `
  推荐一部科幻电影，并说明理由。
  `;
  
  const promptB = `
  你是一位资深影评人。
  请推荐一部你认为被低估的科幻电影（非热门作品）。
  
  要求：
  - 说明推荐理由（至少 3 点）
  - 分析电影的独特之处
  - 指出适合的观众群体
  - 评分（1-10 分）
  `;
  
  const [resultA, resultB] = await Promise.all([
    llm.generate(promptA),
    llm.generate(promptB)
  ]);
  
  console.log("Prompt A 结果:", resultA);
  console.log("Prompt B 结果:", resultB);
  
  // 人工评估哪个更好
}
```

## 8. 最佳实践清单

✅ **DO - 推荐做法：**

1. **明确角色定位**
   ```
   ✅ "你是一位经验丰富的医生..."
   ❌ "请告诉我..."
   ```

2. **提供充足上下文**
   ```
   ✅ "针对 35 岁的男性上班族，有轻度高血压..."
   ❌ "有什么健康建议？"
   ```

3. **分解复杂任务**
   ```
   ✅ 第一步...第二步...第三步...
   ❌ "完成所有事情"
   ```

4. **使用分隔符**
   ```
   ✅ """
   文章内容：
   ---
   ${content}
   ---
   请总结上文
   """
   ```

5. **指定输出格式**
   ```
   ✅ "用表格形式呈现"
   ✅ "输出 JSON 格式"
   ```

❌ **DON'T - 避免做法：**

1. 不要一次问多个不相关的问题
2. 不要使用模糊的代词（它、这个、那个）
3. 不要假设模型知道隐含信息
4. 不要忽略负面约束（"不要做什么"）
5. 不要期望 100% 准确率（需要人工审核）

## 9. 工具与资源

### 9.1 Prompt 管理工具

```javascript
// Prompt 模板库
class PromptLibrary {
  constructor() {
    this.templates = new Map();
  }
  
  register(name, template) {
    this.templates.set(name, template);
  }
  
  get(name, variables = {}) {
    const template = this.templates.get(name);
    if (!template) throw new Error(`Template ${name} not found`);
    
    // 替换变量
    return template.replace(/\$\{(\w+)\}/g, (_, key) => {
      return variables[key] || '';
    });
  }
}

// 使用示例
const prompts = new PromptLibrary();

prompts.register('codeReview', `
你是一位资深代码审查员。

代码：
\`\`\`
${code}
\`\`\`

请从以下维度审查：
1. 代码规范
2. 潜在 bug
3. 性能问题
4. 可读性建议
`);

const reviewPrompt = prompts.get('codeReview', { code: myCode });
```

### 9.2 评估指标

| 指标 | 说明 | 评估方法 |
|------|------|---------|
| **准确性** | 答案是否正确 | 人工审核/标准答案对比 |
| **一致性** | 多次生成是否稳定 | Self-Consistency 测试 |
| **相关性** | 是否切题 | 人工评分/语义相似度 |
| **完整性** | 是否覆盖所有要点 | 检查清单核对 |
| **简洁性** | 是否冗余 | 字数统计/信息密度 |

## 10. 实战案例

### 案例：智能客服系统

```javascript
class CustomerServiceBot {
  constructor() {
    this.systemPrompt = `
    你是一位专业的电商客服助手，名为"小智"。
    
    服务准则：
    1. 热情友好，耐心细致
    2. 快速响应，解决问题
    3. 无法回答时，引导用户提供订单号
    4. 敏感问题（投诉、退款）建议转人工
    
    常用场景：
    - 物流查询
    - 退换货政策
    - 商品咨询
    - 优惠活动
    `;
  }
  
  async handleQuery(userMessage, context = {}) {
    const prompt = `
    ${this.systemPrompt}
    
    【对话历史】
    ${context.history || '无'}
    
    【用户信息】
    用户等级：${context.userLevel || '普通'}
    当前订单：${context.orderStatus || '无'}
    
    【用户问题】
    ${userMessage}
    
    请用亲切的语气回答，控制在 200 字以内。
    如果需要查询信息，请告诉用户稍等片刻。
    `;
    
    return await this.llm.generate(prompt);
  }
}

// 使用示例
const bot = new CustomerServiceBot();

const response = await bot.handleQuery(
  "我的快递到哪了？",
  {
    userLevel: 'VIP',
    orderStatus: '运输中',
    history: ''
  }
);

console.log(response);
// 输出："亲爱的 VIP 用户您好！您的包裹目前正在运输途中..."
```

---

**下一节：** [c2-3 上下文窗口管理](./c2-3-context-window-management.md)  
**上一节：** [c2-2 Function Calling / Tool Calling](./c2-2-function-calling.md)
