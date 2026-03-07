# c2-2 Function Calling / Tool Calling

## 1. 概述

Function Calling（函数调用）是让大模型从"纯聊天"转向"实际执行"的关键技术。通过此机制，LLM 可以理解用户意图并调用相应的工具完成任务。

## 2. 核心原理

### 2.1 工作流程

```
用户请求
   ↓
┌─────────────────┐
│ LLM + Tools 定义 │ ← 告诉模型有哪些工具可用
└────────┬────────┘
         ↓
┌─────────────────┐
│ LLM 分析意图     │
│ 决定调用哪个工具 │
│ 提取参数         │
└────────┬────────┘
         ↓
┌─────────────────┐
│ 返回结构化调用   │
│ {tool, args}    │
└────────┬────────┘
         ↓
┌─────────────────┐
│ 执行实际工具     │
└────────┬────────┘
         ↓
┌─────────────────┐
│ 返回结果给用户   │
└─────────────────┘
```

### 2.2 与传统 API 的区别

| 传统 API | Function Calling |
|---------|-----------------|
| 明确指定函数名 | 自然语言描述意图 |
| 精确参数传递 | 模糊参数自动补全 |
| 硬编码逻辑 | 动态选择函数 |
| 开发者定义流程 | AI 自主决策 |

## 3. 主流平台实现

### 3.1 OpenAI Function Calling

**定义工具：**
```javascript
const tools = [
  {
    type: "function",
    function: {
      name: "get_weather",
      description: "获取指定城市的天气信息",
      parameters: {
        type: "object",
        properties: {
          location: {
            type: "string",
            description: "城市名称，如'北京'"
          },
          unit: {
            type: "string",
            enum: ["celsius", "fahrenheit"],
            description: "温度单位"
          }
        },
        required: ["location"]
      }
    }
  },
  {
    type: "function",
    function: {
      name: "send_email",
      description: "发送邮件",
      parameters: {
        type: "object",
        properties: {
          to: { type: "string", description: "收件人邮箱" },
          subject: { type: "string", description: "邮件主题" },
          body: { type: "string", description: "邮件正文" }
        },
        required: ["to", "subject", "body"]
      }
    }
  }
];
```

**调用示例：**
```javascript
const response = await openai.chat.completions.create({
  model: "gpt-4-turbo",
  messages: [
    { role: "user", content: "明天北京天气怎么样？" }
  ],
  tools: tools,
  tool_choice: "auto"
});

// 检查是否需要调用工具
const message = response.choices[0].message;
if (message.tool_calls) {
  const toolCall = message.tool_calls[0];
  if (toolCall.function.name === "get_weather") {
    const args = JSON.parse(toolCall.function.arguments);
    const weather = await getWeather(args.location, args.unit);
    
    // 将结果返回给模型生成最终回复
    const finalResponse = await openai.chat.completions.create({
      model: "gpt-4-turbo",
      messages: [
        { role: "user", content: "明天北京天气怎么样？" },
        { role: "assistant", tool_calls: message.tool_calls },
        { 
          role: "tool", 
          tool_call_id: toolCall.id,
          content: JSON.stringify(weather)
        }
      ]
    });
  }
}
```

### 3.2 Anthropic Claude Tools

**定义方式：**
```javascript
const tools = [
  {
    name: "get_weather",
    description: "Get the current weather in a given location",
    input_schema: {
      type: "object",
      properties: {
        location: {
          type: "string",
          description: "The city and state, e.g. San Francisco, CA"
        },
        unit: {
          type: "string",
          enum: ["celsius", "fahrenheit"]
        }
      },
      required: ["location"]
    }
  }
];

const response = await anthropic.messages.create({
  model: "claude-3-opus-20240229",
  max_tokens: 1024,
  tools: tools,
  messages: [
    { role: "user", content: "What's the weather like in Beijing?" }
  ]
});

// 处理工具调用
if (response.stop_reason === "tool_use") {
  const toolUse = response.content.find(block => block.type === "tool_use");
  console.log("Tool called:", toolUse.name);
  console.log("Arguments:", toolUse.input);
}
```

### 3.3 开源方案：LlamaIndex

```python
from llama_index.core.tools import FunctionTool
from llama_index.llms.openai import OpenAI

# 定义工具
def multiply(a: int, b: int) -> int:
    """Multiply two integers."""
    return a * b

tool = FunctionTool.from_defaults(
    fn=multiply,
    name="multiply",
    description="Multiply two integers together"
)

# 创建 Agent
llm = OpenAI(model="gpt-4")
agent = ReActAgent.from_tools([tool], llm=llm, verbose=True)

# 使用 Agent
response = agent.chat("What is 123 * 456?")
print(response)
```

## 4. 参数提取技巧

### 4.1 强制格式化输出（重要！）

**问题：** 如何确保大模型按指定 JSON 格式输出？

#### 方法 1：使用平台的 Function Calling API（推荐）

OpenAI/Claude 等官方 API **自动保证**输出符合定义的 schema：

```javascript
// OpenAI 会自动验证输出格式
const response = await openai.chat.completions.create({
  model: "gpt-4-turbo",
  messages: [{ role: "user", content: "北京天气" }],
  tools: [{
    type: "function",
    function: {
      name: "get_weather",
      parameters: {
        type: "object",
        properties: {
          location: { type: "string" },
          unit: { type: "string", enum: ["celsius", "fahrenheit"] }
        },
        required: ["location"]
      }
    }
  }],
  tool_choice: "auto"
});

// ✅ 返回的 arguments 一定是合法的 JSON
const args = JSON.parse(response.choices[0].message.tool_calls[0].function.arguments);
// args 类型：{ location: string; unit?: string }
```

**优势：**
- ✅ 平台自动验证格式
- ✅ 类型安全
- ✅ 无需手动解析错误处理

#### 方法 2：Few-shot Prompting（少样本提示）

在 prompt 中提供多个正确格式的示例：

```javascript
const prompt = `
你是一个 API 调用助手。请根据用户输入，生成正确的工具调用。

可用工具：
${JSON.stringify(tools, null, 2)}

===== 示例 =====

用户："帮我查一下北京的天气"
工具调用：
{
  "name": "get_weather",
  "arguments": {
    "location": "北京"
  }
}

用户："给李四发邮件，主题是会议通知"
工具调用：
{
  "name": "send_email",
  "arguments": {
    "to": "lisi@example.com",
    "subject": "会议通知",
    "body": "您好，请查收会议通知。"
  }
}

===== 现在开始 =====

用户："${userInput}"
工具调用：
`;

const response = await llm.generate(prompt);
const toolCall = JSON.parse(response.trim());
```

**关键技巧：**
- ✅ 提供 3-5 个不同场景的示例
- ✅ 示例格式必须完全一致
- ✅ 最后一个示例留空让模型补全

#### 方法 3：Output Schema 约束

明确要求模型只输出 JSON，不要其他内容：

```javascript
const systemPrompt = `
你是一个函数调用引擎。你的任务是根据用户输入选择合适的工具并提取参数。

重要规则：
1. 只输出 JSON 格式，不要有任何解释文字
2. JSON 必须符合以下 schema：
   {
     "type": "object",
     "properties": {
       "tool": { "type": "string", "enum": ["get_weather", "send_email"] },
       "args": { "type": "object" }
     },
     "required": ["tool", "args"]
   }
3. 如果无法确定工具或参数，返回 null

可用工具定义：
${JSON.stringify(tools, null, 2)}
`;

const userPrompt = `用户输入：明天北京天气怎么样？`;

const response = await llm.generate(systemPrompt + '\n\n' + userPrompt);

// 防御性解析
try {
  const toolCall = JSON.parse(response);
  // 验证 schema
  if (!toolCall.tool || !toolCall.args) {
    throw new Error('Invalid format');
  }
} catch (error) {
  console.error('Failed to parse tool call:', error);
  // 降级处理...
}
```

#### 方法 4：Grammar/JSON Mode

部分平台支持强制 JSON 输出模式：

```javascript
// OpenAI JSON Mode
const response = await openai.chat.completions.create({
  model: "gpt-4-turbo",
  messages: [
    { 
      role: "system", 
      content: "You are a function calling AI. Respond only with valid JSON." 
    },
    { role: "user", content: userInput }
  ],
  response_format: { type: "json_object" }  // 🔑 强制 JSON 输出
});

const toolCall = JSON.parse(response.choices[0].message.content);
```

**支持的平台：**
- ✅ OpenAI GPT-4 Turbo (`response_format: { type: "json_object" }`)
- ✅ Claude 3 (`stop_sequences: ['```']`)
- ✅ Llama.cpp (BNF grammar)
- ✅ vLLM (guided generation)

#### 方法 5：后处理验证与修复

```javascript
class ToolCallValidator {
  constructor(schema) {
    this.schema = schema;
    this.ajv = new Ajv(); // JSON Schema validator
    this.validate = this.ajv.compile(schema);
  }
  
  async validateAndFix(rawOutput) {
    // 尝试直接解析
    try {
      const parsed = JSON.parse(rawOutput);
      if (this.validate(parsed)) {
        return parsed; // ✅ 验证通过
      }
    } catch (e) {
      console.log('Initial parse failed, attempting repair...');
    }
    
    // 尝试修复：提取 JSON 片段
    const jsonMatch = rawOutput.match(/\{[\s\S]*\}/);
    if (jsonMatch) {
      try {
        const extracted = JSON.parse(jsonMatch[0]);
        if (this.validate(extracted)) {
          return extracted;
        }
      } catch (e) {}
    }
    
    // 请求模型重新生成
    return await this.requestRegeneration(rawOutput);
  }
  
  async requestRegeneration(failedOutput) {
    const retryPrompt = `
    你之前的输出格式不正确：
    ${failedOutput}
    
    请严格按照以下 JSON Schema 重新输出：
    ${JSON.stringify(this.schema, null, 2)}
    
    只输出 JSON，不要其他文字：
    `;
    
    const newResponse = await llm.generate(retryPrompt);
    return JSON.parse(newResponse);
  }
}

// 使用示例
const validator = new ToolCallValidator({
  type: "object",
  properties: {
    tool: { type: "string" },
    args: { type: "object" }
  },
  required: ["tool", "args"]
});

const toolCall = await validator.validateAndFix(llmOutput);
```

**对比总结：**

| 方法 | 可靠性 | 实现难度 | 适用场景 |
|------|--------|---------|---------|
| **官方 Function Calling** | ⭐⭐⭐⭐⭐ | ⭐ | 首选，如果有 API |
| **JSON Mode** | ⭐⭐⭐⭐ | ⭐ | OpenAI/Claude 用户 |
| **Few-shot Prompting** | ⭐⭐⭐ | ⭐⭐ | 通用，任何模型 |
| **Output Schema** | ⭐⭐⭐ | ⭐⭐ | 需要明确约束 |
| **后处理验证** | ⭐⭐⭐⭐ | ⭐⭐⭐ | 高可靠性要求 |

**最佳实践：**
```javascript
// 组合使用多种方法
const robustToolCalling = async (userInput) => {
  // 1. 使用官方 API（最可靠）
  if (platform.supportsFunctionCalling) {
    return await platform.callFunction(userInput);
  }
  
  // 2. 启用 JSON Mode
  const response = await llm.generate(userInput, {
    response_format: { type: "json_object" },
    system: `You only output valid JSON.`
  });
  
  // 3. 验证和修复
  const validator = new ToolCallValidator(toolSchema);
  return await validator.validateAndFix(response);
};
```

### 4.2 隐式参数推断

**用户输入：**
```
"帮我给张三发邮件，说会议改到下午 3 点"
```

**期望提取：**
```json
{
  "tool": "send_email",
  "args": {
    "to": "zhangsan@example.com",
    "subject": "会议时间调整",
    "body": "您好，会议时间已改到今天下午 3 点。请准时参加。"
  }
}
```

**实现方法：**
```javascript
// 在 system prompt 中提供背景信息
const systemPrompt = `
你是一个智能助手，帮助用户完成任务。

已知信息：
- 张三的邮箱是 zhangsan@example.com
- 当前时间是 2024-03-07 14:00
- 默认会议时长为 1 小时

可用工具：
${JSON.stringify(tools, null, 2)}
`;
```

### 4.3 多轮对话参数收集

```javascript
async function collectParameters(schema, userMessage, conversationHistory) {
  const collectedParams = {};
  const missingParams = [];
  
  // 检查哪些参数已经有了
  for (const [key, prop] of Object.entries(schema.properties)) {
    const value = extractFromConversation(key, conversationHistory);
    if (value) {
      collectedParams[key] = value;
    } else if (schema.required?.includes(key)) {
      missingParams.push(key);
    }
  }
  
  // 如果有缺失的参数，向用户提问
  if (missingParams.length > 0) {
    const question = generateClarifyingQuestion(missingParams, schema);
    return { 
      action: "ask", 
      question: question,
      collectedSoFar: collectedParams
    };
  }
  
  return { action: "execute", params: collectedParams };
}
```

## 5. 错误处理策略

### 5.1 常见错误类型

| 错误类型 | 原因 | 解决方案 |
|---------|------|---------|
| 参数缺失 | 用户未提供必要信息 | 追问澄清 |
| 参数格式错误 | 日期、数字格式不对 | 格式化重试 |
| 工具不存在 | 拼写错误或未注册 | 返回友好提示 |
| 权限不足 | 缺少 API Key 或授权 | 引导用户授权 |
| 超时 | 网络或 API 问题 | 重试或降级 |

### 5.2 重试机制

```javascript
async function callToolWithRetry(toolName, args, maxRetries = 3) {
  let lastError;
  
  for (let i = 0; i < maxRetries; i++) {
    try {
      const result = await executeTool(toolName, args);
      return { success: true, data: result };
    } catch (error) {
      lastError = error;
      
      // 判断是否可重试
      if (!isRetryableError(error)) {
        break;
      }
      
      // 指数退避
      const delay = Math.pow(2, i) * 1000;
      await sleep(delay);
    }
  }
  
  return { 
    success: false, 
    error: lastError.message,
    suggestion: "请稍后重试或联系管理员"
  };
}
```

### 5.3 优雅降级

```javascript
async function gracefulToolFallback(toolName, args, error) {
  const fallbackStrategies = {
    "send_email": async () => {
      // 邮件发送失败，尝试生成草稿
      return {
        action: "draft_created",
        message: "邮件发送失败，已为您保存草稿",
        draft: { to: args.to, subject: args.subject, body: args.body }
      };
    },
    
    "get_weather": async () => {
      // 天气 API 失败，返回缓存数据或建议
      return {
        action: "cached_data",
        message: "实时天气数据暂时不可用，建议查看天气预报网站",
        lastKnown: getCachedWeather(args.location)
      };
    }
  };
  
  const fallback = fallbackStrategies[toolName];
  if (fallback) {
    return await fallback();
  }
  
  throw error;
}
```

## 6. 实战案例

### 6.1 智能日程管理

**场景：** 用户通过自然语言安排会议

**工具定义：**
```javascript
const calendarTools = [
  {
    name: "check_availability",
    description: "检查指定时间的可用性",
    parameters: {
      type: "object",
      properties: {
        startTime: { type: "string", format: "date-time" },
        endTime: { type: "string", format: "date-time" },
        attendees: { 
          type: "array", 
          items: { type: "string" } 
        }
      }
    }
  },
  {
    name: "create_event",
    description: "创建日历事件",
    parameters: {
      type: "object",
      properties: {
        title: { type: "string" },
        startTime: { type: "string", format: "date-time" },
        endTime: { type: "string", format: "date-time" },
        attendees: { type: "array", items: { type: "string" } },
        location: { type: "string" },
        description: { type: "string" }
      }
    }
  }
];
```

**对话流程：**
```
用户："下周一上午 10 点和团队开个项目启动会"

AI 思考过程：
1. 解析时间：下周一 = 2024-03-11, 上午 10 点 = 10:00
2. 识别参会人："团队" → 需要从联系人列表获取
3. 估算时长：默认 1 小时
4. 调用 check_availability 确认可用性
5. 如果可用，调用 create_event 创建事件

执行结果：
✅ 已创建事件
- 标题：项目启动会
- 时间：2024-03-11 10:00-11:00
- 参会人：张三、李四、王五
- 会议链接：https://meet.example.com/abc123
```

### 6.2 数据分析助手

**场景：** 用户查询业务数据

**工具集：**
```javascript
const analyticsTools = [
  {
    name: "query_sales_data",
    description: "查询销售数据",
    parameters: {
      type: "object",
      properties: {
        dateRange: { 
          type: "object",
          properties: {
            start: { type: "string", format: "date" },
            end: { type: "string", format: "date" }
          }
        },
        product: { type: "string" },
        region: { type: "string" },
        metrics: { 
          type: "array", 
          items: { enum: ["revenue", "units", "growth"] }
        }
      }
    }
  },
  {
    name: "generate_chart",
    description: "生成图表",
    parameters: {
      type: "object",
      properties: {
        data: { type: "array" },
        chartType: { 
          type: "string", 
          enum: ["line", "bar", "pie"] 
        },
        xAxis: { type: "string" },
        yAxis: { type: "string" }
      }
    }
  }
];
```

**执行流程：**
```
用户："帮我看看上个月华东地区的销售情况，画个趋势图"

步骤：
1. query_sales_data(
     dateRange: { start: "2024-02-01", end: "2024-02-29" },
     region: "华东",
     metrics: ["revenue", "units", "growth"]
   )
   
2. 获取数据后，调用 generate_chart(
     data: [...],
     chartType: "line",
     xAxis: "date",
     yAxis: "revenue"
   )

输出：
📊 华东地区 2 月销售趋势图
[图表图片]

关键洞察：
- 总营收：¥1,234,567 (+15% YoY)
- 销量：8,901 件
- 增长最快产品：XXX
```

## 7. 最佳实践

✅ **DO - 推荐做法：**

1. **工具命名要直观**
   ```javascript
   // ✅ 好
   { name: "send_email", ... }
   
   // ❌ 不好
   { name: "email_sender_v2", ... }
   ```

2. **描述要详细清晰**
   ```javascript
   // ✅ 好
   description: "发送邮件到指定收件人，支持 HTML 格式"
   
   // ❌ 不好
   description: "发邮件"
   ```

3. **提供足够的示例**
   ```javascript
   const examples = `
   用户："给张三发邮件说你好"
   → send_email(to="zhangsan@example.com", subject="问候", body="你好")
   `;
   ```

4. **处理边界情况**
   ```javascript
   // 在工具内部验证参数
   if (args.to.includes('@') === false) {
     throw new Error("无效的邮箱地址");
   }
   ```

❌ **DON'T - 避免做法：**

1. 不要定义过多工具（< 10 个为佳）
2. 不要让参数过于复杂
3. 不要忽略错误处理
4. 不要假设 100% 准确率

## 8. 性能优化

### 8.1 减少 Token 消耗

```javascript
// 压缩工具描述
const compactTools = tools.map(tool => ({
  name: tool.name,
  desc: tool.description.substring(0, 100), // 限制描述长度
  params: simplifySchema(tool.parameters)
}));
```

### 8.2 缓存常用调用

```javascript
const toolCallCache = new Map();

async function cachedToolCall(toolName, args) {
  const key = `${toolName}:${JSON.stringify(args)}`;
  
  if (toolCallCache.has(key)) {
    return toolCallCache.get(key);
  }
  
  const result = await executeTool(toolName, args);
  toolCallCache.set(key, result);
  
  // 5 分钟后过期
  setTimeout(() => toolCallCache.delete(key), 300000);
  
  return result;
}
```

## 9. 调试技巧

### 9.1 日志记录

```javascript
function logToolCall(toolName, args, result) {
  console.log(JSON.stringify({
    timestamp: new Date().toISOString(),
    tool: toolName,
    input: args,
    output: result,
    duration: result.duration
  }, null, 2));
}
```

### 9.2 可视化追踪

使用 LangSmith、Weights & Biases 等工具追踪完整的调用链。

---

**下一节：** [c2-3 上下文窗口管理](./c2-3-context-window-management.md)  
**上一节：** [c2-1 提示工程实践](./c2-1-prompt-engineering.md)
