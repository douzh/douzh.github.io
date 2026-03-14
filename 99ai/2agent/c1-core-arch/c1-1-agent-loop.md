# c1-1 Agent Loop 设计模式

## 1. 概述

Agent Loop（智能体循环）是 AI智能体的核心工作模式，描述了智能体从接收指令到完成任务的完整闭环流程。

## 2. Agent Loop 五阶段模型

```
┌─────────────┐
│  感知 (Sense)  │ ← 接收用户指令、环境信息
└──────┬──────┘
       ↓
┌─────────────┐
│  规划 (Plan)  │ ← 理解意图、制定计划
└──────┬──────┘
       ↓
┌─────────────┐
│ 行动 (Act)   │ ← 调用工具、执行操作
└──────┬──────┘
       ↓
┌─────────────┐
│ 观察 (Observe)│ ← 检查结果、收集反馈
└──────┬──────┘
       ↓
┌─────────────┐
│ 反馈 (Report) │ ← 汇报结果、更新记忆
└─────────────┘
```

### 2.1 感知阶段 (Sense)

**职责：** 接收并预处理输入信息

**输入来源：**
- 用户消息（聊天平台、语音输入）
- 定时任务触发器
- 系统监控告警
- 其他智能体消息

**关键处理：**
```typescript
interface SenseInput {
  source: 'user' | 'scheduler' | 'monitor' | 'agent';
  content: string;
  timestamp: number;
  context?: {
    userId: string;
    platform: string;
    conversationId: string;
  };
}
```

### 2.2 规划阶段 (Plan)

**职责：** 理解意图并制定执行计划

**核心步骤：**

1. **意图识别**
   - 分类任务类型（查询、执行、创作、分析）
   - 提取关键实体（时间、地点、对象）
   - 识别约束条件（预算、优先级）

2. **任务拆解**
   ```
   原始任务："帮我分析本周行业热点并写一篇推文"
   
   拆解为：
   - 子任务 1：抓取本周科技资讯
   - 子任务 2：筛选 TOP10 热点话题
   - 子任务 3：确定推文主题和角度
   - 子任务 4：撰写推文内容
   - 子任务 5：配图并格式化
   ```

3. **工具匹配**
   - 根据子任务选择合适的技能/工具
   - 检查工具可用性和权限
   - 生成工具调用序列

4. **计划验证**
   - 检查依赖关系是否合理
   - 评估预期耗时
   - 识别潜在风险点

### 2.3 行动阶段 (Act)

**职责：** 执行计划中的各个步骤

**执行模式：**

```typescript
// 串行执行（默认）
await step1();
await step2();
await step3();

// 并行执行（无依赖时）
await Promise.all([step1(), step2()]);

// 条件执行
if (condition) {
  await branchA();
} else {
  await branchB();
}
```

**工具调用示例：**
```typescript
// 执行 Shell 命令
const result = await tools.shell.execute('ls -la');

// 文件操作
await tools.fileSystem.write('output.md', content);

// 浏览器自动化
const data = await tools.browser.scrape('https://example.com');

// API 调用
const response = await tools.http.get('/api/data');
```

### 2.4 观察阶段 (Observe)

**职责：** 检查结果并收集反馈

**检查维度：**

1. **成功/失败判断**
   ```typescript
   interface ActionResult {
     success: boolean;
     data?: any;
     error?: Error;
     metadata: {
       duration: number;
       tokensUsed?: number;
       retries: number;
     };
   }
   ```

2. **数据验证**
   - 检查返回数据格式
   - 验证数据完整性
   - 识别异常值

3. **状态更新**
   - 更新任务进度
   - 记录中间结果
   - 标记需要重试的步骤

### 2.5 反馈阶段 (Report)

**职责：** 向用户汇报结果并更新系统状态

**反馈内容：**

```markdown
✅ 任务完成报告

**原始任务：** [任务描述]

**执行结果：**
- 步骤 1: ✅ 完成
- 步骤 2: ✅ 完成
- 步骤 3: ⚠️ 部分完成（说明原因）

**产出物：**
- [文件路径/链接 1]
- [文件路径/链接 2]

**关键指标：**
- 总耗时：2 分 30 秒
- Token 消耗：1,234
- 调用工具：5 次

**后续建议：**
[可选的后续操作建议]
```

**记忆更新：**
- 短期记忆：添加到对话上下文
- 长期记忆：结构化存储到 Markdown/SQlite
- 偏好学习：记录用户反馈和习惯

## 3. 完整循环示例

### 场景：安排会议

```
用户指令："明天下午 3 点安排一个项目评审会议，邀请张三和李四"

【感知】
- 接收消息：WhatsApp
- 解析内容：时间（明天 15:00）、事件（项目评审）、参会人（张三、李四）

【规划】
1. 检查日历空闲时段
2. 预定会议室或生成线上会议链接
3. 发送邀请邮件给参会人
4. 设置会议提醒

【行动】
- 调用 Google Calendar API 检查可用性
- 创建日历事件
- 发送邮件邀请
- 设置提前 15 分钟提醒

【观察】
- ✅ 日历事件创建成功
- ✅ 邮件已发送
- ✅ 提醒已设置
- 冲突检测：张三明天 2-4 点已有安排 → 需要调整

【反馈】
"会议已安排，但张三时间冲突。建议：
1. 改到明天 4 点后
2. 或今天任何时间
请确认哪个时间更合适？"
```

## 4. 异常处理策略

### 4.1 错误类型

| 错误类型 | 处理方式 | 示例 |
|---------|---------|------|
| 工具调用失败 | 重试 3 次后跳过或终止 | API 超时 |
| 权限不足 | 请求用户授权 | 访问受限文件 |
| 数据格式错误 | 尝试修复或报错 | JSON 解析失败 |
| 依赖缺失 | 提示安装或寻找替代 | 缺少 Python 库 |

### 4.2 恢复机制

```typescript
try {
  await action();
} catch (error) {
  // 1. 自动重试
  if (retryCount < MAX_RETRIES) {
    await retry(action);
  } 
  // 2. 降级方案
  else if (fallbackAction) {
    await fallbackAction();
  } 
  // 3. 人工介入
  else {
    await requestHumanIntervention(error);
  }
}
```

## 5. 性能优化

### 5.1 缓存策略

- **LLM 响应缓存**：相同 prompt 直接返回缓存结果
- **工具结果缓存**：API 调用结果缓存 5-30 分钟
- **上下文缓存**：会话历史缓存在内存中

### 5.2 并行化

```
可并行的步骤：
- 同时查询多个数据源
- 并行处理独立子任务
- 批量发送邮件/消息

不可并行的步骤：
- 有依赖关系的操作
- 共享状态修改
- 需要顺序保证的任务
```

### 5.3 Token 优化

- 精简上下文（只保留相关信息）
- 使用摘要替代完整历史
- 选择合适大小的模型

## 6. 调试与监控

### 6.1 日志记录

```json
{
  "timestamp": "2026-03-07T14:30:00Z",
  "loopId": "agent-loop-123",
  "phase": "act",
  "action": "calendar.createEvent",
  "input": {...},
  "output": {...},
  "duration": 234,
  "tokensUsed": 150
}
```

### 6.2 关键指标

- **循环次数**：每天执行的 Agent Loop 数量
- **成功率**：一次性完成的任务比例
- **平均耗时**：从感知到反馈的平均时间
- **重试率**：需要重试的操作比例
- **用户满意度**：基于反馈评分

## 7. 最佳实践

✅ **DO - 推荐做法：**
- 保持每个阶段职责单一清晰
- 为所有外部调用添加超时限制
- 记录详细的执行日志
- 提供友好的错误提示
- 支持人工中断和干预

❌ **DON'T - 避免做法：**
- 不要在规划阶段执行实际操作
- 不要忽略小概率的错误情况
- 不要在没有确认的情况下执行危险操作
- 不要缓存敏感个人信息
- 不要让循环无限执行（设置最大迭代次数）

## 8. 参考资源

- [OpenClaw Agent Loop 实现](https://github.com/openclaw/agent-loop)
- [Nanobot 最小化 Agent 循环](https://github.com/hkust-nlp/nanobot)
- [ReAct: Synergizing Reasoning and Acting](https://arxiv.org/abs/2210.03629)

---

**下一节：** [c1-2 轮辐式架构实现](./c1-2-hub-spoke-architecture.md)  
**上一节：** [c1 核心架构模块](./README.md)
