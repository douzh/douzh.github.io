# c1-2 轮辐式架构实现

## 1. 概述

轮辐式架构（Hub-and-Spoke）是 OpenClaw等AI智能体系统的核心架构模式，通过单一网关协调多个智能体和工具的执行。

## 2. 架构组成

```
┌─────────────────────────────────────────────────┐
│              用户输入渠道                        │
│  WhatsApp  Telegram  Slack  WebUI  CLI  ...    │
└───────────────┬─────────────────────────────────┘
                │
                ↓
        ┌───────────────┐
        │   网关 (Hub)   │ ← WebSocket 服务器
        │  - 消息路由    │
        │  - 会话管理    │
        │  - 访问控制    │
        └───────┬───────┘
                │
    ┌───────────┼───────────┐
    ↓           ↓           ↓
┌────────┐ ┌────────┐ ┌────────┐
│Agent 1 │ │Agent 2 │ │Agent 3 │
│运行时  │ │运行时  │ │运行时  │
└────┬───┘ └────┬───┘ └────┬───┘
     │          │          │
     └──────────┴──────────┘
                │
                ↓
        ┌───────────────┐
        │   工具层       │
        │ Shell  File   │
        │ Browser  API  │
        └───────────────┘
```

## 3. 核心组件详解

### 3.1 网关（Hub）

**职责：**
- 统一接入所有用户输入渠道
- 验证和授权请求
- 将会话路由到正确的 Agent
- 管理并发和限流
- 聚合多个 Agent 的响应

**技术实现：**
```typescript
class Gateway {
  private agents: Map<string, AgentRuntime>;
  private connections: Map<string, WebSocket>;
  
  // 启动 WebSocket 服务器
  async start(port: number) {
    this.wss = new WebSocket.Server({ port });
    this.wss.on('connection', this.handleConnection.bind(this));
  }
  
  // 处理新连接
  private handleConnection(ws: WebSocket) {
    const sessionId = generateSessionId();
    this.connections.set(sessionId, ws);
    
    ws.on('message', (data) => {
      this.routeMessage(sessionId, JSON.parse(data));
    });
  }
  
  // 路由消息到对应 Agent
  private routeMessage(sessionId: string, message: Message) {
    const agent = this.getAgentForSession(sessionId);
    agent.process(message).then(response => {
      this.sendResponse(sessionId, response);
    });
  }
}
```

### 3.2 Agent 运行时

**职责：**
- 执行完整的 Agent Loop
- 管理短期和长期记忆
- 调用工具和技能
- 维护任务状态

**核心接口：**
```typescript
interface AgentRuntime {
  // 处理单个消息
  processMessage(message: Message): Promise<Response>;
  
  // 执行任务
  executeTask(task: Task): Promise<TaskResult>;
  
  // 调用工具
  callTool(toolName: string, args: any): Promise<any>;
  
  // 更新记忆
  updateMemory(type: 'short' | 'long', data: any): void;
  
  // 获取上下文
  getContext(): AgentContext;
}
```

### 3.3 工具层

**工具分类：**

| 类别 | 工具示例 | 权限级别 |
|------|---------|---------|
| 系统级 | Shell 执行、文件操作 | 高 |
| 应用级 | 邮件、日历、浏览器 | 中 |
| 网络级 | HTTP 请求、API 调用 | 低 |
| 数据级 | 数据库查询、文件读写 | 中 |

**工具注册机制：**
```typescript
class ToolRegistry {
  private tools: Map<string, Tool> = new Map();
  
  // 注册工具
  register(tool: Tool) {
    this.tools.set(tool.name, tool);
    console.log(`Registered tool: ${tool.name}`);
  }
  
  // 获取工具
  get(name: string): Tool | undefined {
    return this.tools.get(name);
  }
  
  // 列出所有可用工具
  listTools(): ToolInfo[] {
    return Array.from(this.tools.values()).map(t => ({
      name: t.name,
      description: t.description,
      parameters: t.parameters
    }));
  }
}
```

## 4. 通信协议

### 4.1 WebSocket 消息格式

**客户端 → 网关：**
```json
{
  "type": "user_message",
  "sessionId": "sess_abc123",
  "userId": "user_xyz",
  "content": "帮我检查今天的日程安排",
  "platform": "telegram",
  "timestamp": 1709812800000
}
```

**网关 → Agent：**
```json
{
  "type": "agent_request",
  "requestId": "req_def456",
  "sessionId": "sess_abc123",
  "message": {
    "content": "帮我检查今天的日程安排",
    "context": {...}
  },
  "availableTools": ["calendar.read", "email.send"]
}
```

**Agent → 网关：**
```json
{
  "type": "agent_response",
  "requestId": "req_def456",
  "status": "success",
  "response": {
    "text": "您今天有 3 个会议...",
    "actions": [
      {"type": "show_calendar", "data": {...}}
    ]
  },
  "metadata": {
    "duration": 1234,
    "tokensUsed": 456
  }
}
```

### 4.2 会话管理

```typescript
interface Session {
  id: string;
  userId: string;
  platform: string;
  createdAt: number;
  lastActiveAt: number;
  context: {
    conversationHistory: Message[];
    currentTask?: Task;
    preferences: UserPreferences;
  };
}

// 会话超时处理
const SESSION_TIMEOUT = 30 * 60 * 1000; // 30 分钟

setInterval(() => {
  const now = Date.now();
  sessions.forEach((session, id) => {
    if (now - session.lastActiveAt > SESSION_TIMEOUT) {
      this.archiveSession(id);
    }
  });
}, 5 * 60 * 1000); // 每 5 分钟检查一次
```

## 5. 路由策略

### 5.1 基于会话的路由

```typescript
private getAgentForSession(sessionId: string): AgentRuntime {
  // 1. 检查是否已有专属 Agent
  const existing = this.sessionAgentMap.get(sessionId);
  if (existing) {
    return existing;
  }
  
  // 2. 负载均衡选择空闲 Agent
  const agent = this.selectLeastLoadedAgent();
  this.sessionAgentMap.set(sessionId, agent);
  return agent;
}
```

### 5.2 基于任务类型的路由

```typescript
private selectAgentByTaskType(taskType: string): AgentRuntime {
  switch (taskType) {
    case 'code_generation':
      return this.codeSpecialistAgent;
    case 'data_analysis':
      return this.analysisAgent;
    case 'creative_writing':
      return this.creativeAgent;
    default:
      return this.generalPurposeAgent;
  }
}
```

## 6. 扩展性设计

### 6.1 水平扩展

```
                    ┌──────────┐
                    │ 负载均衡  │
                    └────┬─────┘
                         │
         ┌───────────────┼───────────────┐
         ↓               ↓               ↓
   ┌──────────┐   ┌──────────┐   ┌──────────┐
   │ Gateway 1│   │ Gateway 2│   │ Gateway 3│
   └────┬─────┘   └────┬─────┘   └────┬─────┘
        │              │              │
        └──────────────┴──────────────┘
                       │
              ┌────────┴────────┐
              │  Redis Pub/Sub  │
              └────────┬────────┘
                       │
         ┌─────────────┼─────────────┐
         ↓             ↓             ↓
   ┌──────────┐ ┌──────────┐ ┌──────────┐
   │ Agent 池 1│ │ Agent 池 2│ │ Agent 池 3│
   └──────────┘ └──────────┘ └──────────┘
```

### 6.2 共享状态管理

使用 Redis 存储共享状态：
```typescript
import Redis from 'ioredis';

class SharedState {
  private redis: Redis;
  
  constructor(redisUrl: string) {
    this.redis = new Redis(redisUrl);
  }
  
  // 设置会话状态
  async setSessionState(sessionId: string, state: any) {
    await this.redis.setex(
      `session:${sessionId}`,
      1800, // 30 分钟过期
      JSON.stringify(state)
    );
  }
  
  // 获取会话状态
  async getSessionState(sessionId: string): Promise<any> {
    const data = await this.redis.get(`session:${sessionId}`);
    return data ? JSON.parse(data) : null;
  }
}
```

## 7. 容错机制

### 7.1 Agent 故障恢复

```typescript
class FaultTolerance {
  // 检测 Agent 健康状态
  private healthCheckInterval = setInterval(async () => {
    for (const [id, agent] of this.agents) {
      try {
        await agent.healthCheck();
      } catch (error) {
        console.error(`Agent ${id} unhealthy, restarting...`);
        await this.restartAgent(id);
      }
    }
  }, 30000);
  
  // 重启故障 Agent
  private async restartAgent(agentId: string) {
    const oldAgent = this.agents.get(agentId);
    oldAgent?.shutdown();
    
    const newAgent = await this.createAgent(agentId);
    this.agents.set(agentId, newAgent);
    
    // 迁移受影响的会话
    await this.migrateSessions(agentId);
  }
}
```

### 7.2 消息重试

```typescript
async function sendMessageWithRetry(
  ws: WebSocket, 
  message: any,
  maxRetries = 3
): Promise<boolean> {
  for (let i = 0; i < maxRetries; i++) {
    try {
      ws.send(JSON.stringify(message));
      return true;
    } catch (error) {
      if (i === maxRetries - 1) throw error;
      await sleep(1000 * Math.pow(2, i)); // 指数退避
    }
  }
  return false;
}
```

## 8. 性能优化

### 8.1 连接池化

```typescript
class ConnectionPool {
  private pool: AsyncQueue<WebSocket>[] = [];
  private size: number;
  
  constructor(size: number = 10) {
    this.size = size;
    for (let i = 0; i < size; i++) {
      this.pool.push(new AsyncQueue());
    }
  }
  
  async acquire(): Promise<WebSocket> {
    // 获取空闲连接
  }
  
  async release(ws: WebSocket): Promise<void> {
    // 释放连接回池
  }
}
```

### 8.2 消息批处理

```typescript
// 批量发送非紧急通知
class BatchProcessor {
  private buffer: Message[] = [];
  private interval: NodeJS.Timeout;
  
  start() {
    this.interval = setInterval(() => {
      if (this.buffer.length > 0) {
        this.sendBatch(this.buffer);
        this.buffer = [];
      }
    }, 1000); // 每秒发送一次
  }
  
  addMessage(msg: Message) {
    this.buffer.push(msg);
  }
}
```

## 9. 安全考虑

### 9.1 认证与授权

```typescript
class SecurityMiddleware {
  // JWT Token 验证
  authenticate(token: string): User {
    const decoded = jwt.verify(token, SECRET_KEY);
    return decoded as User;
  }
  
  // 权限检查
  authorize(user: User, action: string): boolean {
    return user.permissions.includes(action);
  }
  
  // 速率限制
  rateLimit(userId: string): boolean {
    const count = this.requestCounts.get(userId) || 0;
    if (count > MAX_REQUESTS_PER_MINUTE) {
      return false;
    }
    this.requestCounts.set(userId, count + 1);
    return true;
  }
}
```

### 9.2 沙箱隔离

```typescript
// 在沙箱中执行不受信任的代码
import { VM } from 'vm2';

const vm = new VM({
  timeout: 5000,
  sandbox: {
    console: console,
    Buffer: Buffer
  }
});

try {
  const result = vm.run(userCode);
} catch (error) {
  console.error('Sandbox execution failed:', error);
}
```

## 10. 监控与日志

### 10.1 关键指标

```typescript
interface Metrics {
  // 网关指标
  gatewayConnections: number;
  gatewayMessagesPerSecond: number;
  
  // Agent 指标
  activeAgents: number;
  agentUtilization: number;
  averageLoopDuration: number;
  
  // 工具指标
  toolCallsPerMinute: number;
  toolErrorRate: number;
  
  // 业务指标
  tasksCompleted: number;
  userSatisfactionScore: number;
}
```

### 10.2 分布式追踪

使用 OpenTelemetry 进行追踪：
```typescript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('gateway');

async function processMessage(message: Message) {
  return tracer.startActiveSpan('process.message', async (span) => {
    span.setAttribute('message.id', message.id);
    
    try {
      // ... 处理逻辑
      span.setStatus({ code: SpanStatusCode.OK });
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}
```

---

**下一节：** [c1-3 微内核与单体架构对比](./c1-3-microkernel-vs-monolith.md)  
**上一节：** [c1-1 Agent Loop 设计模式](./c1-1-agent-loop.md)
