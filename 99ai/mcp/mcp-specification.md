# Model Context Protocol (MCP) 规范说明

> 本文档基于 MCP 官方规范整理，详细描述 Model Context Protocol 的架构、组件和实现方式。

---

## 📋 目录

- [概述](#概述)
- [基础架构](#基础架构)
- [核心组件](#核心组件)
- [通信协议](#通信协议)
- [会话管理](#会话管理)
- [安全机制](#安全机制)
- [实现指南](#实现指南)

---

## 概述

**Model Context Protocol (MCP)** 是一个开放协议，用于实现 LLM（大型语言模型）应用程序与外部数据源和工具之间的无缝集成。无论您是在构建 AI 驱动的 IDE、增强聊天界面，还是创建自定义 AI 工作流，MCP 都提供了一种标准化的方式来连接 LLM 与其所需的上下文。

### 核心价值

- 🔌 **标准化接口**: 统一的协议规范，类似 USB-C for AI
- 🔒 **安全性**: 内置认证和授权机制
- 🔄 **互操作性**: 支持多模型、多服务器协作
- 📦 **可扩展性**: 灵活的架构设计，易于扩展

---

## 基础架构

### 架构模式

MCP 采用 **客户端 - 主机 - 服务器 (Client-Host-Server)** 架构：

```
┌─────────────┐
│   Host      │  ← AI 应用程序 (如 Claude Desktop, IDE)
│             │
│  ┌─────────┐│
│  │ Client  ││  ← 协议客户端 (连接器)
│  └────┬────┘│
└───────┼─────┘
        │ JSON-RPC 2.0
        │ (stdio / SSE)
┌───────┼─────┐
│  ┌────▼────┐│
│  │ Server  ││  ← MCP 服务器 (提供功能)
│  └─────────┘│
└─────────────┘
```

### 架构特点

1. **每个主机可以运行多个客户端实例**
2. **清晰的边界**: 保持明确的安全边界和关注点隔离
3. **基于 JSON-RPC**: 提供有状态的会话协议
4. **专注于上下文交换**: 协调客户端和服务器之间的交互

---

## 核心组件

### 1. 主机 (Hosts)

**定义**: 发起连接的 LLM 应用程序

**职责**:
- 🖥️ 运行 AI 模型程序
- 👤 处理用户交互
- 🔐 管理权限和安全策略
- 📊 协调整体流程

**示例**:
- Claude Desktop
- AI 驱动的 IDE (如 Visual Studio Code)
- 聊天界面应用
- 自定义 AI 工作流平台

### 2. 客户端 (Clients)

**定义**: 主机应用程序内的连接器

**职责**:
- 🔗 管理与服务器的连接
- 📝 处理消息的编码和解码
- 💾 维护会话状态
- 🎯 转发请求和响应

**工作流程**:
```
Host → Client → Server
         ↓
    编码/解码
         ↓
    状态管理
```

### 3. 服务器 (Servers)

**定义**: 实现 MCP 协议的轻量级程序

**职责**:
- 🛠️ 暴露特定功能给 AI 模型
- 📚 提供上下文和数据访问
- 🔧 执行具体操作 (文件读写、数据库查询等)
- 📤 返回结构化结果

**功能类型**:
- **Tools (工具)**: 可执行的操作
- **Resources (资源)**: 可访问的数据
- **Prompts (提示)**: 预定义的交互模板

**示例**:
- 文件系统服务器
- 数据库服务器
- API 集成服务器
- 搜索引擎服务器

---

## 通信协议

### 传输层

MCP 支持多种传输方式：

#### 1. 标准输入输出 (stdio)

**适用场景**: 本地集成、命令行工具

**特点**:
- ✅ 进程间直接通信
- ✅ 低延迟
- ✅ 无需网络配置

**示例**:
```bash
# 启动 MCP 服务器
node mcp-server.js
```

#### 2. Server-Sent Events (SSE)

**适用场景**: 网络服务、远程连接

**特点**:
- ✅ 单向服务器推送
- ✅ 基于 HTTP 协议
- ✅ 防火墙友好

**通信流程**:
```
Client                    Server
  |                         |
  |--- HTTP GET (SSE) ------>|
  |                         |
  |<== event stream =========|
  |                         |
  |--- POST (JSON-RPC) ----->|
  |                         |
```

### 应用层：JSON-RPC 2.0

MCP 基于 **JSON-RPC 2.0** 协议构建。

#### 基本消息格式

**请求格式**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "方法名",
  "params": {
    // 参数
  }
}
```

**响应格式**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    // 成功结果
  }
  // 或
  "error": {
    "code": -32600,
    "message": "错误描述"
  }
}
```

#### 通知 (Notification)

不需要响应的消息：
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

---

## 会话管理

### 会话生命周期

```
1. 初始化阶段
   Client → Server: initialize 请求
   Server → Client: initialize 响应
   Client → Server: notifications/initialized

2. 运行阶段
   Client ↔ Server: 工具调用、资源访问

3. 关闭阶段
   Client → Server: 关闭连接
```

### 初始化流程

**步骤 1: 客户端发送初始化请求**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": {
        "listChanged": true
      }
    },
    "clientInfo": {
      "name": "example-client",
      "version": "1.0.0"
    }
  }
}
```

**步骤 2: 服务器响应**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": {},
      "resources": {}
    },
    "serverInfo": {
      "name": "example-server",
      "version": "1.0.0"
    }
  }
}
```

**步骤 3: 客户端确认**
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

---

## 安全机制

### 认证流程

MCP 支持完整的认证规范：

1. **OAuth 2.0**: 标准授权框架
2. **PKCE**: 增强安全性
3. **动态客户端注册**: 自动配置

### 访问控制

**权限级别**:
- 🔴 **只读**: 仅允许读取资源
- 🟡 **受限写入**: 限制特定的写操作
- 🟢 **完全访问**: 所有操作权限

**安全边界**:
- 进程隔离
- 文件系统沙箱
- 网络访问控制
- 资源配额限制

---

## 实现指南

### 开发 MCP Server

#### 使用 Node.js SDK

**安装依赖**:
```bash
npm install @modelcontextprotocol/sdk
```

**基本示例**:
```javascript
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

const server = new McpServer({
  name: "example-server",
  version: "1.0.0"
});

// 注册工具
server.tool("add", "加法运算", {
  a: { type: "number" },
  b: { type: "number" }
}, async ({ a, b }) => {
  return { result: a + b };
});

// 启动服务器
const transport = new StdioServerTransport();
await server.connect(transport);
```

#### 使用 Python SDK

**安装依赖**:
```bash
pip install mcp
```

**基本示例**:
```python
from mcp.server import Server
from mcp.server.stdio import stdio_server

app = Server("example-server")

@app.tool()
def add(a: float, b: float) -> float:
    """加法运算"""
    return a + b

async def main():
    async with stdio_server() as streams:
        await app.run(
            streams[0],
            streams[1]
        )
```

### 测试 MCP Server

**命令行测试**:
```bash
# 启动服务器并测试
node test-server.js

# 发送 JSON-RPC 请求
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05"}}' | node test-server.js
```

**使用 curl 测试 SSE**:
```bash
# 连接到 SSE 端点
curl -N http://localhost:3000/sse

# 发送 POST 请求
curl -X POST http://localhost:3000/message \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### 调试技巧

1. **启用日志**: 记录所有 JSON-RPC 消息
2. **使用开发者工具**: 检查网络请求
3. **验证协议版本**: 确保兼容性
4. **错误处理**: 捕获并记录异常

---

## 应用场景

### 1. AI 驱动的 IDE
- 📁 文件系统访问
- 🔍 代码搜索和分析
- 🏗️ 项目构建和运行

### 2. 智能助手
- 📊 数据查询和分析
- 📅 日历和任务管理
- 📧 邮件和通讯集成

### 3. 自动化工作流
- 🔄 跨应用操作
- 📈 数据处理管道
- 🤖 RPA 自动化

### 4. 研究和分析
- 📚 文献检索
- 📊 数据分析
- 🔬 实验管理

---

## 最佳实践

### 设计原则

1. **单一职责**: 每个服务器专注于特定功能
2. **最小权限**: 只授予必要的权限
3. **清晰接口**: 定义明确的工具和资源
4. **错误处理**: 提供有意义的错误信息

### 性能优化

- ⚡ 异步操作
- 💾 结果缓存
- 📦 批量处理
- 🎯 增量更新

### 安全性

- 🔐 输入验证
- 🔒 敏感数据加密
- 🚫 防止注入攻击
- 📝 审计日志

---

## 常见问题

### Q: MCP 与 Function Calling 有什么区别？

**A**: MCP 是更底层的协议，定义了完整的通信架构；Function Calling 是 LLM 的功能特性。MCP 可以作为 Function Calling 的实现基础。

### Q: 如何选择合适的传输方式？

**A**: 
- 本地集成优先选择 stdio
- 网络服务使用 SSE
- 需要双向通信考虑 WebSocket (未来支持)

### Q: MCP 支持哪些编程语言？

**A**: 官方提供 TypeScript 和 Python SDK，社区正在开发 Java、Go、Rust 等语言的实现。

---

## 参考资料

### 官方资源

- 📖 [MCP 官方规范](https://spec.modelcontextprotocol.io/)
- 💻 [MCP SDK](https://github.com/modelcontextprotocol/sdk)
- 🌐 [MCP Servers](https://github.com/modelcontextprotocol/servers)
- 📚 [MCP 文档](https://modelcontextprotocol.io/docs)

### 社区资源

- 💬 [GitHub Discussions](https://github.com/modelcontextprotocol/specification/discussions)
- 🔧 [Awesome MCP](https://github.com/modelcontextprotocol/awesome-mcp)
- 📝 [MCP Blog](https://modelcontextprotocol.io/blog)

---

## 版本历史

| 版本 | 日期 | 主要更新 |
|------|------|----------|
| 2024-11-05 | 2024-11-05 | 初始版本发布 |
| Draft | 2024-10 | 公开草案 |

---

**文档维护**: One AI Team  
**最后更新**: 2026-03-10  
**协议版本**: 2024-11-05
