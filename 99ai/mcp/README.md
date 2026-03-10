# MCP 文档索引

本目录包含 MCP (Model Context Protocol) 相关的完整文档。

---

## 📚 文档列表

### 1. [MCP 使用指南](./mcp-user-guide.md) 👤
**面向用户** - 如何配置和使用 MCP 服务

**主要内容:**
- ✅ MCP 简介和核心价值
- ✅ 快速开始（安装和配置）
- ✅ **如何在 IDE 中添加 MCP 服务**（包含界面截图说明）
- ✅ 可用工具列表
- ✅ 使用示例
- ✅ 启动模式说明
- ✅ 常见问题解答

**适合人群:** 普通用户、开发者（快速上手）

---

### 2. [MCP 开发指南](./mcp-development-guide.md) 💻
**面向开发者** - 如何查询工具、集成 MCP、自定义开发

**主要内容:**
- ✅ 获取工具列表的多种方法
- ✅ JSON-RPC 协议详解
- ✅ Python/Node.js 代码示例
- ✅ 命令行测试脚本
- ✅ 调用原理和流程
- ✅ 自定义 MCP Server 开发
- ✅ 调试技巧

**适合人群:** 开发者、需要深度集成的用户

---

### 3. [MCP 协议规范](./mcp-specification.md) 📋
**官方规范整理** - MCP 协议的完整说明

**主要内容:**
- ✅ MCP 概述和核心价值
- ✅ 基础架构详解 (Client-Host-Server)
- ✅ 核心组件说明 (Hosts, Clients, Servers)
- ✅ JSON-RPC 2.0 协议格式
- ✅ 会话管理和初始化流程
- ✅ 安全机制和认证流程
- ✅ 实现指南 (Node.js/Python)
- ✅ 应用场景和最佳实践

**适合人群:** 开发者、架构师、需要深入了解 MCP 的用户

---

### 4. [MCP 执行结果](./mcp-execution-results.md) 📊
**实际执行记录** - 工具调用的真实返回结果

**主要内容:**
- ✅ 数学计算工具执行结果
- ✅ 环境变量查询工具执行结果
- ✅ JSON-RPC 请求/响应格式示例
- ✅ 环境信息分析

**适合人群:** 测试人员、需要了解输出格式的开发者

---

## 🚀 快速导航

### 我想...

**配置 MCP 服务**  
→ 查看 [MCP 使用指南](./mcp-user-guide.md) 第三节

**查看有哪些工具可用**  
→ 查看 [MCP 使用指南](./mcp-user-guide.md) 第五节

**获取工具列表（命令行）**  
→ 查看 [MCP 开发指南](./mcp-development-guide.md) 第二节

**用代码集成 MCP**  
→ 查看 [MCP 开发指南](./mcp-development-guide.md) 第三节

**了解调用原理**  
→ 查看 [MCP 开发指南](./mcp-development-guide.md) 第五节

**调试 MCP 服务**  
→ 查看 [MCP 开发指南](./mcp-development-guide.md) 第八节

---

## 📋 文档结构

```
doc/mcp/
├── README.md                          # 本文档（索引）
├── mcp-specification.md               # 协议规范（核心文档）
├── mcp-user-guide.md                  # 使用指南（用户向）
├── mcp-development-guide.md           # 开发指南（开发者向）
└── mcp-execution-results.md           # 执行结果记录
```

---

## 🎯 推荐阅读顺序

### 对于普通用户
1. [MCP 使用指南](./mcp-user-guide.md) - 完整阅读
2. [MCP 执行结果](./mcp-execution-results.md) - 查看示例

### 对于开发者
1. [MCP 协议规范](./mcp-specification.md) - 深入阅读
2. [MCP 使用指南](./mcp-user-guide.md) - 快速浏览
3. [MCP 开发指南](./mcp-development-guide.md) - 实践操作
4. [MCP 执行结果](./mcp-execution-results.md) - 参考格式

### 对于测试人员
1. [MCP 使用指南](./mcp-user-guide.md) - 了解基本用法
2. [MCP 执行结果](./mcp-execution-results.md) - 查看实际输出

---

## 📝 更新记录

- **2026-03-10**: 添加 MCP 协议规范说明文档，基于官方规范整理
- **2026-03-06**: 创建完整文档体系，拆分为使用指南和开发指南
- **2026-03-06**: 添加 IDE 配置说明（包含界面截图说明）
- **2026-03-06**: 添加文档索引（本文档）

---

## 🔗 相关链接

- [MCP 官方规范](https://modelcontextprotocol.io/specification)
- [MCP SDK](https://github.com/modelcontextprotocol/sdk)
- [MCP Servers](https://github.com/modelcontextprotocol/servers)

---

**文档维护:** One AI Team  
**最后更新:**2026-03-10
