# Lingma 智能体文档索引

本目录包含 Lingma 智能体相关的所有文档。

---

## 📚 文档列表

### 1. [Lingma 用户指南](./lingma-user-guide.md) 👤
**面向最终用户** - 如何使用 Lingma 智能助手

**主要内容:**
- ✅ 认识 Lingma（能力分层、核心功能）
- ✅ 快速开始（第一次使用、日常场景）
- ✅ 高效使用技巧（提问技巧、复杂任务）
- ✅ 高级功能（MCP 工具调用、物理世界交互）
- ✅ 能力边界和限制
- ✅ 常见问题解答
- ✅ 最佳实践

**适合人群:** 所有 Lingma 用户

**阅读建议:** 
- 新手：从头到尾完整阅读
- 有经验用户：直接查看"高级功能"和"最佳实践"

---

### 2. [Lingma 开发扩展指南](./lingma-development-guide.md) 💻
**面向开发者** - 如何扩展和集成 Lingma

**主要内容:**
- ✅ 架构概览（整体架构、工作流程）
- ✅ 工具匹配机制（算法、案例分析）
- ✅ 配置和部署（IDE 集成、环境变量）
- ✅ 调试和监控（日志、性能、问题排查）
- ✅ 扩展开发（自定义工具、技能扩展）
- ✅ 高级集成（Agent 集成、物理设备）
- ✅ 性能优化（缓存、并发、资源管理）
- ✅ 测试（单元测试、集成测试）

**适合人群:** 开发者、系统集成工程师

**阅读建议:**
- 应用开发者：重点阅读“配置和部署”、“扩展开发”
- 系统工程师：完整阅读所有章节

---

### 3. [Lingma 架构设计文档](./lingma-architecture.md) 🏗️
**面向架构师和高级开发者** - Lingma 的完整技术架构和实现细节

**主要内容:**
- ✅ 三层能力架构详解
- ✅ 系统架构图和组件说明
- ✅ AI 核心引擎实现（意图识别、参数提取、工具匹配）
- ✅ 工具抽象层设计（统一接口、错误处理）
- ✅ 文件系统工具实现（VS Code API、安全检查）
- ✅ 终端执行工具实现（命令验证、超时处理）
- ✅ 项目分析工具（结构识别、入口点查找）
- ✅ AI 与 IDE 通信机制（消息格式、通信流程）
- ✅ 完整工作流程示例（创建项目、执行构建）
- ✅ 安全机制（多层防护、审计日志）
- ✅ 性能优化（缓存策略、并发控制）
- ✅ 错误处理和监控诊断
- ✅ 扩展机制（自定义工具注册）

**适合人群:** 系统架构师、高级开发者、技术负责人

**阅读建议:**
- 架构师：重点关注系统架构、安全机制、扩展机制
- 高级开发者：深入阅读各个工具的详细实现
- 配合"Lingma 开发扩展指南"一起阅读

---

### 4. [VS Code Agent 接口完整指南](./vscode-api-guide.md) 🔌
**面向开发者** - VS Code 提供给 Agent 调用的所有 API 接口

**主要内容:**
- ✅ 文件系统接口（读写、创建、删除、目录操作）
- ✅ 终端和命令执行接口（创建终端、发送命令、任务执行）
- ✅ 代码分析和导航接口（符号查找、定义定位、引用搜索）
- ✅ 调试接口（启动调试、断点管理、会话监听）
- ✅ 窗口和用户界面接口（消息显示、输入框、快速选择、Webview）
- ✅ 配置和工作区接口（读取配置、修改配置、工作区管理）
- ✅ 命令执行接口（内置命令、自定义命令注册）
- ✅ 安全和权限检查（路径验证、命令过滤）
- ✅ 完整示例（项目模板创建、自动化工作流）
- ✅ 最佳实践（错误处理、性能优化）

**适合人群:** VS Code 扩展开发者、Agent 开发者

**阅读建议:**
- 作为工具书查阅，按需查看相关章节
- 重点关注第十章的完整示例
- 参考第十一章的最佳实践

---

### 5. [Agent 对接 MCP 指南](./mcp-integration-guide.md) 🔌
**面向 Agent 开发者** - 如何在 Agent 中集成 MCP

**主要内容:**
- ✅ MCP 与 Agent 的关系
- ✅ Agent 集成 MCP 的完整流程
- ✅ 工具发现和管理
- ✅ 意图理解和工具匹配
- ✅ 工具调用和执行
- ✅ Agent 完整示例（MCPEnabledAgent）
- ✅ 调试和监控
- ✅ 最佳实践

**适合人群:** AI Agent 开发者、架构师

**阅读建议:**
- 配合"Lingma 开发扩展指南"一起阅读
- 重点关注 Agent 架构设计和工具匹配机制

---

## 🚀 快速导航

### 我想...

**开始使用 Lingma**  
→ 查看 [Lingma 用户指南](./lingma-user-guide.md)

**了解 Lingma 能做什么**  
→ 查看 [Lingma 用户指南](./lingma-user-guide.md) 第一章

**学习高效提问技巧**  
→ 查看 [Lingma 用户指南](./lingma-user-guide.md) 第三章

**配置和部署 Lingma**  
→ 查看 [Lingma 开发扩展指南](./lingma-development-guide.md) 第三章

**调试和监控 Lingma**  
→ 查看 [Lingma 开发扩展指南](./lingma-development-guide.md) 第四章

**扩展 Lingma 的能力**  
→ 查看 [Lingma 开发扩展指南](./lingma-development-guide.md) 第五章

**在 Agent 中集成 MCP**  
→ 查看 [Agent 对接 MCP 指南](./mcp-integration-guide.md)

**开发自定义 MCP 工具**  
→ 查看 [MCP 开发指南](../mcp/mcp-development-guide.md)

---

## 📋 文档结构

```
doc/agent/
├── README.md                          # 本文档（索引）
├── lingma-user-guide.md               # 👤 用户指南（623 行，含记忆、规则、Skill）
├── lingma-development-guide.md        # 💻 开发指南（540 行）
├── lingma-architecture.md             # 🏗️ 架构文档（1463 行）
├── vscode-api-guide.md                # 🔌 VS Code 接口指南（1982 行）
├── mcp-integration-guide.md           # 🔌 MCP 集成指南（790 行）
└── lingma-skill-guide.md              # 🎯 Skill 配置指南（1059 行）← 新增
```

---

## 🎯 推荐阅读顺序

### 对于最终用户
1. [Lingma 用户指南](./lingma-user-guide.md) - 完整阅读
2. [Lingma 用户指南](./lingma-user-guide.md) 第四章 - 高级功能

### 对于应用开发者
1. [Lingma 用户指南](./lingma-user-guide.md) - 快速浏览
2. [Lingma 开发扩展指南](./lingma-development-guide.md) - 重点阅读
3. [MCP 使用指南](../mcp/mcp-user-guide.md) - 了解 MCP 配置

### 对于系统工程师
1. [Lingma 开发扩展指南](./lingma-development-guide.md) - 完整阅读
2. [Agent 对接 MCP 指南](./mcp-integration-guide.md) - 深入阅读
3. [MCP 开发指南](../mcp/mcp-development-guide.md) - 参考实现

### 对于架构师
1. [Agent 对接 MCP 指南](./mcp-integration-guide.md) - 架构设计
2. [Lingma 开发扩展指南](./lingma-development-guide.md) - 技术选型
3. [MCP 开发指南](../mcp/mcp-development-guide.md) - 生态了解

---

## 📊 文档对比

| 文档 | 目标读者 | 核心内容 | 技术深度 |
|------|---------|---------|---------|
| **用户指南** | 最终用户 | 使用方法、最佳实践 | ⭐ |
| **开发指南** | 应用开发者 | 配置、扩展、调试 | ⭐⭐⭐ |
| **MCP 集成** | Agent 开发者 | Agent 架构、工具匹配 | ⭐⭐⭐⭐⭐ |
| **MCP 开发** | MCP 开发者 | MCP 服务开发 | ⭐⭐⭐⭐⭐ |

---

## 🔗 相关文档

### MCP 文档系列
- [MCP 使用指南](../mcp/mcp-user-guide.md) - 如何配置和使用 MCP
- [MCP 开发指南](../mcp/mcp-development-guide.md) - 如何开发 MCP 服务
- [MCP 执行结果](../mcp/mcp-execution-results.md) - 实际执行示例

### 项目文档
- [One AI 项目介绍](../../README.md)
- [任务管理系统](../../ai-workspace/README.md)

---

## 📝 更新记录

- **2026-03-07**: 创建完整文档体系，拆分为用户指南和开发指南
- **2026-03-07**: 添加文档索引（本文档）
- **2026-03-07**: 更新能力分层架构说明

---

## 💡 使用建议

### 第一次使用？
1. 从 [Lingma 用户指南](./lingma-user-guide.md) 开始
2. 按照"快速开始"章节操作
3. 遇到问题查看"常见问题"

### 需要开发集成？
1. 阅读 [Lingma 开发扩展指南](./lingma-development-guide.md)
2. 参考"配置和部署"章节
3. 查看"扩展开发"示例

### 开发 Agent 系统？
1. 精读 [Agent 对接 MCP 指南](./mcp-integration-guide.md)
2. 参考完整的代码示例
3. 结合 [MCP 开发指南](../mcp/mcp-development-guide.md) 了解生态

---

**文档维护:** One AI Team  
**最后更新:** 2026-03-07  
**反馈建议:** support@lingma.ai
