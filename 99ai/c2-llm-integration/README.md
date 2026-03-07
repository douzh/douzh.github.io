# c2 LLM 集成模块

本模块涵盖大模型集成的核心技术，包括提示工程、Function Calling、上下文管理等。

## 📁 文档结构

```
c2-llm-integration/
├── README.md                      # 本文档
├── c2-1-prompt-engineering.md     # 提示工程实践
├── c2-2-function-calling.md       # Function Calling / Tool Calling
├── c2-3-context-window-management.md # 上下文窗口管理
├── c2-4-token-optimization.md     # Token 优化策略
└── c2-5-multi-model-switching.md  # 多模型切换方案
```

## 📚 文档列表

| 文档 | 核心内容 | 难度 |
|------|---------|------|
| **[提示工程实践](./c2-1-prompt-engineering.md)** | Prompt 设计模式、Few-shot、CoT | ⭐⭐ |
| **[Function Calling](./c2-2-function-calling.md)** | 工具调用、参数提取、错误处理 | ⭐⭐⭐ |
| **[上下文窗口管理](./c2-3-context-window-management.md)** | 记忆压缩、摘要生成 | ⭐⭐⭐ |
| **[Token 优化策略](./c2-4-token-optimization.md)** | 降低成本、提升速度 | ⭐⭐ |
| **[多模型切换方案](./c2-5-multi-model-switching.md)** | 路由策略、降级方案 | ⭐⭐⭐⭐ |

## 🎯 快速开始

### 想提升 Prompt 质量？
→ 查看 [提示工程实践](./c2-1-prompt-engineering.md)

### 想让 AI 调用工具？
→ 查看 [Function Calling](./c2-2-function-calling.md)

### 想降低 Token 成本？
→ 查看 [Token 优化策略](./c2-4-token-optimization.md)

## 🔗 相关模块

- **核心架构** → [c1 模块](../c1-core-arch/README.md) - Agent Loop 实现
- **记忆系统** → [c3 模块](../c3-memory-system/README.md) - 长期记忆存储
- **技能生态** → [c7 模块](../c7-skill-ecosystem/README.md) - MCP 协议

---

**最后更新:** 2026-03-07  
**模块状态:** ✅ 全部完成 (5/5)  
**总行数:** 3,087 行  
**维护者:** One AI Team
