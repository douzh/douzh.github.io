# c1 核心架构模块

本模块包含 AI智能体的核心架构设计文档，涵盖 Agent Loop、轮辐式架构、微内核设计等关键概念。

## 📁 文档结构

```
c1-core-arch/
├── README.md                      # 本文档
├── c1-1-agent-loop.md             # Agent Loop 设计模式
├── c1-2-hub-spoke-architecture.md # 轮辐式架构实现
├── c1-3-microkernel-vs-monolith.md# 微内核与单体架构对比
├── c1-4-plugin-system.md          # 插件化扩展机制
└── c1-5-four-layer-design.md      # 四层架构能力详解
```

## 📚 文档列表

| 文档 | 核心内容 | 难度 |
|------|---------|------|
| **[Agent Loop 设计模式](./c1-1-agent-loop.md)** | 感知→规划→行动→观察→反馈循环 | ⭐⭐⭐ |
| **[轮辐式架构实现](./c1-2-hub-spoke-architecture.md)** | 网关 + 智能体运行时架构 | ⭐⭐⭐⭐ |
| **[微内核与单体架构对比](./c1-3-microkernel-vs-monolith.md)** | 架构选型与权衡 | ⭐⭐⭐ |
| **[插件化扩展机制](./c1-4-plugin-system.md)** | 热插拔技能系统设计 | ⭐⭐⭐⭐ |
| **[四层架构能力详解](./c1-5-four-layer-design.md)** | 渠道适配、推理引擎、技能执行、记忆系统 | ⭐⭐ |

## 🎯 快速开始

### 想了解 Agent 如何工作？
→ 查看 [Agent Loop 设计模式](./c1-1-agent-loop.md)

### 想设计系统架构？
→ 查看 [轮辐式架构实现](./c1-2-hub-spoke-architecture.md)

### 想扩展系统功能？
→ 查看 [插件化扩展机制](./c1-4-plugin-system.md)

## 🔗 相关模块

- **LLM 集成** → [c2 模块](../c2-llm-integration/README.md) - 大模型推理引擎
- **记忆系统** → [c3 模块](../c3-memory-system/README.md) - 双模记忆管理
- **技能生态** → [c7 模块](../c7-skill-ecosystem/README.md) - 技能插件开发

---

**最后更新:** 2026-03-07  
**维护者:** One AI Team
