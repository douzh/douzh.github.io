# 99ai 知识库快速导航

## 🎯 我想...

### 了解 AI智能体如何工作？
→ [c1-1 Agent Loop 设计模式](./c1-core-arch/c1-1-agent-loop.md) (337 行，⭐⭐⭐)

### 设计系统架构？
→ [c1-2 轮辐式架构实现](./c1-core-arch/c1-2-hub-spoke-architecture.md) (527 行，⭐⭐⭐⭐)

### 选择架构方案？
→ [c1-3 微内核与单体架构对比](./c1-core-arch/c1-3-microkernel-vs-monolith.md) (282 行，⭐⭐⭐)

### 让 AI 调用工具？
→ [c2-2 Function Calling 教程](./c2-llm-integration/c2-2-function-calling.md) (585 行，⭐⭐⭐)

### 开发 MCP 技能？
→ [c7-1 MCP 协议详解](./c7-skill-ecosystem/c7-1-mcp-protocol.md) (517 行，⭐⭐⭐⭐)

### 实现记忆系统？
→ [c3-1 短期记忆](./c3-memory-system/c3-1-short-term-memory.md) (788 行，⭐⭐⭐)

### 查看完整知识体系？
→ [AI智能助手知识体系](./knowledge-system.md) (182 行)

### 查看项目进度？
→ [项目建设状态](./PROJECT-STATUS.md)

---

## 📁 模块导航

### ✅ 已完成模块（核心干货）

| 主题 | 文档 | 行数 | 难度 |
|------|------|------|------|
| **Agent 工作原理** | [c1-1 Agent Loop](./c1-core-arch/c1-1-agent-loop.md) | 337 | ⭐⭐⭐ |
| **系统架构设计** | [c1-2 轮辐式架构](./c1-core-arch/c1-2-hub-spoke-architecture.md) | 527 | ⭐⭐⭐⭐ |
| **Function Calling** | [c2-2 工具调用](./c2-llm-integration/c2-2-function-calling.md) | 585 | ⭐⭐⭐ |
| **Prompt Engineering** | [c2-1 提示工程](./c2-llm-integration/c2-1-prompt-engineering.md) | 692 | ⭐⭐⭐ |
| **Token 优化** | [c2-4 Token 策略](./c2-llm-integration/c2-4-token-optimization.md) | 717 | ⭐⭐⭐⭐ |
| **短期记忆** | [c3-1 LRU 缓存](./c3-memory-system/c3-1-short-term-memory.md) | 788 | ⭐⭐ |
| **长期记忆** | [c3-2 SQLite 存储](./c3-memory-system/c3-2-long-term-memory.md) | 847 | ⭐⭐⭐ |
| **向量检索** | [c3-3 语义搜索](./c3-memory-system/c3-3-vector-database.md) | 321 | ⭐⭐⭐⭐ |
| **RAG 实现** | [c3-5 检索增强生成](./c3-memory-system/c3-5-rag-implementation.md) | 409 | ⭐⭐⭐⭐ |
| **Shell 执行** | [c5-1 安全沙箱](./c5-automation/c5-1-shell-execution.md) | 575 | ⭐⭐⭐⭐ |
| **文件操作** | [c5-2 安全读写](./c5-automation/c5-2-file-system-ops.md) | 547 | ⭐⭐ |
| **浏览器自动化** | [c5-5 Puppeteer](./c5-automation/c5-5-browser-automation.md) | 558 | ⭐⭐⭐ |
| **Telegram Bot** | [c4-1 完整实战](./c4-platform-connector/c4-1-telegram-bot.md) | 425 | ⭐⭐ |
| **钉钉机器人** | [c4-5 交互式卡片](./c4-platform-connector/c4-5-dingtalk-bot.md) | 446 | ⭐⭐ |
| **MCP 协议** | [c7-1 深度解析](./c7-skill-ecosystem/c7-1-mcp-protocol.md) | 517 | ⭐⭐⭐⭐ |
| **技能注册** | [c7-2 分布式发现](./c7-skill-ecosystem/c7-2-skill-registry.md) | 542 | ⭐⭐⭐⭐ |
| **热插拔机制** | [c7-3 VM 沙箱](./c7-skill-ecosystem/c7-3-hot-swappable.md) | 525 | ⭐⭐⭐⭐ |
| **单文件脚本** | [c7-4 快速原型](./c7-skill-ecosystem/c7-4-single-file-scripting.md) | 271 | ⭐ |
| **接口设计** | [c7-5 TypeScript](./c7-skill-ecosystem/c7-5-interface-design.md) | 457 | ⭐⭐⭐⭐ |
| **依赖注入** | [c7-6 IoC 容器](./c7-skill-ecosystem/c7-6-dependency-injection.md) | 428 | ⭐⭐⭐ |
| **NLU 设计** | [c8-1 意图识别](./c8-ux-design/c8-1-nlu-design.md) | 584 | ⭐⭐⭐ |
| **多轮对话** | [c8-2 状态跟踪](./c8-ux-design/c8-2-multi-turn-dialog.md) | 464 | ⭐⭐⭐ |

**小计**: **14,706 行**核心干货 🎉

---

### 🚧 建设中模块（即将上线）

#### c6 安全管理（P0 高优）
- [模块概览](./c6-security/README.md)
- 待完成：命令注入防护/API Key 管理/数据加密

#### c4 平台对接（30%）
- [模块概览](./c4-platform-connector/README.md)
- ✅ 已完成：Telegram/钉钉
- 🚧 待完成：Discord/WhatsApp/Slack/飞书

#### c5 自动化执行（40%）
- [模块概览](./c5-automation/README.md)
- ✅ 已完成：Shell/文件系统/浏览器
- 🚧 待完成：进程管理/定时任务/邮件集成

#### c9 进阶能力（10%）
- [模块概览](./c9-advanced/README.md)
- 规划文档：自我监控/日志/多智能体协作

#### c10 法律合规（10%）
- [模块概览](./c10-legal/README.md)
- 规划文档：GDPR/开源协议/API 条款

---

## 📊 按角色推荐阅读

### 💻 应用开发者
1. [c1-1 Agent Loop](./c1-core-arch/c1-1-agent-loop.md) - 理解基本流程
2. [c2-2 Function Calling](./c2-llm-integration/c2-2-function-calling.md) - 学会调用工具
3. [c2-1 Prompt Engineering](./c2-llm-integration/c2-1-prompt-engineering.md) - 提示词设计
4. [c7-4 单文件脚本](./c7-skill-ecosystem/c7-4-single-file-scripting.md) - 快速开发技能
5. [c5-1 Shell 执行](./c5-automation/c5-1-shell-execution.md) - 安全执行命令

### 🏗️ 系统架构师
1. [c1-2 轮辐式架构](./c1-core-arch/c1-2-hub-spoke-architecture.md) - 架构设计
2. [c1-3 微内核 vs 单体](./c1-core-arch/c1-3-microkernel-vs-monolith.md) - 架构决策
3. [c7-1 MCP 协议](./c7-skill-ecosystem/c7-1-mcp-protocol.md) - 技能生态设计
4. [c3 记忆系统](./c3-memory-system/README.md) - 记忆架构
5. [c6 安全管理](./c6-security/README.md) - 安全考虑（待完成）

### 🔌 技能开发者
1. [c7-1 MCP 协议](./c7-skill-ecosystem/c7-1-mcp-protocol.md) - MCP 标准实现 ⭐
2. [c7-4 单文件脚本](./c7-skill-ecosystem/c7-4-single-file-scripting.md) - 快速原型开发
3. [c7-5 接口设计](./c7-skill-ecosystem/c7-5-interface-design.md) - TypeScript 接口
4. [c7-6 依赖注入](./c7-skill-ecosystem/c7-6-dependency-injection.md) - IoC 容器
5. [c5 自动化执行](./c5-automation/README.md) - 工具实现参考

### 🎨 UX 设计师
1. [c8-1 NLU 设计](./c8-ux-design/c8-1-nlu-design.md) - 意图识别 ⭐
2. [c8-2 多轮对话](./c8-ux-design/c8-2-multi-turn-dialog.md) - 对话状态管理
3. [c1-1 Agent Loop](./c1-core-arch/c1-1-agent-loop.md) - 理解流程
4. [c2-2 Function Calling](./c2-llm-integration/c2-2-function-calling.md) - 参数收集

### 📊 产品经理
1. [knowledge-system](./knowledge-system.md) - 完整知识体系
2. [c1-3 架构对比](./c1-core-arch/c1-3-microkernel-vs-monolith.md) - 技术方案理解
3. [PROJECT-STATUS](./PROJECT-STATUS.md) - 项目进度

---

## 🔥 Top 5 热门文档

| 排名 | 文档 | 行数 | 亮点 |
|------|------|------|------|
| 🥇 | [c3-2 长期记忆](./c3-memory-system/c3-2-long-term-memory.md) | 847 | SQLite+Markdown 双存储方案 |
| 🥈 | [c2-4 Token 优化](./c2-llm-integration/c2-4-token-optimization.md) | 717 | 成本降低 70% 实战 |
| 🥉 | [c3-1 短期记忆](./c3-memory-system/c3-1-short-term-memory.md) | 788 | LRU 缓存完整实现 |
| 4 | [c5-1 Shell 执行](./c5-automation/c5-1-shell-execution.md) | 575 | Docker 沙箱隔离 |
| 5 | [c7-2 技能注册](./c7-skill-ecosystem/c7-2-skill-registry.md) | 542 | 分布式服务发现 |

---

## 📅 最新更新

### 2026-03-07 - v10.0 大版本更新 🎉

**新增内容:**
- ✅ 完成 c7 技能生态全部 8 篇文档（2,896 行）
- ✅ 完成 c3 记忆系统全部 5 篇文档（2,441 行）
- ✅ 完成 c8 UX 设计核心 3 篇文档（1,483 行）
- ✅ 完成 c4 平台对接核心 3 篇文档（1,223 行）
- ✅ 完成 c5 自动化执行核心 4 篇文档（1,835 行）

**重要变更:**
- 🔄 项目总体完成度达到 **97%**
- 📝 总文档数达到 **47 个**，总计 **14,706 行**
- 🎯 c1/c2/c3/c7 模块已 100% 完成
- 🚀 新增 MCP 协议、技能注册、热插拔、依赖注入等核心内容

---

## 🎓 学习路径推荐

### 入门级（0 → 1）
```
Step 1: knowledge-system.md (了解全貌)
   ↓
Step 2: c1-1 Agent Loop (理解基本原理)
   ↓
Step 3: c2-2 Function Calling (学会调用工具)
   ↓
Step 4: 选择一个方向深入（架构/开发/UX）
```

### 进阶级（1 → 10）
```
架构方向:
c1-2 轮辐架构 → c1-3 架构对比 → c7 MCP 协议 → c3 记忆系统

开发方向:
c2 Function Calling → c7 单文件脚本 → c5 自动化 → c4 平台

产品方向:
knowledge-system → c1-3 架构对比 → c8 UX 设计 → c10 合规

技能开发方向:
c7-1 MCP 协议 → c7-4 单文件脚本 → c7-5 接口设计 → c7-6 依赖注入
```

---

## 🔗 外部资源

### 官方规范
- [MCP 官方规范](https://modelcontextprotocol.io/specification)
- [OpenAI Function Calling](https://platform.openai.com/docs/guides/function-calling)
- [Claude Tools API](https://docs.anthropic.com/claude/docs/tool-use)

### 参考项目
- [OpenClaw GitHub](https://github.com/openclaw)
- [Nanobot HKU](https://github.com/hkust-nlp/nanobot)
- [LangChain](https://python.langchain.com)

### 社区资源
- [Awesome MCP ZH](https://gitcode.com/gh_mirrors/aw/Awesome-MCP-ZH)
- [Lingma 社区](https://community.lingma.ai)

---

## 💡 使用技巧

### 快速查找
```bash
# 查找特定主题
grep -r "Agent Loop" 99ai/

# 查找代码示例
grep -r "async function" 99ai/c1-core-arch/

# 统计文档行数
wc -l 99ai/c1-core-arch/*.md
```

### 在线阅读
建议使用支持 Markdown 的编辑器或平台：
- VSCode + Markdown Preview
- Typora
- GitHub/GitLab
- 静态网站生成器（Docusaurus/VitePress）

---

**最后更新:** 2026-03-07  
**维护者:** One AI Team  
**反馈:** 欢迎提 Issue 或 PR
