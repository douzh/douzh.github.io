# 笔记工具档案:MCP 与 AI 接入能力对比

[返回索引](note-soft.md) | 其他维度:[笔记功能](01-note-features.md) | [存储方式](02-storage.md) | [链接功能](03-link-features.md) | [便携支持](05-portable.md)

覆盖工具:Trilium Notes / lapisnote / Obsidian / Logseq / 思源笔记

> MCP(Model Context Protocol,Anthropic 2024 年发布)是 AI 应用的标准化上下文协议:应用暴露 MCP Server,AI 客户端(Claude Code / Claude Desktop / 其他兼容客户端)即可标准化地读写应用数据。笔记工具支持 MCP 后,**AI 可以直接检索、读取、创建、修改笔记**——笔记库变成 AI 的长期记忆与知识源。

---

## 一、总览对比

| MCP 维度 | Trilium Notes | lapisnote | Obsidian | Logseq | 思源笔记 |
| --- | --- | --- | --- | --- | --- |
| **官方 MCP Server** | ❌(REST API 生态) | ✅ **内置自动启动** | ❌(插件生态) | ❌(社区) | ⚠️(内置 AI 付费,API 开放) |
| **MCP 接入方式** | REST API + 第三方 MCP 适配器 | 启动即开 `127.0.0.1:19230` | Local REST API 插件 + mcp-obsidian 等适配 | 社区 MCP 适配器 | HTTP API(kernel) + 社区 MCP 适配器 |
| **配置难度** | 中(装适配器+配 token) | **零配置**(自动注册 Claude Code) | 中(装两个插件+配 key) | 中(社区项目) | 中(token + 适配器) |
| **AI 可操作能力** | 全量 CRUD(经 REST) | 直接读写笔记库 | CRUD + 搜索(经 REST 插件) | 读写页面/块(视适配器) | 全量 CRUD(官方 API 完整) |
| **内置 AI 助手** | ❌(脚本自建) | ✅ 多模型(DeepSeek/OpenAI/Claude/Gemini/Ollama) | ❌(插件:Copilot 等) | ❌(插件) | ✅ 官方 AI(付费订阅) |
| **本地模型支持** | 自建脚本 | ✅ Ollama | ✅ 插件 + Ollama | ✅ 插件 | ⚠️ 配置自定义 API |
| **API 类型** | REST(ETAPI) | MCP + (推断有 HTTP) | 插件 REST(Local REST API) | 社区适配 | 官方 HTTP API(127.0.0.1:6806,token) |
| **AI 检索笔记精度** | 高(SQL/搜索接口) | 高(SQLite/MCP) | 高(搜索插件接口) | 中-高 | 高(SQL 查询接口) |

---

## 二、各工具 MCP/AI 接入详述

### 2.1 lapisnote —— 唯一"零配置原生 MCP"

- **机制**:应用启动时自动在 `127.0.0.1:19230` 开启 MCP Server,**并自动注册到 Claude Code**——打开 Claude Code 就能直接读写笔记,无需任何手工配置
- **意义**:把 MCP 当一等公民设计(而非事后补丁),是目前五款中 MCP 集成度最高的:
  - AI 客户端 → MCP Server → SQLite 笔记库,链路最短
  - 天然适合"AI 代理直接管理笔记库"的工作流(自动归档、自动整理、对话式检索)
- **内置 AI 写作助手**(应用内,非 MCP):
  - 支持 DeepSeek / OpenAI / Claude / Gemini / **Ollama 本地模型**
  - 可同时配置多模型切换——国内网络环境友好(DeepSeek/Ollama 可离线)
- **定位**:AI 原生笔记工具的代表,MCP 优先场景的首选

### 2.2 Trilium Notes —— 成熟 REST API + 社区 MCP 适配

- **自有 API**:ETAPI(REST API),覆盖笔记 CRUD、搜索、附件、属性(含 relation 关系)——API 完整度高,是老牌"可编程笔记"
- **MCP 路径**:`社区 MCP 适配器` → 调 ETAPI → 笔记库
  - GitHub 上有多个 Trilium MCP server 项目(mcp-trilium 等)
  - 需要手工:启动适配器 + 配置 Trilium token + 注册到 AI 客户端
- **优势场景**:
  - **结构化读写**:ETAPI 直接操作属性/relation,AI 可查询三元组关系("列出作者=鲁迅的所有笔记")——配合 [关系属性系统](03-link-features.md),AI 能做知识图谱级检索
  - JS 脚本引擎可自建 AI 管道(调 OpenAI/Claude API 处理笔记,定时批处理)
- **短板**:无官方 MCP;配置门槛中等;需保持服务端运行

### 2.3 Obsidian —— 插件生态路线

- **官方无 MCP**,但插件生态提供两条路径:
  1. **Local REST API 插件**(社区经典):在本地开 REST 端点(可鉴权),暴露笔记 CRUD 与搜索接口
  2. **MCP 适配器**(如 mcp-obsidian / obsidian-mcp-tools):对接上述 REST 端点,包装成 MCP 工具给 Claude 等客户端
- **另一路**:**Smart Connections / Copilot for Obsidian** 等插件在应用内做 AI(语义检索、对话问答、补全),支持配置 OpenAI/Ollama 等——这是"AI 进笔记",与 MCP 的"AI 外部接管笔记"互补
- **配置难度**:中等——装 2 个组件(REST 插件 + MCP 适配器)、配 API key/token
- **优势**:生态最活跃,方案迭代快;笔记是纯 Markdown 文件,**AI 也可以不经 API 直接读写文件系统**(最简单的旁路方案)

### 2.4 Logseq —— 社区适配器路线

- **官方无 MCP**,社区有 logseq-mcp-server 等适配项目,基于 Logseq 的 HTTP API(需开启)或直接读 Markdown 文件夹
- **能做什么**(视适配器实现):页面读写、块级检索、属性查询(Datalog 能力可部分暴露给 AI)
- **另一路**:纯文本文件夹特性 → **AI 直接用文件系统工具读写**(与 Obsidian 同理,不经 API 的旁路)
- **成熟度**:社区项目,更新与稳定性依赖维护者,需实测验证

### 2.5 思源笔记 —— 官方完整 API + 内置 AI(付费)

- **官方 HTTP API**(kernel,默认 `127.0.0.1:6806`,token 鉴权):功能最完整的笔记 API 之一——块级 CRUD、SQL 查询、模板、导出、笔记本管理全覆盖
- **MCP 路径**:社区 siyuan-mcp-server 等适配器 → 官方 API → 笔记库
  - 因 API 强大,适配器能暴露的能力上限高(块级操作 + SQL 检索)
- **官方 AI 助手**:内置人工智能功能(写作、问答、块级 AI 操作),**付费订阅**(可自配 API Key 走自有模型)
- **优势**:SQL 查询接口让 AI 可精确检索(比全文检索精准);块级 API 与块级双链天然匹配
- **短板**:官方 AI 收费;MCP 需社区适配器;本地 API 需手动开启

---

## 三、MCP 集成成熟度分级

```
T0 原生内置(零配置)
    └─ lapisnote(自动启动 + 自动注册 Claude Code)

T1 官方 API + 成熟社区适配
    ├─ Trilium Notes(ETAPI 完整,适配器多)
    ├─ 思源笔记(API 最强,含 SQL 查询)
    └─ Obsidian(REST 插件 + 适配器,生态活跃)

T2 社区方案(可用但需验证)
    └─ Logseq(社区适配器 + 文件系统旁路)
```

---

## 四、典型 AI 工作流匹配

| 想要的工作流 | 推荐 | 说明 |
| --- | --- | --- |
| Claude Code 直接读写笔记库,零配置 | **lapisnote** | 唯一原生 MCP |
| AI 检索/写入知识三元组(图谱级) | **Trilium** | ETAPI 操作 relation 属性 |
| AI 用 SQL 精确查询块级数据 | **思源** | 官方 SQL API |
| AI 旁路直接改 Markdown 文件 | **Obsidian / Logseq** | 文件系统即接口,无需任何 API |
| 应用内 AI 写作(不依赖外部客户端) | **lapisnote(免费多模型)/ 思源(付费)** | 内置助手 |
| 完全离线的本地 AI | lapisnote(Ollama)/ Obsidian(插件+Ollama) | 本地模型 |
| 自动化批处理(定时 AI 整理笔记) | **Trilium** | JS 脚本引擎 + API |

---

## 五、一句话总结

> **MCP/AI 接入能力分三档:lapisnote 是 T0 唯一原生——启动即自动开 MCP Server(127.0.0.1:19230)并自动注册 Claude Code,零配置实现 AI 直接读写笔记库,另有内置免费多模型助手(DeepSeek/OpenAI/Claude/Gemini/Ollama);Trilium、思源、Obsidian 同属 T1(官方或插件 REST API + 社区 MCP 适配器)——Trilium 的 ETAPI 能让 AI 操作 relation 三元组,思源的官方 SQL API 提供最精确的块级检索,Obsidian 靠 Local REST API 插件 + mcp-obsidian 生态最活跃;Logseq 属 T2 社区方案。注意一个共性:纯文本系(Obsidian/Logseq)存在"文件系统旁路"——AI 无需任何 API 直接读写 Markdown 文件,是最简单的集成方式;而 SQLite 系(Trilium/lapisnote)反而必须走 API/MCP 才能安全读写。选型:AI 代理管理笔记选 lapisnote,AI 做知识图谱检索选 Trilium,AI 做 SQL 精确查询选思源,AI 全自动改文件选 Obsidian/Logseq。**
