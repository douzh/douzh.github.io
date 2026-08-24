# 笔记软件总览

每个软件独立目录建档,本页为总览与选型速查。

## 目录

| 目录 | 软件 | 一句话定位 |
| --- | --- | --- |
| [01-trilium/](01-trilium/README.md) | Trilium Notes | 可编程树状知识库——唯一官方全便携 + 唯一关系属性(L5 三元组);v0.104+ 内置 AI + MCP |
| [02-lapisnote/](02-lapisnote/README.md) | lapisnote | AI 原生轻量新锐——唯一零配置原生 MCP(自动注册 Claude Code) |
| [03-obsidian/](03-obsidian/README.md) | Obsidian | 插件生态之王(2000+)——纯 Markdown 数据主权最强 |
| [04-logseq/](04-logseq/README.md) | Logseq | 大纲块级记录流——Journal 日记流 + 块引用 + Datalog,开源 |
| [05-siyuan/](05-siyuan/README.md) | 思源笔记 | 内置功能最全 All-in-one——属性数据库 + 闪卡 + 多端 + Docker |
| [06-affine/](06-affine/README.md) | AFFiNE | Notion+Miro 融合——文档⇄白板双模式唯一开源方案,内置多模态 AI |
| [07-appflowy/](07-appflowy/README.md) | AppFlowy | Notion 像素级开源克隆——本地 SQLite + AI 会议纪要,全平台含移动原生 |
| [08-anytype/](08-anytype/README.md) | Anytype | 加密去中心化知识库——对象+关系建模(L4),E2EE + P2P,官方也读不了你的数据 |

---

## 五维度快速对比

### 笔记功能

| | Trilium | lapisnote | Obsidian | Logseq | 思源 |
| --- | --- | --- | --- | --- | --- |
| 编辑范式 | 树状+富文本/MD | Markdown | Markdown | 大纲块级 | 块级(MD 语法) |
| 双链 | ✅ | ❓待验证 | ✅ 强 | ✅ 强 | ✅ 块级 |
| 任务管理 | ⚠️ 脚本实现 | ✅ 提醒 | ⚠️ 插件 | ✅ 原生 | ✅ |
| 闪卡 | ⚠️ 脚本 | ❌ | ⚠️ 插件 | ⚠️ 插件 | ✅ **官方内置** |
| 数据库表格 | ⚠️ 属性轻量 | ❌ | ⚠️ 插件 Dataview | ⚠️ Datalog | ✅ **官方最强** |
| 插件生态 | 脚本+社区库 | 内置为主 | ✅ **2000+** | ✅ 市场 | ✅ 市场 |
| 移动端 | ❌ | ❌ | ✅ | ✅ | ✅ +Docker |

### 存储方式

| | Trilium | lapisnote | Obsidian | Logseq | 思源 |
| --- | --- | --- | --- | --- | --- |
| 范式 | **单 SQLite** | **单 SQLite** | **纯 MD 文件夹** | **纯 MD/Org 文件夹** | .sy(JSON)+SQLite 混合 |
| 备份 | 复制单文件 | 复制单文件 | 复制文件夹/Git | 复制文件夹/Git | 复制 workspace |
| Git diff | ❌ | ❌ | ✅ | ✅ | ⚠️ 不可读 |
| 迁移成本 | 导出(有损) | 导出 | **零成本** | **零成本** | 导出(有损) |
| 十万级 | ✅ | 未验证 | ⚠️ 启动慢 | ⚠️ 内存索引慢 | ✅ |

### 链接功能(分级:L1单向 → L5关系一等公民)

| | Trilium | lapisnote | Obsidian | Logseq | 思源 |
| --- | --- | --- | --- | --- | --- |
| 评级 | **L5** ⭐⭐⭐⭐⭐ | 待验证 ⭐ | L2-L3 ⭐⭐ | L3-L4 ⭐⭐⭐ | L4-L5 ⭐⭐⭐⭐ |
| 链接带属性 | ✅ **relation 原生** | ❓ | ❌(Dataview 补) | ⚠️ key::value 语法 | ✅ 块属性+数据库 |
| 三元组建模 | **最强**(可 SQL 查询) | ❓ | 弱 | 中(Datalog) | 强(数据库视图) |
| 块级引用 | ⚠️ 克隆形态 | ❓ | ✅ | ✅ 原生强 | ✅ **最精细** |

### MCP 与 AI 接入(分级:T0原生 → T2社区)

| | Trilium | lapisnote | Obsidian | Logseq | 思源 |
| --- | --- | --- | --- | --- | --- |
| 评级 | **T1+** | **T0** ⭐⭐⭐⭐⭐ | T1 | T2 | T1 |
| 官方 MCP | ✅ **官方 MCP Server**(腾讯云 MCP 广场上架) | ✅ **自动启动+注册** | ❌ | ❌ | ❌ |
| API 路线 | ETAPI(REST)+ 社区适配器 | MCP 直连 | REST 插件+适配器 | 社区适配器 | **官方 HTTP+SQL 最全** |
| 内置 AI | ✅ **可配 LLM;可复用 Claude Code 订阅**(v0.104+) | ✅ **多模型免费**(含 Ollama) | ⚠️ 插件 | ⚠️ 插件 | ✅ 付费 |
| 文件旁路 | ❌ 必须走 API | ❌ | ✅ | ✅ | ❌ |

> 注:Trilium v0.104+ 桌面版默认不对局域网开放 ETAPI 端口,使用 API/MCP 需在 Options → Security 开启。

### 便携支持

| | Trilium | lapisnote | Obsidian | Logseq | 思源 |
| --- | --- | --- | --- | --- | --- |
| 官方便携版 | ✅ **原生标杆** | ❌(包小易装) | ❌(社区绿色包) | ❌(社区绿色包) | ✅ 官方 zip |
| 数据目录自选 | ✅(默认随程序) | ✅ | ✅ 库=文件夹 | ✅ 图谱=文件夹 | ✅ workspace |
| 程序+数据 U 盘一体 | ✅ **官方原生** | ⚠️ 数据便携型 | ⚠️ 数据便携型 | ⚠️ 数据便携型 | ✅ 可行 |
| 数据脱离程序可读 | ❌ SQLite | ❌ SQLite | ✅ **纯文本** | ✅ **纯文本** | ❌ .sy |

### Notion 系替代(06-08)对比

| | AFFiNE | AppFlowy | Anytype |
| --- | --- | --- | --- |
| 编辑范式 | 块+文档⇄白板双模式 | 块编辑(Notion 手感) | 块+对象模型 |
| 数据库视图 | 表/看板/日历/画廊 | Grid/Board/Calendar | Sets 集合+过滤 |
| 内置 AI | ✅ **多模态最强** | ✅ 会议纪要/转录/Chat | ❌ 缺位 |
| 存储范式 | 本地 SQLite+CRDT | 本地 SQLite | **本地+E2EE 加密** |
| 同步 | P2P/自托管(PostgreSQL) | AppFlowy Cloud 自托管 | **P2P 无中心** |
| 链接评级 | L2-L3 | L3 | **L4**(关系一等公民) |
| MCP 评级 | T1(REST API) | T1(Cloud API) | T2(无公开 API) |
| 移动端 | PWA | ✅ **原生双端** | ✅ **原生双端** |
| 官方便携版 | ❌ | ❌ | ❌ |
| 数据可脱离程序读 | ❌ | ❌ | ❌(加密锁定) |
| 许可证 | AGPL-3.0 | AGPL-3.0 | 协议 MIT/客户端 source-available |

> 与 01-05 的本质分野:01-05 是 PKM 系(树/大纲/双链文本),06-08 是 Notion 系(块+数据库+对象)。两系唯一交集是"关系建模"——Trilium(L5,SQL 可查)> Anytype(L4,UI 层)> 思源(L4-L5)> 其余。

### 开源情况(全八款)

| | Trilium | lapisnote | Obsidian | Logseq | 思源 | AFFiNE | AppFlowy | Anytype |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 许可证 | AGPL-3.0 | AGPL-3.0 | ❌ **闭源** | AGPL-3.0 | AGPL-3.0 | AGPL-3.0 | AGPL-3.0 | 协议 MIT/客户端受限 |
| 开放范围 | ✅ 全部 | ✅ 全部 | 仅插件 API/主题 | ✅ 全部 | ✅ 全部(含移动端) | ⚠️ 核心开源,企业功能闭源 | ✅ 全部 | ⚠️ 半开源 |
| 治理主体 | TriliumNext 社区 | 个人开发者(极早期) | Dynalist Inc.(商业) | Logseq 公司 | 88250/B3log | TOEVERYTHING 公司 | AppFlowy-IO(YC) | **非营利协会** |
| 付费项 | 无(无官方云) | 无 | Sync/Publish 云 | Sync 云 | 云同步/云空间 | 官方云/AI | Cloud/AI 积分 | 备份空间 |
| 社区规模 | 30k+ ⭐ | 16 ⭐ | —(闭源) | 44.5k+ ⭐ | 45k+ ⭐ | 61.8k+ ⭐ | **67.5k+ ⭐** | 活跃 |

> 开源格局速读:**AGPL-3.0 是笔记工具的事实标准**(八款中五款采用,防白嫖式套壳又保证个人免费);Obsidian 走"闭源但纯文本无锁定"的特殊路线;Anytype 半开源但治理最纯粹(非营利协会)。个人日常使用下,闭源不影响 Obsidian 数据安全,但影响长期信任与可审计性。

---

## 选型速查

| 核心需求 | 推荐 |
| --- | --- |
| 程序+SQLite 全放 U 盘、免安装即插即用 | **Trilium** |
| MCP 接入 AI(Claude Code 直接读写、零配置) | **lapisnote**(Trilium 官方 MCP 次之) |
| 链接带属性(三元组/知识图谱建模) | **Trilium**(最强)/ 思源(次之) |
| AI 结构化读写三元组(检索+写入 relation) | **Trilium**(ETAPI/MCP) |
| 应用内 AI 聊天、回答沉淀为笔记 | **Trilium**(v0.104+,可复用 Claude Code 订阅) |
| 数据永不锁定、百年可读、Git 管理 | **Obsidian / Logseq** |
| 内置功能最全(数据库+闪卡+多端) | **思源** |
| 插件生态定制、长文写作 | **Obsidian** |
| 快速记录、大纲流、任务、开源隐私 | **Logseq** |
| AI 用 SQL 精确检索 | **思源** |
| 完全离线本地 AI | **lapisnote**(Ollama) |
| Notion 全功能替代(数据库+多视图) | **AppFlowy**(克隆度最高)/ AFFiNE |
| 白板头脑风暴与文档同源 | **AFFiNE**(Edgeless 双模式独有) |
| AI 会议纪要(实时转录+说话人) | **AppFlowy**(开源独一份) |
| 隐私敏感内容(官方也无法读取) | **Anytype**(E2EE+P2P) |
| 对象+关系的 Notion 式建模但去中心化 | **Anytype**(Trilium 的加密镜像) |
| 移动端原生体验(Notion 系) | **Anytype / AppFlowy** |

---

## 资源

在 github 搜:
- https://github.com/topics/knowledge-base
- https://github.com/topics/knowledge-management
