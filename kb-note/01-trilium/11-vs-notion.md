# Trilium vs Notion 功能对标

[返回 Trilium 档案](README.md) | [Notion 档案](../10-notion/README.md) | [返回总览](../note-soft.md)

> 两款是笔记工具光谱的两端:Trilium 把一切握在本地换来最强自主,Notion 把一切交给云端换来最强协作。本页按档案维度逐项对标。

## 一、基本盘

| 维度 | Trilium | Notion | 胜者 |
| --- | --- | --- | --- |
| 形态 | 本地程序 + SQLite(可自建同步) | 纯云端 SaaS | — |
| 开源 | ✅ AGPL-3.0 全开源 | ❌ 闭源(API 开放) | Trilium |
| 定价 | 全免费(无官方云) | 免费版 + $10-20/人/月(AI 捆绑) | Trilium |
| 数据主权 | ✅ document.db 在自己手里 | ❌ 厂商服务器,导出有损 | Trilium |
| 便携(U 盘) | ✅ 官方原生标杆 | ❌ 无 | Trilium |

## 二、编辑与组织

| 维度 | Trilium | Notion | 胜者 |
| --- | --- | --- | --- |
| 编辑范式 | 树状无限层级 + 富文本/MD 双模式 | 块编辑(品类定义者)+ `/` 命令 | 各有千秋 |
| 组织结构 | 层级树 + **笔记克隆**(一内容多处引用) | 无限嵌套页面 + 团队空间 | 平手 |
| 结构化数据 | ⚠️ 属性(label/relation)+ 搜索/SQL,无多视图 UI | ✅ **数据库多视图**(表/看板/日历/画廊/时间线)+ Relation/Rollup | **Notion** |
| 双链 | ✅ 内部链接 + Linked Mentions | ✅ mentions + Linked Mentions | 平手 |
| 块级引用 | ⚠️ 克隆形态(整笔记) | ✅ 块/数据库行/同步块 | Notion |
| 模板 | ✅ 模板笔记 + Launcher | ✅ 数据库模板 + 按钮 | Notion |
| 任务管理 | ⚠️ 脚本实现 | ✅ 数据库原生 | Notion |
| 闪卡 | ⚠️ 脚本 | ⚠️ 第三方 | 平手(都弱) |

## 三、关系建模(两者各自的王牌)

| 维度 | Trilium | Notion |
| --- | --- | --- |
| 链接带属性 | ✅ **relation 原生**——任意笔记挂带名关系,三元组 `(笔记, 关系名, 目标)` | ⚠️ 仅数据库内 Relation(表间外键),正文链接不带属性 |
| 查询能力 | ✅ 搜索语法 + **SQL 直查**(全文/属性/关系) | ✅ 数据库 filter/sort,跨库查询需 Enterprise |
| 图谱可视化 | ✅ Relation Map 按关系类型渲染 | ❌ 无原生图谱 |
| 结论 | **L5 关系一等公民,知识图谱建模更强** | **L4,强在结构化业务数据(项目/CRM)** |

## 四、AI 与自动化(两者各有一条独特路线)

| 维度 | Trilium | Notion |
| --- | --- | --- |
| 内置 AI | ✅ v0.104+ 可配 LLM、**可复用 Claude Code 订阅** | ✅ 最成熟全家桶(写作/Q&A/Connectors/Agents/纪要) |
| MCP | ✅ 本地 MCP 端点 + ETAPI | ✅ **官方托管 MCP + OAuth**(Claude/ChatGPT 一键连) |
| 可编程性 | ✅ **五层**(前后端 JS/SQL/ETAPI)——笔记即程序 | ❌ 无脚本引擎,只能靠 API/Agents 外部编排 |
| 自动化 | 脚本定时任务、自定义渲染小应用 | 数据库自动化 + AI Agents |

## 五、协作与生态

| 维度 | Trilium | Notion | 胜者 |
| --- | --- | --- | --- |
| 多人协作 | ⚠️ 自建同步服务器(同步非协作) | ✅ 实时共编 + 权限 + 评论 | **Notion** |
| 移动端 | ❌(PWA/浏览器) | ✅ 全平台 | Notion |
| 插件生态 | 脚本 + 社区库(小而极客) | 官方集成生态 + API 应用 | 各有生态 |
| 离线 | ✅ 天然离线 | ⚠️ 有限 | Trilium |

## 六、总结

- **Trilium 强在"深度"**:数据在自己手里、关系是一等公民、笔记可编程——单机知识图谱/研究型个人库之王。
- **Notion 强在"广度"**:数据库多视图、协作权限、内置 AI 全家桶、托管 MCP——团队工作流之王。
- **互相做不到的**:Notion 给不了数据主权和脚本引擎;Trilium 给不了实时协作、移动端和成熟 AI 全家桶。
- **功能上最接近的取舍**:要"Notion 的结构化数据库 + Trilium 的本地开源",看[思源](../05-siyuan/README.md)(两者各占一半);要"Trilium 的关系建模 + Notion 的块库体验",看[Anytype](../08-anytype/README.md)。
