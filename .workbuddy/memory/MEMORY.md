# MEMORY.md — 项目长期约定

## 用户

- **方向**：投资研究 + 量化交易。已有「投资」（政策/概念/行业/产业链/交易体系）与「量化」（因子/回测策略）两个领域目录
- **偏好**：方案要可执行、有步骤；喜欢分层的表格与性质对比；对本体论/知识表示有系统学习（kb-krr 19+ 篇自成体系）
- **知识库双轨**：本地 markdown 仓 `kb-*` 系列（kb-krr / kb-km / kb-note…）+ Trilium 实例（实际笔记载体）

## Trilium 组织约定（2026-09-12 确立）

**核心原则：物理位置只回答「属于哪个工作领域」，不回答「是什么主题」。**

| 维度 | 载体 | 规则 |
| --- | --- | --- |
| 领域 | 目录树 | 不深于两层；新领域等第一篇笔记再建，不预建空目录 |
| 形态 | `#type` | permanent / literature / fleeting |
| 个体类型 | `#kind` | theorem / person / concept / law… |
| 主题 | `#学科码` + `~subject` | **绝不为学科建目录**（会与 3533 节点的学科树重复） |
| 关联 | 12 个核心关系 | relatesTo / isA / partOf / instanceOf / oppositeOf / sameAs / supports / contradicts / refines / questions / derivesFrom / subject |

**文献笔记与永久笔记不分开存放** —— 按领域混放，用 `#type` 区分，靠 Saved Search 集中查看；
理由是分开会切断领域上下文（投资领域的文献笔记脱离投资目录）。**闪念笔记例外，必须独立收件箱**。

## Trilium 关键 ID

| 对象 | ID |
| --- | --- |
| TBOX（本体基础设施容器） | `Tbs2oilaEojl` |
| 📥 收件箱（挂 `#inbox`，全库唯一） | `lG440e0fP7RQ` |
| 📚 学科分类(GB/T 13745) | `QQiw2PWpLwps` |
| 🧩 卡片模板 | `z1jcqBHwoW1g` |
| 📝 永久笔记模板 | `JlJwRZkJQrad` |
| 📖 文献笔记模板 | `UW8WSGiLLUR9` |
| 📥 闪念笔记模板 | `gWFrSxA7a0DU` |
| 🔧 脚本 | `ztveaxgadzx7` |
| 🔧 04 学科码自动挂载 | `tvictWplVx6k` |
| 🔧 06 关系维护 | `pkRD3gS3YJb2` |
| 📊 学科视图 | `9CDKChURjufe` |
| 📋 关系维护报告 | `QCuUhsRwbGI7` |
| 投资（book） | `fQJOFlZmWzrw` |
| 📚 知识管理（book） | `zZW4XQEtb7SR` |
| 🌌 生命智慧文明（book，99ai/1life 导入目标） | `sl3LZiRWSUwc` |
| ├─ 本体论（book，kb-krr 导入目标） | `r544M2wUKEKE` |
| └─ 笔记软件（book，kb-note 导入目标） | `MjO5QCY0uHRU` |
| 量化（book） | `ziP9X31cQ6uQ` |

## Trilium MCP 的硬限制（反复踩过）

1. **`#run` / `~runOn*` 等可执行属性一律设不了**，报 `potentially dangerous and cannot be set by the LLM` → 触发器必须用户手动挂
2. **backend 脚本里 `require('fs')` 被封锁**（`child_process`/`net`/`os` 同理）。取数走 `fetch()` + 本地 HTTP 服务（`python3 -m http.server 8765 --bind 127.0.0.1`），异步逻辑需包进 async IIFE，日志进 Help → Show logs
3. `get_child_notes` 有时返回空，改用 `search_notes` + `note.parents.title = 'X'` 更可靠
4. 查询「最新笔记」时用户指南会占满 `dateModified` 榜，需用 `ancestorNoteId` 圈定范围
