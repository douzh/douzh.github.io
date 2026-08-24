# Trilium 数据库表设计与存储逻辑

[返回 Trilium 档案](README.md) · [返回总览](../note-soft.md)

- 分析对象:`D:\devtools\TriliumNotes\trilium-data\document.db`(实测,SQLite)
- 数据库版本:TriliumNext 新版 schema(notes 与 blobs 分离,内容寻址)
- 实测体量:574 笔记 / 589 分支 / 1406 属性(1306 label + 100 relation)/ 153 内容块 / 9 版本 / 19 附件

---

## 一、整体架构:一张图看懂

Trilium 把"一篇笔记"拆成**四个独立实体**,各住一张表:

```
notes(笔记元数据)──blobId──► blobs(内容,内容寻址+去重)
      │                          ▲
      │                          ├──► attachments(附件,role+mime)
      │                          └──► revisions(历史版本快照)
      ├──noteId──► branches(树位置/克隆,一篇笔记可挂多个父)
      └──noteId──► attributes(label 标签 + relation 关系=三元组)

entity_changes(同步变更日志,所有实体的改动指纹)
options / etapi_tokens / recent_notes / sessions / user_data(支撑表)
```

**设计哲学:元数据与内容分离、位置与笔记分离、属性独立成表**——树结构、链接、属性全部是数据库记录而非正文文本,这就是"关系作为一等公民"的物理实现。

---

## 二、核心表设计

### 2.1 notes —— 笔记元数据(不含内容!)

```sql
CREATE TABLE notes (
    noteId          TEXT PRIMARY KEY,     -- 20位随机ID,笔记终身唯一
    title           TEXT NOT NULL,
    isProtected     INT DEFAULT 0,        -- 是否加密
    type            TEXT DEFAULT 'text',  -- 笔记类型
    mime            TEXT DEFAULT 'text/html',
    blobId          TEXT,                 -- ★ 指向内容块(与内容分离)
    isDeleted       INT DEFAULT 0,        -- ★ 软删除
    deleteId        TEXT,                 -- 删除批次标记
    dateCreated / dateModified / utcDateCreated / utcDateModified TEXT
);
-- 索引:title, type, blobId, isDeleted+utcDateModified 等
```

**要点**:
- `type` × `mime` 决定笔记形态,实测组合:

| type | mime | 用途 |
| --- | --- | --- |
| text | text/html | 富文本笔记(115) |
| doc | — | 文档(294,新版默认) |
| code | application/javascript;env=frontend 等 | 代码/脚本笔记(32) |
| mermaid | text/mermaid | 流程图(13) |
| book | — | 容器笔记(书/目录,30) |
| image / file | image/jpg 等 | 二进制载体(17) |
| canvas / mindMap / noteMap / render / webView / launcher / contentWidget | — | 特殊渲染类型 |

- 软删除:`isDeleted=1` 不物理删除,`deleteId` 标记删除批次,后台按 `utcDateScheduledForErasureSince` 延迟物理清除(可在回收站恢复)。

### 2.2 blobs —— 内容存储(内容寻址 + 去重)★

```sql
CREATE TABLE blobs (
    blobId             TEXT PRIMARY KEY,  -- ★ 内容哈希,非随机ID
    content            TEXT,              -- 正文/附件字节(加密笔记为密文)
    textRepresentation TEXT,              -- 纯文本表示(供全文检索)
    dateModified / utcDateModified TEXT
);
```

**核心机制:blobId = 内容指纹**(base64(SHA-1(content)) 截断)。

实测铁证:本库 574 篇笔记只对应 153 个 blob——其中 **438 篇空笔记共享同一个 blob**(`z4PhNX7vuL3xVChQ1m2A`,即空字符串的哈希):

```sql
SELECT blobId, COUNT(*) FROM notes GROUP BY blobId ORDER BY 2 DESC;
-- z4PhNX7vuL3xVChQ1m2A | 438   ← 一个blob服务438篇空笔记
```

**收益**:
- 内容去重(空笔记/克隆正文零冗余)
- notes / attachments / revisions **三类实体共用内容层**:一个 blob 可同时被笔记正文、附件、历史版本引用
- `textRepresentation` 单独存纯文本,全文检索不必解析 HTML

### 2.3 branches —— 树结构与克隆(位置即数据)★

```sql
CREATE TABLE branches (
    branchId       TEXT PRIMARY KEY,  -- 格式:{parentNoteId}_{noteId}
    noteId         TEXT NOT NULL,     -- 子笔记
    parentNoteId   TEXT NOT NULL,     -- 父笔记
    notePosition   INTEGER,           -- 排序
    prefix         TEXT,              -- 该位置的前缀标签
    isExpanded     INTEGER,           -- 树折叠状态也入库
    isDeleted      INT DEFAULT 0,
    utcDateModified TEXT
);
```

**关键设计**:
- 笔记的"位置"不是笔记的属性,而是独立的 branch 记录——**一篇笔记多个 branch = 克隆**(同一内容出现在树中多处,修改任一处全局同步)
- 实测:本库 11 篇笔记被克隆(589 branches > 574 notes),branchId 直接用 `父ID_子ID` 命名

```
A9DSPSH2ewrm|k9heq8jMJaTi_A9DSPSH2ewrm|k9heq8jMJaTi|20   ← 克隆1
A9DSPSH2ewrm|uluxcf3iHb3K_A9DSPSH2ewrm|uluxcf3iHb3K|10   ← 克隆2
```

- 树的根:`root`(可创建/克隆给多用户)、`_hidden`(系统区)、`_share`(对外共享)、`_lbBookmarks`(启动栏)、`_search`(搜索历史)

### 2.4 attributes —— 标签与关系(三元组表)★★★

```sql
CREATE TABLE attributes (
    attributeId    TEXT PRIMARY KEY,
    noteId         TEXT NOT NULL,     -- 属性挂靠的笔记(三元组主体)
    type           TEXT NOT NULL,     -- 'label'(标量) | 'relation'(指向笔记)
    name           TEXT NOT NULL,     -- 属性名(三元组谓词)
    value          TEXT NOT NULL,     -- label=字符串 | relation=目标noteId(客体)
    position       INT DEFAULT 0,
    isInheritable  INT DEFAULT 0,     -- ★ 子树继承
    isDeleted      INT DEFAULT 0,
    utcDateModified TEXT
);
-- 索引:name+value / noteId / value(为图查询优化)
```

**一行 = 一条三元组**:
- label:`(noteId, name, value)` → (狂人日记, 发表年份, 1918)
- relation:`(noteId, name, 目标noteId)` → (狂人日记, 作者, 鲁迅)

**实测:语义知识库 Demo 的三元组全貌**(本库真实数据):

| 主体 | 谓词 | 客体 | type |
| --- | --- | --- | --- |
| 狂人日记 | 实体类型 | 作品 | label |
| 狂人日记 | 作品体裁 | 短篇小说 | label |
| 狂人日记 | 发表年份 | 1918 | label |
| 狂人日记 | 作者 | 鲁迅 | **relation** |
| 狂人日记 | 发表期刊 | 新青年 | **relation** |
| 鲁迅 | 实体类型 | 人物 | label |
| 鲁迅 | 出生日期 | 1881-09-25 | label |
| 鲁迅 | 职业 | 文学家,思想家 | label |
| 鲁迅 | 代表作品 | 狂人日记 | **relation** |
| 新青年 | 实体类型 | 期刊 | label |
| 新青年 | 创刊年份 | 1915 | label |
| 新青年 | 主要撰稿人 | 鲁迅 | **relation** |
| 新青年 | 收录作品 | 狂人日记 | **relation** |

**系统保留属性名**(实测存在):`template`(模板)、`target`、`widget`、`renderNote`、`run`、`runOnAttributeChange`、`dateTemplate`、`child:template`、`internalLink`(见下节)、`cssClass` / `iconClass` / `docName`(界面标签)。带 `~` 前缀为"否决/特殊"变体(如 `~run` 禁用脚本)。

**isInheritable=1**:属性沿子树向下继承(如 日记模板的 `startDate` 让全部子笔记自动获得),实测本库有 6 个继承属性。

---

## 三、链接的三层存储逻辑(重点)

Trilium 的链接不是一种,而是**三种机制并存**,层层递进:

### 第一层:正文锚点(HTML 文本内)

```html
<!-- 狂人日记 正文实际内容 -->
<a class="reference-link" href="#root/RxKvLuvqvSiA">鲁迅</a>
```

- 存在 blob 的 HTML 里,`href` 携带 notePath(`#root/noteId`)
- Markdown 语法 `[[noteId]]` / `[文字](#root/noteId)` 最终都渲染成这种锚点
- **弱点**:锚点本身是纯文本,目标被删后会变成 `[missing note]`(实测 Trilium Demo 笔记中已存在),无引用完整性

### 第二层:internalLink relation(自动同步的结构化记录)★

**在正文插入链接时,Trilium 自动额外写入一条 relation**——实测数据:

```
狂人日记 --internalLink--> 鲁迅        (对应正文锚点1)
狂人日记 --internalLink--> 新青年      (对应正文锚点2)
新建笔记 --internalLink--> 狂人日记    (对应 [[eEKn61sG6ybl]] 语法)
```

**意义**:正文里"看起来只是文本"的链接,**同时是 attributes 表里可查询的数据库记录**。反链面板、链接统计、"哪些笔记引用了我"全部直接查表,不用扫描 HTML。这是对"纯 Markdown 链接无元数据"这一痛点的数据库级解法。

### 第三层:语义 relation(用户自定义三元组)

`作者` / `发表期刊` / `代表作品` 这类带语义的关系,与 internalLink 同表同构——**普通跳转链接和知识图谱边在存储层完全统一**,统一用 SQL 查询:

```sql
-- 知识图谱查询:狂人日记的所有语义关联(含对方指向它的)
SELECT n1.title AS 主体, a.name AS 关系, n2.title AS 客体
FROM attributes a
JOIN notes n1 ON n1.noteId = a.noteId
JOIN notes n2 ON n2.noteId = a.value
WHERE a.type='relation' AND a.isDeleted=0
  AND (n1.title='狂人日记' OR n2.title='狂人日记');
```

```
狂人日记|作者|鲁迅
狂人日记|发表期刊|新青年
鲁迅|代表作品|狂人日记        ← 反向边也是一行数据
新青年|收录作品|狂人日记
```

### 三层对比

| | 存哪 | 带语义 | 可查询 | 引用完整性 |
| --- | --- | --- | --- | --- |
| 正文锚点 | blobs.content(HTML) | ❌ | ❌(需解析) | ❌ 目标删→死链 |
| internalLink relation | attributes 行 | 关系名固定 | ✅ SQL 直查 | ✅ 目标删→记录仍在可追溯 |
| 语义 relation | attributes 行 | ✅ 用户命名 | ✅ SQL 直查 | ✅ 同上 |

> **这正是 [README](README.md) 中"L5 关系一等公民"的落地证据**:链接(边)与笔记(点)同级,都是数据库记录;反向边天然存在(每条关系独立一行);图查询有专门索引(`IDX_attributes_value_index`)。

---

## 四、辅助表设计

### 4.1 attachments —— 附件

```sql
CREATE TABLE attachments (
    attachmentId TEXT PRIMARY KEY,
    ownerId      TEXT NOT NULL,   -- 挂靠的笔记(或属性)
    role         TEXT,            -- image / file / viewConfig(实测三种)
    mime         TEXT, title TEXT,
    blobId       TEXT,            -- ★ 内容同样走 blobs 共享层
    utcDateScheduledForErasureSince TEXT,  -- 计划物理清除时间
    ...
);
```

实测:图片 jpg/svg、canvas 导出 svg、mermaid 导出 svg 均为附件;**附件内容也存 blobs**(与笔记正文共用去重机制)。

**附件 100% 在库内,无外部文件**(与 Obsidian 的 `attachments/` 文件夹路线根本不同):

| typeof(blobs.content) | 含义 | 实测例 |
| --- | --- | --- |
| `blob` | 二进制字节直接入库 | codenames.jpg(7KB)、The Last Question.pdf(48KB)、woff2 字体、01.jpeg~03.jpeg |
| `text` | 文本型附件 | mermaid-export.svg、geoMap.json、chart.js |

本库 153 个 blob = 16 个二进制 + 137 个文本,`trilium-data/` 目录下除 `document.db` 外没有任何内容文件。**含大附件时备份就是复制一个 db 文件,但库体积会随附件线性膨胀**(SQLite 单值上限约 1GB,理论可存视频)。

### 4.2 revisions —— 版本历史

```sql
CREATE TABLE revisions (
    revisionId TEXT PRIMARY KEY,
    noteId TEXT, type TEXT, mime TEXT, title TEXT,
    description TEXT,
    source TEXT DEFAULT 'auto',   -- ★ auto(定时快照)/ llm(AI编辑)/ manual
    blobId TEXT,                  -- ★ 快照内容也走 blobs
    ... 时间戳×5
);
```

**实测亮点**:`source` 字段出现 **`llm`(5 条)**——内置 AI 每次编辑笔记自动留快照,与人工编辑(auto, 4 条)同一套版本机制可回滚。

### 4.3 entity_changes —— 同步引擎(日志表)

```sql
CREATE TABLE entity_changes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    entityName TEXT,   -- notes/branches/attributes/blobs/revisions/...
    entityId   TEXT,   -- 对应实体主键
    hash       TEXT,   -- 实体内容哈希(冲突检测)
    isErased   INT,
    changeId / componentId / instanceId TEXT,
    isSynced   INTEGER,
    utcDateChanged TEXT
);
```

- 所有实体的每次变更追加一行(实测 3017 行,attributes 1408 / options 262 / notes 574...),这是**多端同步与 ETAPI 增量拉取的物理基础**
- `hash` 用于同步双方比对,不一致即冲突

### 4.4 其余支撑表

| 表 | 用途 | 实测 |
| --- | --- | --- |
| options | 全局配置(name-value,`isSynced` 标记哪些配置跨端同步) | 262 行 |
| etapi_tokens | ETAPI 令牌(存哈希不存原文) | **本库有一枚名为 `mcp` 的 token——MCP 经 ETAPI 接入的直接证据** |
| recent_notes | 最近访问(含 notePath) | — |
| sessions | 服务端会话 | — |
| user_data | 用户与加密密钥派生材料 | — |

---

## 五、存储逻辑总结

### 5.1 一篇笔记的完整画像

```
用户看到:树中一篇《狂人日记》
物理存储:
  notes      1 行   (eEKn61sG6ybl, title=狂人日记, type=text, mime=text/html, blobId=...)
  blobs      1 行   (HTML 正文:<a href="#root/RxKvLuvqvSiA">鲁迅</a>...)
  branches   1+ 行  (父=现代文学语义知识库Demo, position, 若克隆则多行)
  attributes N 行   (3 label + 2 语义relation + 2 internalLink)
  revisions  0+ 行  (历史快照, 内容也指向 blobs)
  attachments 0+ 行 (图片等, 内容也指向 blobs)
  entity_changes 每次修改追加 1 行 (同步日志)
```

### 5.2 设计取舍

| 设计决策 | 收益 | 代价 |
| --- | --- | --- |
| 内容/元数据分离(notes↔blobs) | 内容寻址去重、三类实体共享存储 | 查一篇笔记需 join |
| 位置独立(branches) | 克隆零成本、树操作=增删行 | "笔记在哪"需查表 |
| 属性独立(attributes) | **三元组可 SQL 查询、关系带语义** | 属性多时 join 开销(有索引) |
| 软删除+延迟清除 | 回收站、可恢复 | 表膨胀(靠 erasure 任务回收) |
| entity_changes 日志 | 多端同步/增量 API 免全量比对 | 写放大(每次改动双写) |

### 5.3 对 AI/MCP 集成的意义

这套 schema 让 AI 拿到的是**结构化知识图谱而非一堆文本**:

```sql
-- 1. AI 检索知识子图:人物的全部关系
SELECT a.name, n2.title FROM attributes a
JOIN notes n2 ON n2.noteId=a.value
WHERE a.type='relation' AND a.isDeleted=0
  AND a.noteId=(SELECT noteId FROM notes WHERE title='鲁迅' AND isDeleted=0);
-- → 代表作品|狂人日记

-- 2. AI 写入新知识:一条 SQL 即一条三元组
INSERT INTO attributes (attributeId, noteId, type, name, value, ...)
VALUES (gen_random_id(), 'eEKn61sG6ybl', 'relation', '收录文集', '呐喊笔记ID', ...);

-- 3. 反向检索"谁引用了我"(含正文链接,因 internalLink 也是 relation)
SELECT n1.title FROM attributes a JOIN notes n1 ON n1.noteId=a.noteId
WHERE a.value='RxKvLuvqvSiA' AND a.type='relation' AND a.isDeleted=0;
```

正文锚点(第一层)负责"人看",internalLink + 语义 relation(第二三层)负责"机器查"——**同一链接,双轨存储,各取所需**。
