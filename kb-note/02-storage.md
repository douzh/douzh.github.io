# 笔记工具档案:存储方式对比

[返回索引](note-soft.md) | 其他维度:[笔记功能](01-note-features.md) | [链接功能](03-link-features.md) | [MCP 支持](04-mcp-support.md) | [便携支持](05-portable.md)

覆盖工具:Trilium Notes / lapisnote / Obsidian / Logseq / 思源笔记

---

## 一、总览对比

| 维度 | Trilium Notes | lapisnote | Obsidian | Logseq | 思源笔记 |
| --- | --- | --- | --- | --- | --- |
| **存储形态** | 单个 SQLite 文件 | 单个 SQLite 文件 | 纯 Markdown 文件夹 | 纯 Markdown/Org 文件夹 | SQLite + .sy(JSON)文件 |
| **主文件** | `document.db` | SQLite 库(自定义路径) | `*.md` 每笔记一文件 | `journals/*.md` 等每页一文件 | `*.sy` 块树 + 索引 DB |
| **数据目录** | 程序目录 `trilium-data/` | `%APPDATA%/com.jdnotes.app/`(可自定义) | 自选任意文件夹 | 自选任意文件夹 | workspace 目录(可自选) |
| **是否纯文本** | ❌ SQLite 二进制 | ❌ SQLite 二进制 | ✅ 纯文本 | ✅ 纯文本 | ❌ JSON(半结构化) |
| **备份方式** | 复制单个 `document.db` | 复制 SQLite 文件 | 复制文件夹 / Git | 复制文件夹 / Git | 复制 workspace 文件夹 |
| **Git 版本管理** | ❌(二进制无法 diff) | ❌ | ✅ 每次修改可 diff | ✅ 每次修改可 diff | ⚠️ JSON 可 diff 但难读 |
| **迁移到其他工具** | 导出 MD/OPML(有损) | 导出 MD | ✅ 直接就是 MD,零成本 | ✅ 直接就是 MD | 导出 MD(转换) |
| **平台锁定风险** | 中(需导出) | 中(需导出) | 无 | 无 | 中(块结构转换有损) |
| **索引机制** | SQLite 查询 | SQLite 查询 | 实时扫描文件 + 缓存 | 启动时加载进内存(Datascript 图数据库) | 本地索引 SQLite |
| **附件存储** | 入库 document.db | 入库 SQLite | 文件夹内(可选入库方式) | 文件夹内 assets | workspace assets 文件夹 |
| **十万级体量** | ✅ 数据库优化 | 未验证 | ⚠️ 文件多时启动变慢 | ⚠️ 内存索引,启动慢 | ✅ 数据库优化 |

---

## 二、存储范式分析

五款工具正好覆盖四种存储范式:

```
范式一:单文件数据库(SQLite)     → Trilium Notes / lapisnote
范式二:纯文本文件文件夹          → Obsidian / Logseq
范式三:自有格式 + 数据库混合     → 思源笔记
```

### 2.1 范式一:单文件数据库(Trilium Notes / lapisnote)

**机制**:所有笔记、附件、配置、版本历史存于一个 SQLite 数据库文件。

**优点**:
- 备份极简:复制一个文件即全部备份
- 数据一致性强:事务保证,不会出现半写状态
- 大数据量性能好:数据库索引优化,十万级笔记无压力
- 结构化查询:可 SQL 直接查询(配合 [MCP](04-mcp-support.md) 时 AI 可精确读写)

**缺点**:
- 二进制文件:无法用 Git diff 看变化,无法用文本编辑器直接改
- 锁定风险:脱离工具后数据不可直接读,需依赖导出功能
- 单点故障:文件损坏 = 全库风险(需勤备份)
- 搜索/编辑必须通过应用本身或 API

### 2.2 范式二:纯文本文件夹(Obsidian / Logseq)

**机制**:每个笔记是一个 `.md` 文件,目录结构即组织结构,工具只做编辑器+索引器,不管存储。

**优点**:
- **数据主权终极形态**:任何文本编辑器都能打开,百年后仍可读
- **Git 友好**:每次修改可 diff 可回滚,天然版本管理
- **工具无关**:换任何 Markdown 工具零成本迁移
- **同步自由**:任意网盘/Git/Syncthing 自由组合
- 文件系统级搜索(ripgrep/grep)可直接用

**缺点**:
- 海量小文件:数万笔记后启动扫描变慢
- 双链等元数据依赖工具索引(文件本身只是 `[[文本]]` 字面量)
- 一致性弱:并发写、同步冲突需自己处理
- 附件、配置、插件数据散落多处

### 2.3 范式三:自有格式+数据库混合(思源笔记)

**机制**:内容以 `.sy` 文件(JSON,块树结构)存于 workspace,本地另有 SQLite 索引加速;配置与数据分离。

**优点**:
- 块级结构完整保留(JSON 描述块树),功能上限高(块级双链、属性数据库)
- 有本地索引,大数据量流畅
- JSON 半开放:理论上可程序化解析

**缺点**:
- `.sy` 虽是文本但结构自有,人类直接阅读困难
- Git diff 可行但不可读
- 迁移依赖导出功能(MD 导出会损失块级信息/属性数据库)

---

## 三、各工具存储细节

### 3.1 Trilium Notes

```
trilium-data/
├── document.db      # 核心:所有笔记+附件+版本+配置,单 SQLite
├── backup/          # 自动定时备份
└── log/             # 日志
```

- **单文件哲学**:备份 = 复制 `document.db`,一行命令搞定
- **版本历史入库**:每笔记的修改历史存在同一 DB,空间换安全
- **加密**:保护模式下 `document.db` 整库加密
- **同步**:自建 Server 端做多设备同步(数据仍在各端本地 SQLite)

### 3.2 lapisnote

```
%APPDATA%/com.jdnotes.app/   # 默认,可在设置中改到任意路径(如 U 盘)
└── notes.db                  # SQLite 主库(推断命名)
```

- SQLite 存储,路径可自定义——**改到 U 盘目录即实现数据便携**(程序本身另装)
- 有废纸篓(软删除标记),数据在库内可恢复
- MCP Server 直接暴露库的读写能力(详见 [MCP 支持](04-mcp-support.md))

### 3.3 Obsidian

```
MyVault/                 # 任意位置的"库"文件夹
├── note1.md             # 每篇笔记一个文件
├── folder/note2.md      # 文件夹即目录树
├── attachments/         # 附件(可配置入库/独立/当前文件夹)
└── .obsidian/           # 配置:插件、主题、工作区(可 Git 管理或忽略)
```

- **一个文件夹就是一个库**,库可任意复制/搬迁/Git
- `[[链接]]` 只是文本字面量,索引在启动时构建,删掉 `.obsidian` 数据无伤
- 多库并用:每个文件夹一个库,切换库即切换场景

### 3.4 Logseq

```
my-graph/                # 一个"图谱" = 一个文件夹
├── journals/            # 日记页(按日期一页一文件)
│   ├── 2025_07_08.md
│   └── 2025_07_09.md
├── pages/               # 常规页面([[引用]]自动生成)
├── assets/              # 附件
├── logseq/              # 配置
└── *.edn                # 索引缓存(可删,启动重建)
```

- Markdown/Org-mode 双格式支持
- **索引机制特殊**:启动时把全部文本加载进内存中的 Datascript(图数据库),缓存文件只加速启动——超大库内存与启动时间是瓶颈
- 数据主权与 Obsidian 同级:纯文本,永久可读

### 3.5 思源笔记

```
SiYuanWorkspace/         # 工作空间(可自选位置)
├── data/                # 用户数据核心
│   ├── notebook1/
│   │   ├── doc.sy       # 文档块树(JSON)
│   │   └── assets/      # 附件
│   └── notebook2/
└── conf/                # 配置
(另有本地 SQLite 索引,可重建)
```

- `.sy` 是 JSON 格式的块树:一个文档一棵块树,块有 ID,支撑块级双链/引用/属性
- 索引丢失可重建(从 .sy 恢复)
- 同步官方 S3/WebDAV 可选,也支持纯手动同步 workspace

---

## 四、与"U 盘便携 + 数据主权"需求的匹配

| 需求 | 最佳匹配 | 说明 |
| --- | --- | --- |
| 备份最简单 | **Trilium** | 复制单文件 `document.db` |
| 数据永久可读 | **Obsidian / Logseq** | 纯 Markdown,百年可读 |
| Git 版本管理 | **Obsidian / Logseq** | 文本 diff |
| 程序+数据全上U盘 | **Trilium** | Portable 包 + 数据在程序目录 |
| 数据上U盘、程序留机器 | **lapisnote / Obsidian / Logseq / 思源** | 存储/库路径均可自选 |
| AI 精确读写(SQL 级) | **Trilium / lapisnote** | SQLite 结构化查询 |
| 百年后迁移成本最低 | **Obsidian / Logseq** | 零转换 |

---

## 五、一句话总结

> **五款工具覆盖三种存储范式:Trilium Notes 与 lapisnote 是"单文件 SQLite"(备份=复制一个文件,数据库性能强,AI 可 SQL 级精确读写,但二进制不可 diff、有锁定风险);Obsidian 与 Logseq 是"纯 Markdown 文件夹"(数据主权终极形态,Git 友好,零迁移成本,任何编辑器可读,但海量文件启动慢);思源笔记是"自有 .sy(JSON)+ SQLite 索引"混合(块级结构完整、功能上限高,但迁移依赖导出、半锁定)。选型口诀:要备份极简和数据库性能选 SQLite 系(Trilium/lapisnote),要数据永久主权选纯文本系(Obsidian/Logseq),要块级功能上限选思源。U 盘便携需求下:Trilium 官方 Portable+程序目录存储一体化最优,其余四款均可通过自定义数据路径实现数据便携。**
