# Trilium 模板笔记详解

[返回 Trilium 档案](README.md) | [属性系统:标签/关系与定义](04-attributes.md) | [本体论 TBox/ABox 对应实现](06-ontology-tbox-abox.md) | [返回总览](../note-soft.md)

> 模板笔记是 Trilium 的"类定义"机制——它**本身就是一条普通笔记**,但通过挂 `~template` 关系或设为父节点,让其他笔记"套用"它,自动获得它声明的全部字段定义、初始内容和行为脚本。

## 一、核心概念

| | 普通笔记 | 模板笔记 |
| --- | --- | --- |
| 本质 | ABox 的一个个体 | TBox 的一个类(Class) |
| 内容 | 具体数据 | 字段定义 + 初始骨架 + 脚本 |
| 挂什么 | `#标签值`、`~关系实例` | `#label:xxx=type,promoted`、`#relation:xxx=promoted` |
| 被谁用 | 人读 | 其他笔记通过 `~template=@模板` 套用 |
| 数量 | 多 | 少(模式稳定) |

**三种"套用"方式**:

1. **`~template` 关系**:实例笔记挂 `~template=@模板名`——字段定义自动作用于实例
2. **`#template` 可继承标签**:父节点挂 `#template=@模板名`(isInheritable),所有子笔记自动套用
3. **`child:` 前缀**:`#child:template=@模板名` 仅对直接子节点生效(不含孙辈)

## 二、端到端示例:读书笔记模板

### 第 1 步:建模板笔记

新建一条笔记,标题"📖 书籍模板",挂上这些定义(写在属性面板或正文用 `#`/`~` 语法):

```
#label:书名=text,promoted
#label:作者名=text,promoted
#label:出版年=number,promoted
#label:genre=enum(小说,散文,诗歌,传记,其他),promoted,multi
#label:状态=enum(在读,读完,弃读,想读),promoted
#label:评分=number,promoted
#relation:作者关系=promoted
#relation:系列=promoted
```

再加一条内容骨架作为模板正文(套用时复制到新笔记):

```markdown
## 一句话总结

## 核心观点

## 摘录
- 

## 我的评论
```

可选:挂一个自动脚本(放在模板的子笔记里,`#runOnNoteCreation`):

```javascript
// 新建读书笔记时自动填入今天日期作为"开始阅读日"
module.exports = async ({ note }) => {
  note.setLabel('开始日', new Date().toISOString().slice(0, 10));
};
```

### 第 2 步:用模板建实例笔记

**方式 A — 主动挂 `~template`**:新建笔记《百年孤独》,在属性面板挂 `~template=@📖 书籍模板`。

效果立即发生:
- 顶部出现 8 个表单字段(书名/作者名/出版年/genre/状态/评分/作者关系/系列)
- enum 字段是下拉,number 是数字输入框,relation 是笔记选择器
- 正文自动填入模板的骨架内容(取决于版本,有的需手动 Ctrl+点击模板按钮)
- 触发 `runOnNoteCreation` 脚本,自动补 `#开始日=2026-08-24`

填完实际生成:

```
《百年孤独》
  ~template=<书籍模板 noteId>
  #书名=百年孤独
  #作者名=马尔克斯
  #出版年=1967
  #genre=小说
  #状态=在读
  #开始日=2026-08-24
  ~作者关系=<马尔克斯 noteId>
```

**方式 B — 模板按钮**:右键模板笔记 → "Create note from template" → 直接生成套好模板的子笔记。

**方式 C — 父节点继承**:建"📚 读书"笔记,挂 `#template=@📖 书籍模板`(勾 isInheritable)。之后在"读书"下新建任何子笔记,全部自动套用该模板——**这是最常用的组织方式**。

### 第 3 步:检索与视图

```sql
-- 所有"在读"的书籍
SELECT title FROM notes n JOIN attributes a ON n.noteId=a.noteId
WHERE a.name='状态' AND a.value='在读' AND a.isDeleted=0;

-- 按类型统计我读过的书
SELECT genre.value AS 类型, COUNT(*) AS 数量
FROM attributes t JOIN attributes genre
  ON t.noteId=genre.noteId
WHERE t.name='template' AND t.value='<书籍模板 noteId>'
  AND genre.name='genre' AND genre.isDeleted=0
GROUP BY genre.value;
```

**Collections 视图**(v0.104+):建一条笔记挂 `~template=@书籍模板` 的 Saved Search + `#view:collection=table`,promoted 字段自动成为表格列,排序/筛选即点即用。

## 三、模板的三层能力

| 层 | 模板能定义什么 | 对应 Trilium 机制 |
| --- | --- | --- |
| **L1 数据结构** | 字段名、类型、单值/多值、表单化 | `#label:xxx=type,promoted` / `#relation:xxx=promoted` |
| **L2 默认值与骨架** | 初始内容、默认标签 | 模板正文被复制 + 模板自身的标签(非定义式)被继承 |
| **L3 行为逻辑** | 新建/改/删时的自动化 | 子笔记挂 `#runOnNoteCreation`/`#runOnAttributeChange` 脚本 |

## 四、进阶用法

### 1. 模板继承链(类的子类化)

```
📖 媒介模板(顶层)
  ├── 📖 书籍模板(继承媒介模板,加 #页数=promoted)
  ├── 🎬 电影模板(继承媒介模板,加 #导演=promoted, #时长=number,promoted)
  └── 🎵 音乐模板(继承媒介模板,加 #专辑=boolean,promoted)
```

`#template=@媒介模板` 在书籍模板上勾可继承,则书籍模板的实例同时继承媒介模板的字段——**这是 Trilium 的"子类 extends 父类"**。

### 2. 多模板组合(混入 Mixin)

Trilium 不支持一个笔记挂两个 `~template`,但可以用**可继承标签**模拟:

```
《狂人日记》
  ~template=@书籍模板
  #pageCountTrackable  ← 父节点继承的 mixin 标签,带额外字段定义
```

实际工程中更简单:把多个字段定义放一条模板,需要时挂上去(模板的"多重继承"靠人工组合,不自动)。

### 3. 字段类型清单(标签定义可用)

| 类型 | UI 渲染 | 值 |
| --- | --- | --- |
| `text` | 文本框 | 任意字符串 |
| `number` | 数字输入 | 数字 |
| `boolean` | 复选框 | true/false |
| `date` | 日期选择器 | YYYY-MM-DD |
| `datetime` | 日期时间选择器 | ISO 8601 |
| `url` | 可点击链接 | URL |
| `enum(a,b,c)` | 下拉 | a/b/c 之一 |

修饰符:`promoted`(表单化显示)、`multi`/`single`(多值/单值)、`alias=别名`(显示别名)。

### 4. 套用模板后的实际数据结构

实例笔记与模板之间通过 `~template` 关系连接,**不是物理复制**:

```
attributes 表:
  (狂人日记 noteId, relation, template, <书籍模板 noteId>)  ← 类断言
  (狂人日记 noteId, label,    书名,    百年孤独)            ← 数据
  (狂人日记 noteId, relation, 作者关系, <马尔克斯 noteId>)   ← 关系实例
```

**字段定义**留在模板上不复制——所以改模板(比如加 `#label:出版社=text,promoted`),**所有已存在的实例立即获得新字段**(这是 Trilium 模板的最大优势,类比数据库 ALTER TABLE 即时生效)。

### 5. 与子树 + 模板的配合

最佳实践:**用子树组织内容、用模板定义结构**——

```
📚 读书                              #template=@书籍模板(可继承)
  ├── 🇨🇳 中文                       #child:template=@书籍模板
  │   ├── 《百年孤独》                 ← 自动套书籍模板
  │   └── 《红楼梦》
  └── 🌍 外文
      ├── 《1984》
      └── 《罪与罚》
```

子树分区做导航(中文/外文),模板在每个分区统一结构(任何子笔记都有 书名/作者/评分 字段)——**Trilium 的"目录 + 类型"双轨制**。

## 五、典型应用场景

| 场景 | 模板设计 |
| --- | --- |
| **读书笔记** | 书名/作者/状态/评分 + 摘录骨架 |
| **人物档案** | 生卒年/职业/代表作 + 人物关系 relation |
| **日记** | 日期/天气/心情 + runOnNoteCreation 自动填日期 |
| **会议纪要** | 时间/地点/参会人/决议 + 模板正文骨架 |
| **学科节点**(见 [subject-classification.md](09-subject-classification.md)) | code/level/门类 + 群体学科标记 |
| **公式笔记** | 公式/学科码/推导自关系(见 [formula-relations.md](08-formula-relations.md) 谓词) |
| **启动器脚本** | script/keyboardShortcut + 子笔记 JS 代码(见 [scripting.md](10-scripting.md)) |

## 六、易混点提醒

1. **`~template` vs `#template`**:前者是关系(实例挂上,单向引用模板);后者是可继承标签(父节点挂上,向下传递模板作用于子树)。两者常配合:父挂 `#template` 让子自动套,子也能显式挂 `~template` 切换。
2. **模板正文复制 vs 字段定义继承**:字段定义是**逻辑继承**(改模板即时生效),模板正文是**物理复制**(改模板不影响已建实例的内容,只影响新建的)。
3. **模板笔记本身也是笔记**:可以像普通笔记一样给模板打标签、写正文、放子笔记——它只是多了一个"被引用为模板"的角色。
