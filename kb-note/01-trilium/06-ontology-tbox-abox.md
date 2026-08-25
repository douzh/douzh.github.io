# Trilium 与本体论:TBox/ABox 对应实现

[返回 Trilium 档案](README.md) | [属性系统:标签/关系与定义](04-attributes.md) | [数学物理公式推理关系(图谱谓词表)](08-formula-relations.md) | [按学科分类笔记(GB/T 13745)](09-subject-classification.md) | [返回总览](../note-soft.md)

> 这对概念来自**描述逻辑(Description Logic)**——OWL 本体语言的理论基础。一个本体 = 词汇表 + 事实库,两部分分开存放。Trilium 没有显式的 TBox/ABox 划分,但它的架构天然映射这两层。

## 一、TBox 与 ABox 是什么

| | TBox(Terminological Box) | ABox(Assertional Box) |
| --- | --- | --- |
| 中文 | **术语层/模式层** | **断言层/数据层** |
| 回答 | "世界**有哪些概念**、概念间什么关系" | "世界**有哪些个体**、个体有什么事实" |
| 内容 | 类(Class)层级、属性(Property)定义、公理(Axiom) | 类断言、属性断言 |
| 类比 | 数据库的 **schema**、面向对象的**类** | 数据库的**行数据**、面向对象的**对象实例** |
| 变更频率 | 低(模式稳定) | 高(不断积累) |

**TBox 示例**(模式):

```
类:人物 ⊑ 存在者          (Person 是 Existent 的子类)
类:作品 ⊑ 存在者
属性:作者(作品 → 人物)     (定义域:作品,值域:人物)
公理:作者 ≡ 逆(写作)       (作者 与 写作 互为逆属性)
公理:祖先⁺ (祖先是传递属性)
```

**ABox 示例**(数据):

```
狂人日记 : 作品             (类断言:个体"狂人日记"属于"作品")
鲁迅 : 人物                 (类断言)
(狂人日记, 作者, 鲁迅)      (属性断言 = 三元组)
(鲁迅, 出生年份, "1881")    (数据属性断言)
```

**分离的核心价值——推理**:推理机(Reasoner)基于 TBox 公理对 ABox 做三件事:
1. **分类**:推出 `鲁迅 : 存在者`(因 人物⊑存在者,无需显式断言)
2. **隐含关系**:由 `(呐喊,作者,鲁迅)` + 逆公理自动得出 `(鲁迅,写作,呐喊)`
3. **一致性检查**:若 ABox 里有 `呐喊 : 人物` 且 TBox 声明 作品与人物互斥(DisjointWith),报错

这正是 Palantir Ontology "对象-属性-链接" 三层的思想源头(见 [32-symbolic-model.md](../../99ai/1life/32-symbolic-model.md))。

## 二、Trilium 的对应实现

| 本体论概念 | Trilium 对应 | 实现 |
| --- | --- | --- |
| **TBox:类(Class)** | 实体类型模板笔记 | 建一个"字典/Schema"子树:`字典/人物`、`字典/作品`,每条是模板 |
| **TBox:子类层级** | 树结构 + 编码 | 子类作为父类的子笔记(或学科码前缀,见 [subject-classification.md](09-subject-classification.md)) |
| **TBox:数据属性定义** | `#label:xxx` 定义 | 模板上挂 `#label:出生年份=date,promoted` |
| **TBox:对象属性定义** | `#relation:xxx` 定义 | 模板上挂 `#relation:作者=promoted` |
| **TBox:定义域/值域** | 模板归属约束(弱) | 定义挂在哪类模板上,哪类笔记就出现该字段 |
| **ABox:个体** | 每条内容笔记 | 《狂人日记》《鲁迅》各一条 |
| **ABox:类断言** | `~instanceOf=@人物` 或套模板 | 模板继承(`~template`)自动把个体纳入该类 |
| **ABox:属性断言** | 笔记上的 `#标签值` / `~关系实例` | `(狂人日记,作者,鲁迅noteId)` 就是一条三元组 |
| **推理机** | ❌ 无原生,用 SQL+脚本模拟 | 见下 |

### 建模布局

```
📕 我的知识库
├── 📖 字典(TBox 区)
│   ├── 人物          ← 模板:#label:出生年份=date,promoted  #relation:写作=promoted
│   ├── 作品          ← 模板:#label:发表年份=number,promoted  #relation:作者=promoted
│   └── 谓词表         ← 每个关系一条笔记,记录定义域/值域/逆词(参考 [formula-relations.md](08-formula-relations.md) 谓词表)
└── 📄 内容(ABox 区)
    ├── 狂人日记       ← ~template=@字典/作品  #发表年份=1918  ~作者=@鲁迅
    └── 鲁迅          ← ~template=@字典/人物  ~写作=@狂人日记
```

个体挂 `~template=@字典/作品` 的瞬间,TBox 里"作品"类的全部字段定义(promoted 表单、类型约束)就作用于它——**这就是 Trilium 版的"类断言"**。

### 模拟推理机的三个手段

OWL 推理机能自动做的事,Trilium 要显式做,但都能做:

**1. 逆属性(作者 ≡ 逆(写作))**——`runOnAttributeChange` 脚本双向同步:

```javascript
// 挂在字典模板上,监听 ~作者 变化,自动维护对侧的 ~写作
module.exports = async ({ note }) => {
  const 作者 = note.getAttributes().find(a => a.name === '作者');
  if (!作者) return;
  const target = api.getNote(作者.value);
  // 幂等:目标笔记上补挂反向关系
  if (!target.getAttributes().some(a => a.name === '写作' && a.value === note.noteId))
    target.setAttribute('relation', '写作', note.noteId);
};
```

**2. 传递闭包(祖先⁺、推导链)**——SQLite 递归 CTE:

```sql
-- 沿 ~推导自 关系递归上溯,得到完整推理链(如:开普勒定律←牛顿定律←...)
WITH RECURSIVE chain(start, node, depth) AS (
  SELECT noteId, value, 1 FROM attributes
   WHERE name='推导自' AND type='relation' AND isDeleted=0 AND noteId = :startNoteId
  UNION ALL
  SELECT c.start, a.value, c.depth+1 FROM chain c
  JOIN attributes a ON a.noteId = c.node
   AND a.name='推导自' AND a.type='relation' AND a.isDeleted=0
  WHERE c.depth < 10
)
SELECT n.title, depth FROM chain JOIN notes n ON n.noteId = node ORDER BY depth;
```

**3. 一致性检查(TBox 约束校验)**——定时后端脚本:

```javascript
// 每天 cron:扫描 ABox,校验每个 ~template=@字典/作品 的笔记都已填 作者
const 作品类 = api.searchForNotes('~template=@字典/作品');  // 实际用 noteId
for (const n of 作品类) {
  const has = n.getAttributes().some(a => a.name === '作者');
  if (!has) n.addLabel('schema警告', '缺作者');   // 或写日志/生成待办笔记
}
```

## 三、能力边界:Trilium vs 真本体系统

| 能力 | OWL/Protégé | Trilium |
| --- | --- | --- |
| 类层级与继承 | ✅ 自动推理 | ✅ 树+模板继承(显式) |
| 属性定义与类型 | ✅ 硬约束 | ⚠️ UI 提示为主(枚举下拉有约束,类型靠自觉) |
| 公理(传递/互斥/逆) | ✅ 推理机自动 | ⚠️ 脚本+递归 SQL 显式模拟 |
| 隐含知识推导 | ✅ 自动物化 | ❌ 查询时算,不自动生成 |
| 世界假设 | 开放世界(未说即未知) | 封闭世界(SQL 语义) |
| 人类直接编辑体验 | ❌ 差(专家工具) | ✅ **promoted 表单即编辑器** |
| 日常笔记+图谱一体 | ❌ 纯建模 | ✅ **同一库内笔记与断言共存** |

**一句话总结**:Trilium 用"模板笔记=label:/relation: 定义"实现了 TBox,"普通笔记+属性实例"实现了 ABox,牺牲了自动推理机,换来了人类可直接编辑的表单化体验——对个人知识库而言,这个交换通常是划算的;需要严格公理推理的场景(如 Palantir 式决策分析)才需要真本体引擎。
