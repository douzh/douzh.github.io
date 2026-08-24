# Trilium 按学科分类笔记(GB/T 13745 编码体系)

[返回 Trilium 档案](README.md) | [属性系统:标签/关系与定义](attributes.md) | [数据库表设计](db-design.md) | [返回总览](../note-soft.md)

> 源数据:[level1](../../kb-km/index-sci-source-level1.md) / [level2](../../kb-km/index-sci-source-level2.md) / [level3](../../kb-km/index-sci-source-level3.md)(GB/T 13745—2009,62 一级 / 738 二级 / 2732 三级)。
> GB/T 编码的关键特性:**7 位编码自带层级**(前 3 位一级、前 5 位二级、前 7 位三级)——这正是 Trilium 建模的抓手,一个标签字段即可表达三层归属。

## 一、方案对比

| 方案 | 做法 | 优点 | 缺点 |
| --- | --- | --- | --- |
| A. 学科树 + 克隆 | 把分类建成一棵树,笔记克隆进学科节点 | 树形可视化、导航直观 | 克隆污染主树;一篇笔记多处克隆后位置混乱 |
| B. 标签编码 | 笔记挂 `#学科=1103410` | 轻量、SQL/搜索直达 | 学科本身无实体笔记,无法给学科挂说明 |
| C. 关系节点 | 每个学科是一条笔记,内容笔记挂 `~subject=@微分学` | 学科是实体(可挂简介/反查成员);Relation Map 可视化 | 建库成本高(62+738+2732 条) |

**推荐 B+C 组合**:学科节点树(知识骨架,一次性脚本生成)+ 内容笔记挂标签码+关系(轻量挂靠),两层各司其职。

## 二、推荐实现

### 1. 学科节点树(骨架)

```
📚 学科分类(GB/T 13745)
├── A 自然科学
│   ├── 110 数学            #code=110   #level=1
│   │   ├── 11034 数学分析    #code=11034  #level=2
│   │   │   ├── 1103410 微分学  #code=1103410 #level=3
│   │   │   └── ...
```

学科模板挂定义(子节点填值),模板笔记挂:

```
#label:code=text,promoted
#label:level=number,promoted        ← 1/2/3 级
#label:门类=enum(A,B,C,D,E),promoted
```

**批量生成**:手工建 3542 条不现实,用下方脚本(解析 level2/level3 文件的 `| 代码 | 名称 |` 表格,批量建树)。

### 2. 内容笔记挂靠(标签 + 关系双轨)

内容笔记模板挂:

```
#label:学科码=text,promoted         ← 填最细粒度,如 1103410
#relation:subject=promoted,multi    ← 选择器指向学科节点,如"微分学"
```

一篇笔记填完后实际生成:

```
《微分方程数值解法笔记》
  #学科码=1106120
  ~subject=<常微分方程数值解 noteId>
```

- **双轨的理由**:标签码供**检索**(SQL 前缀查询快),关系供**图谱**(Relation Map、反查成员、学科节点看 incoming links)
- **交叉学科**:GB/T 规定"只在一处赋码"——但 Trilium 不受此限,`~subject` 是 multi,可多挂;`#学科码` 建议保持单值以主分类为准

### 3. 检索:编码前缀 = 层级查询

因为编码自带层级,**不用给笔记挂三级标签**,前缀即上级:

```
#学科码=1103410          ← 精确:微分学
#学科码%=1103            ← 前缀:数学分析及其全部下级(Trilium 前缀匹配)
```

SQL 版(任意层级聚合):

```sql
-- 统计"数学(110)"下各笔记分布,含全部二三级
SELECT n.title, a.value AS 学科码
FROM notes n JOIN attributes a ON n.noteId = a.noteId
WHERE a.name = '学科码' AND a.isDeleted = 0
  AND a.value LIKE '110%'          -- 换前缀即换层级
ORDER BY a.value;
```

码→名的反查(学科节点表):

```sql
SELECT title FROM notes
WHERE noteId IN (SELECT noteId FROM attributes
                 WHERE name='code' AND value LIKE '1103%');
```

### 4. 视图与自动化

- **保存搜索**:每个常用学科建一条 Saved Search(`#学科码%=1103`),侧栏直达
- **Collections 表格**(v0.104+):promoted 的 `#学科码` 直接作为列,排序即按学科聚簇
- **Relation Map**:挂 `~map:subject`,笔记-学科网络自动渲染
- **自动化脚本**:`runOnAttributeChange` 监听 `#学科码`,自动校验码合法性(存在性+位数)并自动挂 `~subject` 关系到对应学科节点——码与关系从此不用双填(见脚本骨架第三节)

### 5. GB/T 特殊规则的处理

| 标准规则 | Trilium 实现 |
| --- | --- |
| 99 = 群体学科("其他学科") | 正常建节点,`#群体学科=1` 标记,统计时可排除 |
| 交叉学科"见(代码)" | 学科节点挂 `~见=@1103410` 关系,检索时跟随 |
| 2016 修订(输血医学 32032 等) | 节点挂 `#修订=2016` 标签,来源可追溯 |
| 名称带"原名为X" | 节点挂 `#旧名=X` 标签 |

## 三、批量建树脚本骨架

三个脚本都是 Trilium 内的 **JS backend code note**(`#run` 手动触发,或 `runOnNoteCreation` 挂在导入文件上)。

### 1. 主脚本:解析 md 表格 + 批量建树

```javascript
// 挂 #run 手动执行;rootNoteId 指向"📚 学科分类"根笔记
const api = require('./api');
const rootNote = api.getNote('ROOT_NOTE_ID');

// level1 门类与一级学科(数量少,直接内联;也可从 level1 文件解析)
const LEVEL1 = [
  { code: '110', name: '数学', 门类: 'A' },
  { code: '120', name: '信息科学与系统科学', 门类: 'A' },
  // ... 共 62 条,从 index-sci-source-level1.md 提取
];
const 门类名 = { A: '自然科学', B: '农业科学', C: '医药科学', D: '工程与技术科学', E: '人文与社会科学' };

// ── 工具:解析 "| **11011** | **数学史** | ... |" 或 "| 1101410 | 演绎逻辑学 | 亦称... |" ──
function parseRows(mdText) {
  const rows = [];
  for (const line of mdText.split('\n')) {
    if (!line.trim().startsWith('|')) continue;
    const cells = line.split('|').map(s => s.trim()).filter(Boolean);
    if (cells.length < 2) continue;
    const code = cells[0].replace(/\*/g, '');          // 去 ** 加粗标记
    const name = cells[1].replace(/\*/g, '');
    const note = cells[2] || '';                        // 说明列(原名/参见)
    if (!/^\d{5,7}$/.test(code)) continue;              // 跳过表头/门类行
    rows.push({ code, name, note });
  }
  return rows;
}

// ── 建/取节点,幂等(已存在则复用) ──
const noteIndex = {};   // code -> noteId
function ensureNote(parentId, { code, name, level, extra }) {
  const existing = api.searchForNote(`#code="${code}"`);
  if (existing) { noteIndex[code] = existing.noteId; return existing; }
  const { note } = api.createNewNote({
    parentNoteId: parentId,
    title: `${code} ${name}`,
    type: 'text',
    content: ''
  });
  note.setAttribute('label', 'code', code);
  note.setAttribute('label', 'level', String(level));
  if (extra) for (const [k, v] of Object.entries(extra)) note.setAttribute('label', k, v);
  noteIndex[code] = note.noteId;
  return note;
}

// ── 三层建树 ──
// 第 0 层:门类
const 门类Nodes = {};
for (const k of Object.keys(门类名)) {
  门类Nodes[k] = ensureNote(rootNote.noteId, { code: `G${k}`, name: 门类名[k], level: 0 });
}

// 第 1 层:一级学科
for (const it of LEVEL1) {
  ensureNote(门类Nodes[it.门类].noteId, { code: it.code, name: it.name, level: 1, extra: { 门类: it.门类 } });
}

// 第 2/3 层:解析 level2/level3 文件内容(把文件正文作为常量或子笔记读取)
const md2 = api.getNote('LEVEL2_NOTE_ID').getContent();  // 或直接内联字符串
const md3 = api.getNote('LEVEL3_NOTE_ID').getContent();
const rows2 = parseRows(md2).filter(r => r.code.length === 5);   // 二级=5位
const rows3 = parseRows(md3).filter(r => r.code.length === 7);   // 三级=7位

for (const r of [...rows2, ...rows3]) {
  const parentCode = r.code.slice(0, r.code.length === 5 ? 3 : 5);   // 前缀即父级
  const parent = noteIndex[parentCode] || api.searchForNote(`#code="${parentCode}"`);
  if (!parent) { console.log(`跳过(父级缺失): ${r.code}`); continue; }
  const extra = {};
  if (r.code.endsWith('99')) extra.群体学科 = '1';
  const m = r.note.match(/原名为[“"](.+?)[”"]/);  if (m) extra.旧名 = m[1];
  if (/2016|输血医学/.test(r.note)) extra.修订 = '2016';
  ensureNote(parent.noteId, { code: r.code, name: r.name, level: r.code.length === 5 ? 2 : 3, extra });
}

console.log(`完成:一级 62 / 二级 ${rows2.length} / 三级 ${rows3.length}`);
```

**用法**:把 level2/level3 两个 md 导入 Trilium 作为 file note,取其 noteId 填入脚本;先手动建好根笔记与"📚 学科分类"标题;执行一次即建全树。重跑幂等(已存在节点复用)。

### 2. 辅脚本:码校验 + 自动挂 subject 关系

```javascript
// 挂在"内容笔记模板"上:#run=now + #runOnAttributeChange=学科码
const api = require('./api');

module.exports = async ({ note }) => {
  const 码attr = note.getAttributes().find(a => a.name === '学科码' && a.type === 'label');
  if (!码attr || !码attr.value) return;

  const code = 码attr.value.trim();
  // 1. 校验:位数 + 节点存在
  if (!/^\d{5,7}$/.test(code)) { console.log(`非法码: ${code}`); return; }
  const subject = api.searchForNote(`#code="${code}"`);
  if (!subject) { console.log(`码不存在于学科树: ${code}`); return; }

  // 2. 幂等挂关系:已挂且目标一致则跳过
  const cur = note.getAttributes().find(a => a.name === 'subject' && a.type === 'relation');
  if (cur && cur.value === subject.noteId) return;
  if (cur) note.setAttribute('relation', 'subject', subject.noteId);   // 覆盖旧目标
  else note.setAttribute('relation', 'subject', subject.noteId);

  // 3. 自动补门类/一级标签(便于 Collections 分组)
  const prefix1 = code.slice(0, 3), prefix2 = code.slice(0, 5);
  note.setLabel('一级学科码', prefix1);
  note.setLabel('二级学科码', prefix2);
};
```

### 3. 验证脚本:统计建树结果

```sql
-- SQL console:各层级节点数应约为 62 / 738 / 2732
SELECT a.value AS level, COUNT(*) AS n
FROM attributes a
WHERE a.name = 'level' AND a.isDeleted = 0
GROUP BY a.value;

-- 抽查:110 数学下的二级
SELECT n.title FROM notes n
JOIN attributes a ON n.noteId = a.noteId
WHERE a.name='code' AND a.value LIKE '110__' AND a.isDeleted=0
ORDER BY a.value;
```

## 四、落地顺序

1. 建学科模板 + 手工建 5 门类、62 一级(少量)
2. 导入 level2/level3 文件,跑脚本骨架 1 批量建 738 二级 + 2732 三级
3. 建内容笔记模板(学科码 + subject 关系定义),挂脚本骨架 2
4. 建 Saved Search 与 Collections 视图
5. 跑验证脚本核对数量,补 `~见` 交叉引用与 `#修订` 标签
