# Protege 实操：从零建一个可推理的本体

## 一句话定位

**Protege 是斯坦福开源的免费本体编辑器，本页用"家族本体"做例子，带你 20 分钟走完 TBox → RBox → ABox → 推理 → 查询全流程。** 全程图形界面，无需写一行代码。

---

## 〇、安装与启动

- 官网下载 `Protege-5.6.x-win.zip` → 解压 → 双击 `run.bat`（**自带 JRE，无需装 Java**）；
- 或 `winget install Stanford.Protege`；
- 启动后主界面默认加载了一个空本体（默认 IRI：`http://www.semanticweb.org/.../untitled-ontology-2`）。

> 更多环境细节见 [[60-tools]]。建议先建个新本体：`File → New`。

---

## 一、建 TBox（类的层级）

1. 左侧面板 `Active Ontology → Entities → Classes`；
2. `owl:Thing` 下点 `Add subclass` 建类：`Person`、`Man`、`Woman`、`Parent`、`Father`、`Mother`；
3. 设父子关系：
   - `Man ⊑ Person`、`Woman ⊑ Person`；
   - `Man` 与 `Woman` **disjoint**（点 `Man` → `Disjoint With` → 选 `Woman`）；
   - 勾选 `Man`、`Woman` 的 **Subclasses Of** 为 `Person`。

> 建完看一下：类层级树是否干净、有没有意外的菱形（多重继承要谨慎）。

## 二、建 RBox（属性与角色公理）

1. 切到 `Object properties` 标签页；
2. 新建对象属性：`hasChild`、`hasParent`、`hasSpouse`、`knows`；
3. 设**逆角色**：点 `hasChild` → `Inverse Of` → `hasParent`（会自动双向绑定）；
4. 设**特征**：
   - `hasSpouse` → `Symmetric`（对称）；
   - `knows` → `Reflexive`（自反，可选）；
   - 新建 `ancestorOf` → `Transitive`（传递）；
   - 新建 `hasBirthMother` → `Functional`（函数性，一个人最多一个生母）。
5. （进阶）**属性链**：`Object property → SubProperty Of (chain)` 处添加 `hasParent ∘ hasBrother ⊑ hasUncle`。

> 想给属性加 domain/range：`Domain` 填 `Person`、`Range` 填 `Person`——注意它本质是 TBox 约束（见 [[30-tbox]]）。

## 三、建 ABox（个体与断言）

1. 切到 `Individuals` 标签页；
2. 点 `Create individual` 建：`zhangsan`、`lisi`、`laozhang`、`laoli`；
3. 给 `zhangsan` 填 `Types` = `Man`；
4. 在 `Object property assertions` 里点 `hasChild` → 选 `lisi`（张三→李四）；
5. 给 `laozhang` 设 `Types` = `Man`，`hasChild` = `zhangsan`；
6. 给 `lisi` 设 `Types` = `Person`。

## 四、跑推理（见证"新事实"诞生）

1. 菜单 `Reasoner → HermiT → Start reasoner (Ctrl+R)`；
2. 观察变化：
   - `lisi` 自动被归为 `Person`（继承）；
   - 由于 `hasChild` 的逆角色，推理器自动补出 `lisi hasParent zhangsan`（在 `lisi` 的 Object property assertions 里能看到灰色推导项）；
   - 如果给 `Father` 写了充分定义 `Father ≡ Man ⊓ ∃hasChild.Person`，`zhangsan` 会被自动归类为 `Father`；
3. `Reasoner → Explain inconsistent ontology` 可查矛盾原因（如果你建的类有冲突）。

## 五、查询（DL Query）

1. 菜单 `Window → Tabs → DL Query` 打开查询面板；
2. 输入表达式如 `Man and hasChild some Person`，点 `Execute`：
   - `Descendants` 给出所有子类；
   - `Instances` 给出所有满足的个体（配合推理器，能查出"隐含"实例）。

## 六、保存与导出

- `File → Save` 默认存 **RDF/XML（.owl）**；
- 想导出 Turtle：`File → Save As → Turtle`；
- 也可导出 `OBO`（生物医学常用）等格式。

## 七、一个完整小例子（结果等价于这份 Turtle）

```turtle
@prefix : <http://example.org/family#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

:Man rdfs:subClassOf :Person .
:Woman rdfs:subClassOf :Person .
:Man owl:disjointWith :Woman .
:Father owl:equivalentClass
    [ a owl:Restriction ; owl:onProperty :hasChild ; owl:someValuesFrom :Person ] .

:hasChild owl:inverseOf :hasParent .
:ancestorOf a owl:TransitiveProperty .
:hasBirthMother a owl:FunctionalProperty .

:zhangsan a :Man ; :hasChild :lisi .
:lisi a :Person .
```

推理器推出：`lisi :hasParent :zhangsan`、`:zhangsan a :Father`。

---

## 常见坑

| 现象 | 原因 | 修法 |
|---|---|---|
| 推理结果没变化 | 忘了 Start reasoner | 确认 `Reasoner` 已启动 |
| 报不一致 | 个体同时属于 disjoint 类 | 检查 Types 与继承 |
| 类没自动归并 | 写的是 `⊑` 不是 `≡` | 改 `Equivalent To` |
| 查询不出隐含结果 | 推理器未运行 | 先推理再查询 |
| 保存后别处打不开 | IRI 冲突 | 改默认 IRI 为你的命名空间 |

---

## 记忆口诀

- **四步建库**：类（TBox）→ 属性（RBox）→ 个体（ABox）→ 推理；
- **一个开关**：Start reasoner 不点，前面全白搭；
- **查询靠 DL Query**：表达式 + Execute，隐含实例也能查。

一句话总结：**Protege 的用法就是"先画类、再定属性、塞个体、按推理"，把 [[30-tbox]]/[[31-rbox]]/[[32-abox]]/[[40-reasoning]] 的理论落到界面上；能跑通 HermiT 并查到隐含结论，你就入门了。**

## 相关笔记

- [[60-tools]]　工具安装与环境
- [[30-tbox]] / [[31-rbox]] / [[32-abox]]　三盒理论
- [[40-reasoning]]　推理器行为
- [[62-sparql]]　导出后用 SPARQL 查询
- [[70-engineering]]　建本体前先做的方法论
