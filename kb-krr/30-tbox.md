# TBox（Terminological Box，术语盒/概念盒）详解

## 定位

TBox 存放**概念（类）层面的公理**，回答"某类是什么、类与类之间是什么关系"。它是本体里"定义词汇表"的部分——**TBox 说清楚：领域里有哪些类，谁是谁的子类，哪些类不相交，某个类怎么由其他类定义出来。**

> 一句话对比三盒：**TBox 定"概念网"，RBox 定"关系网"，ABox 是"具体的点和线"。** 本页只讲概念网。

---

## 一、TBox 装什么（公理类型总览）

| 公理 | DL 记法 | OWL 2 语法 | 通俗含义 |
|---|---|---|---|
| 子类 | `C ⊑ D` | `rdfs:subClassOf` | C 是 D 的子集（C 都是 D）|
| 等价类 | `C ≡ D` | `owl:equivalentClass` | C 与 D 外延完全相同 |
| 不相交 | `C ⊓ D ⊑ ⊥` | `owl:disjointWith` | C 与 D 没有交集 |
| 不相交类组 | — | `owl:AllDisjointClasses` | 一组类两两不相交 |
| 个体类型 | — | `owl:hasKey`（键）| 用某属性组唯一标识个体 |

### 1. 子类（subClassOf）——层级的地基

```
Man ⊑ Person          # 男人是人
Student ⊑ Person      # 学生是人
Dog ⊑ Animal          # 狗是动物
```

**推理意义**：`Man ⊑ Person` + `Person ⊑ Human` ⇒ 自动推出 `Man ⊑ Human`（传递）。分类（classification）就是推理器沿这条链把全部分类树算出来。

### 2. 等价类（equivalentClass）——定义式 vs 描述式

TBox 公理有两种"气质"，这是最重要的概念：

| | 必要（necessary）| 必要且充分（necessary & sufficient）|
|---|---|---|
| 记法 | `C ⊑ D` | `C ≡ D` |
| 意思 | "C 一定是 D" | "是 C 当且仅当是 D" |
| 用途 | 附加约束/父类 | 给出完整定义 |
| 推理 | 单向 | 双向（可用它判定个体归入）|

例子：`Father ≡ Man ⊓ ∃hasChild.Person`（父亲 = 男人且至少有一个孩子是人）。有了这个**充分定义**，ABox 里一个 `Man` 且 `hasChild` 到某 `Person` 的个体，推理器就能自动把它归为 `Father`——这是"推理出新事实"的典型。

### 3. 不相交（disjointWith）——排除错误

```
Man disjointWith Woman      # 没人既是男人又是女人
Dog disjointWith Cat
```

**推理意义**：若 ABox 断言某个体同时是 `Man` 和 `Woman`，推理器报告 **不一致（inconsistent）**。不相交公理是本体质量的关键防线。

### 4. 键（hasKey）——唯一性约束

```
:Person owl:hasKey ( :idCardNumber )
```

声明"身份证号"能唯一确定一个人。推理器可用它做个体合并/去重的依据（类似数据库主键，但语义更灵活）。

---

## 二、复杂类表达式（TBox 里的"公式"）

TBox 不止能写"类 ⊑ 类"，还能写"类 ⊑ 复杂表达式"：

```
# 布尔组合
:Teenager ≡ :Person ⊓ (:Age ≥ 13) ⊓ (:Age ≤ 19)

# 存在限制：什么"需要"什么
:Professor ⊑ ∃:affiliatedWith . :University

# 全称限制：什么"只允许"什么
:Person ⊑ ∀:hasPet . :Animal          # 人的宠物一定是动物

# 基数约束
:Polygamist ⊑ (≥ 2 :spouseOf)         # 多配偶者至少有 2 个配偶

# 枚举类（单例）
:ChineseCity ≡ {:Beijing, :Shanghai, :Guangzhou}
```

这些复杂表达式让 TBox 从"分类树"升级为"可推理的知识规则"。

---

## 三、一个易混点：domain / range 其实属于 TBox

很多教材把属性的 domain/range 归到"属性"上，但在 **OWL 里它被转换成 TBox 公理**：

```
:hasChild rdfs:domain :Person .   # 等价于：∃hasChild.⊤ ⊑ Person
:hasChild rdfs:range  :Person .   # 等价于：⊤ ⊑ ∀hasChild.Person
```

所以 RBox 只装"关系的元性质"（传递/对称/逆/链），**domain/range 这类"属性用在哪类身上"的约束本质是概念约束，归 TBox**。详见 [[31-rbox]]。

---

## 四、TBox 如何参与推理

TBox 是**分类（classification）**的主战场，主要供给 Tableau 算法（见 [[40-reasoning]]）两类信息：

| 推理 | 依赖的 TBox 公理 |
|---|---|
| 分类 / 子类关系 | subClassOf、equivalentClass |
| 概念可满足性 | 全部（尤其 disjoint 与基数）|
| 个体归类（instance of）| equivalentClass（充分定义）|
| 一致性检查 | disjointWith、基数约束 |

典型链路：`Father ≡ Man ⊓ ∃hasChild.Person`（TBox）＋ `Man(zhangsan)`、`hasChild(zhangsan, lisi)`、`Person(lisi)`（ABox）⇒ 推理器推出 `Father(zhangsan)`。**TBox 给出"判据"，ABox 给出"材料"，结论自动涌现。**

---

## 五、建模建议

1. **想清楚"必要" vs "必要且充分"**：只有真正常态共存的才写 `≡`（充分定义），否则用 `⊑` 单向下推，避免过度归并。
2. **充分定义是推理的发动机**：想让"自动归类"发生，就把定义写成 `≡`，否则推理器只能向下查父类、不能向上归纳个体。
3. **不相交要克制**：只对确实互斥的类声明 disjointWith；无根据的不相交会制造"假矛盾"。
4. **复杂类表达式从"口语规则"出发**：先用自然语言写规则（"父亲=男人且有孩子"），再翻译成 DL 记法，别直接写语法。
5. **层级优先**：先建干净的 `subClassOf` 树，再加等价/不相交/基数，由简入繁、每步跑一次推理器验证。

---

## 记忆口诀

- **TBox 四公理**：子类、等价、不相交、键；
- **两种定义**：`⊑` 必要（单向下推）、`≡` 必要且充分（双向可归纳）；
- **domain/range 是伪装的 TBox**：本质是"属性用在哪类身上"的概念约束。

一句话总结：**TBox 是本体里"概念的字典 + 定义规则"，它决定了系统能自动归纳出哪些新类、能识破哪些矛盾；没有好的 TBox，ABox 只是一堆散沙，推理器无事可做。**

## 相关笔记

- [[20-dl]]　描述逻辑语法（⊓/⊔/∃/∀/基数 的来源）
- [[31-rbox]]　RBox 角色盒（与 TBox 的分工）
- [[32-abox]]　ABox 断言盒（TBox 定义的"原料"）
- [[40-reasoning]]　分类与可满足性推理
- [[61-protege-tutorial]]　在 Protege 里建 TBox 实操
