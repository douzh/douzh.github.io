# RBox（Role Box，角色盒）详解

## 定位

RBox 存放**角色（Role，即属性/关系）层面的公理**，描述"关系本身有哪些性质"。在 OWL 2 DL / SROIQ 中，角色被明确区分为**对象属性（ObjectProperty，连接个体与个体）**和**数据属性（DataProperty，连接个体与字面量）**，RBox 主要约束对象属性。

注意一个易混点：**属性的 domain/range 不在 RBox**——它们约束的是"属性用在谁身上"，本质是概念层面的公理，在 OWL 中会被转换成 TBox 公理。RBox 只装"关系的元性质"。

## 常见公理类型 + 示例

### 1. 子角色（SubRole / SubPropertyOf）

一个角色是另一个角色的子集。**推理意义**：如果 `R ⊑ S`，那么凡有 `R(a,b)` 就有 `S(a,b)`。

```
hasSon ⊑ hasChild          # 有儿子 ⇒ 有孩子
hasDaughter ⊑ hasChild
hasChild ⊑ hasRelative     # 有孩子 ⇒ 有亲属
```

推论链：`hasSon(zhangsan, lisi)` 会自动推出 `hasChild(zhangsan, lisi)` 和 `hasRelative(zhangsan, lisi)`。

### 2. 传递性（Transitive）

若 `a R b` 且 `b R c`，则 `a R c`。

```
Transitive(ancestorOf)     # 祖先关系可传递
Transitive(partOf)         # 部分关系可传递（如 发动机 partOf 汽车）
Transitive(isConnectedTo)
```

实例：`partOf(气缸, 发动机)` ∧ `partOf(发动机, 汽车)` ⇒ `partOf(气缸, 汽车)`。

**注意**：传递角色不能用于基数约束和链式属性（property chain），否则导致不可判定或语义混乱——这是 DL 的语法限制，定义角色时要先想清楚。

### 3. 自反性 / 反自反性（Reflexive / Irreflexive）

```
Reflexive(knows)           # 每个人知道自己
Reflexive(relatedTo)
Irreflexive(parentOf)      # 谁都不是自己的父母
Irreflexive(hasParent)
```

`Reflexive(R)` 意味着对所有个体 `a` 都有 `R(a,a)`；`Irreflexive` 则禁止任何 `R(a,a)`。

### 4. 对称性 / 反对称性（Symmetric / Asymmetric）

```
Symmetric(siblingOf)       # 兄弟关系对称：a 是 b 的兄弟 ⇒ b 是 a 的兄弟
Symmetric(spouseOf)
Asymmetric(parentOf)       # 若 a 是 b 的父母，则 b 不可能是 a 的父母
Asymmetric(hasChild)
```

注意区分：`siblingOf` 严格说应是"同父母"而非互称兄弟，建模时语义要谨慎。

### 5. 不相交角色（DisjointRoles）

两个角色不可能同时成立（对同一对个体）。

```
DisjointProperties(hasFather, hasMother)
DisjointProperties(parentOf, childOf)
```

`DisjointProperties(hasFather, hasMother)` 表示不存在个体对 `(a,b)` 同时满足两者——一个人不可能既是某人的父亲又是他的母亲。

### 6. 逆角色（Inverse）

角色与它的逆互为反向，是推理的"桥梁"。

```
inverseOf(hasChild) = hasParent
inverseOf(manages)  = isManagedBy
```

**推理意义**：`hasChild(zhangsan, lisi)` ⇔ `hasParent(lisi, zhangsan)`，自动双向推出。OWL 2 中还提供 `InverseFunctional`（逆函数性，如身份证号）和 `Functional`（函数性，如 hasBirthMother）。

### 7. 属性链（Property Chain / SubPropertyChain）

**OWL 2 / SROIQ 特有**，是 RBox 中最有表达力也最容易出问题的部分。

```
hasParent ∘ hasBrother ⊑ hasUncle
   # 有父母 + 父母有兄弟 ⇒ 我有叔叔（伯父）

hasChild ∘ hasWife ⊑ hasDaughterInLaw
   # 有孩子 + 孩子有妻子 ⇒ 我有儿媳

isPartOf ∘ isPartOf ⊑ isPartOf
   # 链式等价实现传递性
```

**用法**：在 Protege 里勾选 `SubProperty Chain (SubPropertyOf(owl:propertyChainAxiom))` 添加。

**三大坑**：
- 链中**不能有传递角色**、复杂角色（inverse）等非简单角色；
- 链必须"单向"，不能形成 `R∘R` 对非传递 R 的循环导致不可判定；
- 链式推理会产生大量隐式事实，拖慢推理器（尤其 HermiT/Pellet）。

## RBox 如何参与推理

RBox 不是孤立的——它是推理的"燃料"，主要作用于 **Tableau 算法**中的两类规则：

| 推理类型 | 依赖的公理 |
|---|---|
| 实例化（materialization）| 子角色、传递性、对称性 |
| 概念可满足性检查 | 逆角色、属性链、传递性 |
| 分类（classification）| 属性链 + TBox 中的 existential 约束 |

典型链路：TBox 说 `Man ⊑ ∃hasFather.Man`，RBox 说 `hasFather` 是 `hasParent` 的子角色，ABox 断言 `hasFather(zhangsan, laozhang)`——推理器就能推出 `zhangsan` 有 `hasParent` 父亲且该父亲是 `Man`。**TBox 定概念结构，RBox 定关系结构，ABox 提供事实，三者共同决定推理闭包。**

## 与 OWL 的完整对应

| RBox 概念 | OWL 2 语法（Turtle 片段） |
|---|---|
| 子角色 | `hasSon rdfs:subPropertyOf :hasChild .` |
| 传递 | `:ancestorOf a owl:TransitiveProperty .` |
| 对称 | `:siblingOf a owl:SymmetricProperty .` |
| 反对称 | `:parentOf a owl:AsymmetricProperty .` |
| 自反 | `:knows a owl:ReflexiveProperty .` |
| 逆角色 | `:hasChild owl:inverseOf :hasParent .` |
| 不相交 | `:hasFather owl:propertyDisjointWith :hasMother .` |
| 属性链 | `:hasUncle owl:propertyChainAxiom (:hasParent :hasBrother) .` |

## 建模建议

1. **尽量少用传递/链式角色**——它们是推理复杂度（从 NLogTime 跳升到 N2ExpTime 级别）和工程难题的主要来源。能用子角色+逆角色解决就优先。
2. **显式声明逆角色**：双向关系（hasChild/hasParent、manages/isManagedBy）声明逆关系后查询效率大幅提升，避免"一个方向有数据、另一个方向查不到"。
3. **区分真实传递与"想当然传递"**：`partOf` 传递但 `adjacentTo`（相邻）绝不传递——语义上没想清楚就标 Transitive 是本体里最常见的错误之一。

一句话总结：**TBox 定义概念网，RBox 定义关系网（并给推理器提供传递、对称、逆、链等"推导弹药"），ABox 是具体的点和线。**

## 相关笔记

- [[30-tbox]]　TBox 概念盒详解（类的定义与层级）
- [[32-abox]]　ABox 断言盒详解（个体事实）
- [[20-dl]]　描述逻辑基础（SROIQ 语法限制的由来）
- [[40-reasoning]]　RBox 公理如何驱动推理器