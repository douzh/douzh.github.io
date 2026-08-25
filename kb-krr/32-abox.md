# ABox（Assertional Box，断言盒）详解

## 定位

ABox 存放**关于个体的具体事实**，回答"现实世界里有谁、谁跟谁有什么关系"。如果说 TBox 是"字典和规则"，ABox 就是"登记在册的事实清单"。**TBox 定义概念结构，RBox 定义关系结构，ABox 提供原材料——三者共同决定推理闭包。**

---

## 一、ABox 装什么（断言类型总览）

| 断言 | OWL 2 语法（Turtle）| 通俗含义 |
|---|---|---|
| 类型断言 | `:zhangsan a :Man .` | 张三属于"男人"类 |
| 对象属性断言 | `:zhangsan :hasChild :lisi .` | 张三是李四的父亲 |
| 数据属性断言 | `:zhangsan :hasAge 35 .` | 张三年龄 35 |
| 相等个体 | `:lisi owl:sameAs :lisi2 .` | 李四和李四2是同一人 |
| 不同个体 | `:zhangsan owl:differentFrom :lisi .` | 张三和李四不是同一人 |
| 互异组 | `:a owl:AllDifferent (:b :c :d) .` | 一组个体两两不同 |
| 否定断言 | `:zhangsan owl:negativePropertyAssertion :hasChild :wangwu .` | 王五不是张三的孩子 |

### 类型断言（ClassAssertion）

```
:zhangsan a :Man .
:lisi    a :Person .
```

表示个体属于某个类。注意：**类别不写，推理器也能推出来**（见 [[30-tbox]] 的充分定义）。

### 对象/数据属性断言（PropertyAssertion）

```
# 对象属性（个体↔个体）
:zhangsan :hasChild :lisi .
# 数据属性（个体↔字面量）
:zhangsan :hasAge "35"^^xsd:integer .
```

对象属性连接两个个体，数据属性连接个体与一个具体值（字符串/数字/日期）。

### 个体同一性（SameAs / DifferentFrom）

```
:lisi owl:sameAs :lisi2 .        # 两个 IRI 指同一个体
:zhangsan owl:differentFrom :lisi .
```

`sameAs` 是本体重用/数据合并的核心机制：两个不同来源的 IRI 可以声明指向同一实体。

---

## 二、两个决定性的世界观：OWA 与 UNA

这是 ABox 与"数据库表"最本质的分水岭，**不理解这个就理解不了本体**：

| 假设 | 全称 | 含义 | 例 |
|---|---|---|---|
| **OWA** | Open World Assumption 开放世界假设 | 没说的 ≠ 不存在，只是"未知" | 没存"李四的孩子"，不意味着他没有孩子 |
| **CWA** | Closed World Assumption 封闭世界假设 | 没说的 = 不存在（数据库默认） | SQL 查不到 = 没有 |

| 假设 | 全称 | 含义 | 例 |
|---|---|---|---|
| **UNA** | Unique Name Assumption 唯一名假设 | 不同名字默认指不同对象 | 数据库默认成立 |
| **非 UNA** | — | 不同名字**可能**是同一个对象（默认不成立）| 本体里 `a ≠ b` 必须显式声明 |

> 一句话记忆：**本体活在"开放世界 + 不唯一名"的宇宙里**——这是为了支持"知识不完整、可增量补充、多源合并"的场景；数据库活在"封闭世界 + 唯一名"里——那是为了查询的确定性和存储的高效。所以 [[11-vs-kg-db]] 里说两者哲学不同。

**工程影响**：本体里想表达"一个人只能有一个母亲"，光写 `hasMother` 不够，还要显式声明 `Functional(hasMother)`；想表达"张三没有孩子"，必须写否定断言或直接声明 `≤0 hasChild`，否则推理器只会说"未知"。

---

## 三、ABox 如何参与推理

| 推理任务 | 依赖的 ABox + TBox/RBox | 结果 |
|---|---|---|
| **一致性检查** | ABox + disjointWith / 基数 | 报告矛盾（如某人既是 Man 又是 Woman）|
| **实例化（materialization）** | ABox + 充分定义（`≡`）| 自动给个体归类（推出 `Father(zhangsan)`）|
| **蕴含查询** | ABox + 角色公理 | 推出未显式存储的关系（逆角色、传递）|
| **个体合并** | sameAs + 键（hasKey）| 把不同来源的同一实体归并 |

典型链路：ABox 断言 `hasFather(zhangsan, laozhang)` ＋ RBox 声明 `hasFather ⊑ hasParent` ⇒ 推理器自动推出 `hasParent(zhangsan, laozhang)`——**查询时不用手工把每个父子关系都存一遍。**

---

## 四、与数据库"表数据"的对比

| 维度 | ABox | 数据库表 |
|---|---|---|
| 世界观 | OWA（未知≠没有）| CWA（查不到=没有）|
| 名字 | 默认不唯一，靠 sameAs | 主键唯一 |
| 约束 | 靠公理（disjoint、基数）| 靠 CHECK/外键 |
| 推理 | ✅ 自动补全事实 | ❌ 只返回显式存储 |
| 增补 | 可随时加新断言，不影响旧的 | 要迁表/改 schema |
| 冲突 | 推理器报"不一致" | 数据库直接拒绝写入 |

---

## 五、建模建议

1. **不要试图把"所有事实"都存进 ABox**：本体设计的目标是"存最小必要事实 + 用公理推出其余"，否则等于又造了个数据库。
2. **逆角色 + 子角色优先，别手工双写**：`hasChild`/`hasParent` 声明逆角色，ABox 只存一个方向。
3. **善用 sameAs 做数据融合**：多来源数据用 `sameAs`/`owl:hasKey` 归并，而不是物理改 IRI。
4. **表达"没有"要显式**：开放世界下，"无"必须写成基数约束或否定断言，否则推理器答"未知"。
5. **每次加断言都跑一致性检查**：ABox 是矛盾的高发区（尤其配上 disjoint/基数），边加边验。

---

## 记忆口诀

- **三种断言**：类型（属于谁）、关系（连着谁）、同一性（是谁/不是谁）；
- **两对世界观**：OWA（未知≠没有）× 非UNA（同名未必同物）；
- **一个目标**：存最少事实，推最多结论。

一句话总结：**ABox 是本体的"事实底账"——在开放世界假设下记录个体及其关系，配合 TBox/RBox 的规则自动补全隐含事实；它和数据库表长得像，但世界观完全不同。**

## 相关笔记

- [[20-dl]]　描述逻辑基础（断言是 ABox 的语法）
- [[30-tbox]]　TBox 概念盒（给 ABox 提供归类判据）
- [[31-rbox]]　RBox 角色盒（给 ABox 提供关系推理弹药）
- [[11-vs-kg-db]]　本体 vs 数据库（OWA/CWA 详述）
- [[40-reasoning]]　实例化与一致性推理
