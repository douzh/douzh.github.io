# 语义网 / 本体技术栈词汇表（顶层 → 底层）

按"抽象 → 具体"分层，每个词给出英文全称、中文名、一句话解释。

---

## 第 0 层：应用与语言标准（最顶层）

| 缩写 | 英文全称 | 中文 | 解释 |
|---|---|---|---|
| **OWL 2** | Web Ontology Language 2 | 网络本体语言（第二版） | W3C 官方本体描述语言标准，建立在 RDF/RDFS 之上，是"定义概念与规则"的最高层语言 |
| **OWL 2 DL** | OWL 2 Description Logic | OWL 2 描述逻辑剖面 | 表达力最强的剖面，对应 SROIQ，可判定 |
| **OWL 2 EL** | OWL 2 EL (Existential Logic) | OWL 2 存在性剖面 | 基于 EL++，多项式复杂度，适合大型医学本体（如 SNOMED CT） |
| **OWL 2 QL** | OWL 2 Query Language profile | OWL 2 查询剖面 | 基于 DL-Lite，优化数据库查询重写，适合数据集成 |
| **OWL 2 RL** | OWL 2 Rule Logic profile | OWL 2 规则剖面 | 基于 Datalog 规则，可线性时间推理，适合大规模数据校验 |

> 记忆口诀见下；四个剖面的**详细选型对比**见 [[22-profiles]]。

---

## 第 1 层：逻辑基础（语言背后的数学）

| 缩写 | 英文全称 | 中文 | 解释 |
|---|---|---|---|
| **DL** | Description Logic | 描述逻辑 | 一阶逻辑的可判定子家族，本体语言的语义根基 |
| **SROIQ** | 由字母组成的描述逻辑名称 | 描述逻辑 SROIQ | OWL 2 DL 对应的具体描述逻辑，字母各代表一种能力（见下） |

**SROIQ 五个字母展开：**

| 字母 | 英文全称 | 含义 |
|---|---|---|
| **S** | **S**（ACCR 的复合，源自 ALC + 传递角色） | 基础逻辑 ALC 加上传递角色 |
| **R** | **R**ole hierarchies | 角色层级（子角色、角色包含公理） |
| **O** | **O**nominals | 单例类（用个体直接定义类，如 `{Beijing}`） |
| **I** | **I**nverse roles | 逆角色 |
| **Q** | **Q**ualified number restrictions | 限定基数约束（如"至少有 2 个男性孩子"） |

> 历史上还有 ALC（Attributive Concept Language with Complements）、SHOIN(D) 等；**SROIQ** 是表达力最强、OWL 2 DL 采用的逻辑。

---

## 第 2 层：知识库的构成（本体内部结构）

| 缩写 | 英文全称 | 中文 | 解释 |
|---|---|---|---|
| **KB** | Knowledge Base | 知识库 | 整个本体的统称 = TBox + RBox + ABox |
| **TBox** | Terminological Box | 术语盒 | 概念/类的定义与层级（"X 是什么"） |
| **RBox** | Role Box | 角色盒 | 关系的性质公理：传递、对称、逆、属性链等（"关系有什么规矩"） |
| **ABox** | Assertional Box | 断言盒 | 个体事实：某人属于某类、两者存在某关系（"有哪些事实"） |

---

## 第 3 层：数据模型与基础标准（再往下的"底层语言"）

| 缩写 | 英文全称 | 中文 | 解释 |
|---|---|---|---|
| **RDFS** | RDF Schema | RDF 模式语言 | 给 RDF 加简单语义：`rdfs:subClassOf`、`rdfs:domain/range`。比 OWL 弱，没有推理能力 |
| **RDF** | Resource Description Framework | 资源描述框架 | 用"主语-谓词-宾语"三元组表达事实的通用数据模型，是语义网最底层的数据格式 |
| **IRI** | Internationalized Resource Identifier | 国际化资源标识符 | 全球唯一标识资源的字符串，RDF 中一切实体的"名字"（URI 的中文化扩展） |
| **URI** | Uniform Resource Identifier | 统一资源标识符 | IRI 的前身/子集，传统 Web 地址 |
| **Turtle** | Terse RDF Triple Language | 简洁 RDF 三元组语言 | 人最易读的 RDF 文本语法 |
| **RDF/XML** | RDF in XML | XML 形式 RDF | 机器交换最常用的 RDF 语法 |
| **JSON-LD** | JSON for Linking Data | 链接数据 JSON | 面向 Web 开发者的 RDF JSON 语法 |
| **SPARQL** | SPARQL Protocol and RDF Query Language | RDF 查询语言 | 查询 RDF 数据的"SQL"，通过它查本体/知识图谱 |

**这层的层级关系（也是真正的"从顶到底"）：**

```
OWL 2（语义规则，最高层）
   ↓ 建立在
RDFS（弱语义 schema）
   ↓ 建立在
RDF（三元组数据模型）
   ↓ 用
IRI + Turtle / RDF-XML / JSON-LD（标识符与序列化）承载
   ↓ 最终落在
文件 / 三元组存储（Triple Store）
```

---

## 第 4 层：工具与引擎（工程层）

| 名称 | 英文全称（如为缩写） | 中文 | 解释 |
|---|---|---|---|
| **Protege** | 原名 Protégé，无缩写 | 本体编辑器 | 斯坦福开发的开源本体编辑工具，可视化编辑 TBox/RBox/ABox，内置推理器调用 |
| **OWL API** | OWL API | OWL 编程接口 | Java 操作 OWL 本体的标准库 |
| **Jena** | Apache Jena | Jena 语义网框架 | Java 的 RDF/OWL/SPARQL 全家桶 |
| **OWLready2** | — | — | Python 操作本体的库 |
| **HermiT** | — | — | OWL 2 DL 主流推理器（Java） |
| **Pellet** | — | — | 老牌推理器，支持 SROIQ |
| **ELK** | ELK（EL++ reasoner） | — | 专攻 OWL 2 EL，百万级公理仍很快 |
| **Konclude** | — | — | 高性能推理器，面向大规模 |

---

## 一图总览（含层级关系）

```
应用层     Ontology 本体 / Knowledge Graph 知识图谱
             │
语言层     OWL 2  ←── 四个剖面 DL / EL / QL / RL
             │
逻辑层     SROIQ（DL 描述逻辑的一个成员）
             │
结构层     KB = TBox（概念） + RBox（角色） + ABox（个体）
             │
Schema 层 RDFS
             │
数据层     RDF 三元组
             │
基础层     IRI/URI · Turtle · RDF/XML · JSON-LD · SPARQL
             │
工程层     Protege · OWL API · HermiT · Pellet · ELK
```

---

记忆口诀：
- **OWL 2** 是语言，**SROIQ** 是它背后的数学逻辑；
- **TBox / RBox / ABox** 是本体的三个"抽屉"；
- **RDFS** 和 **RDF** 是更底层的"地基"（RDFS 管 schema，RDF 管数据）；
- **Protege** 是画这些结构的工具，**HermiT/Pellet** 是帮你自动推理的引擎。

## 相关笔记

- [[10-ontology]]　Ontology 是什么（定位与体系）
- [[20-dl]]　描述逻辑 DL / SROIQ 语法与语义
- [[21-owl2]]　OWL 2 语言详解
- [[22-profiles]]　四剖面选型对比
- [[30-tbox]] / [[31-rbox]] / [[32-abox]]　三盒结构详解
- [[60-tools]]　工具与平台安装清单
- [[62-sparql]]　SPARQL 查询实战