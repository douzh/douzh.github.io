# Protege / HermiT / 知识图谱平台：个人电脑可安装清单

先厘清三者角色，再给清单和选型建议。这三类不是同一层次的东西：

| 类型 | 干什么 | 典型代表 |
|---|---|---|
| **本体编辑器** | 可视化编辑 TBox/RBox/ABox，画类层级、写公理 | Protege |
| **推理器** | 对本体做自动推导（分类、一致性、实例化） | HermiT、Pellet、ELK、Konclude |
| **知识图谱平台** | 存储三元组/图数据、跑 SPARQL/Cypher 查询 | Jena Fuseki、GraphDB、Neo4j、gStore |

---

## 一、Protege（本体编辑器）✅ 强烈推荐，最易装

**基本信息**
- 开发者：斯坦福大学，免费开源（BSD-2），桌面版 + Web 版（WebProtégé）
- 当前版本：5.6.x（官方分发到 5.6.9，winget 上是 5.6.5）
- 系统：**Windows / macOS / Linux 全平台**，自带 GUI
- 运行环境：基于 Java，平台无关版需 **Java 11+（推荐 17）**；Windows/macOS 平台打包版**自带 JRE，无需自己装 Java**，解压/安装即用
- **内置 HermiT 推理器**（Reasoner 菜单直接选），还支持 Pellet、ELK、Konclude 等插件

**核心能力**
- 编辑 TBox（类层级、等价/不相交类）、RBox（属性传递/对称/逆/属性链）、ABox（个体断言）
- DL Query 查询、SPARQL Tab、推理结果查看与解释（Explain）
- 插件生态：SWRLTab（规则）、OntoGraf（图谱可视化）、OWLViz（类层级图）等

**Windows 安装**：官网下载 `Protege-5.6.x-win.zip` → 解压 → 双击 `run.bat` 启动（自带 JRE 无需额外配置）。也可以 `winget install Stanford.Protege`。

---

## 二、HermiT（推理器）⚠️ 它不是独立软件

**基本信息**
- 开发者：牛津大学，开源（LGPL），Java 实现，支持 OWL 2 DL（SROIQ）
- **关键澄清：HermiT 没有独立 GUI 程序**，它是"推理引擎库"，通过三种方式使用：

| 使用方式 | 怎么装/怎么用 | 适合谁 |
|---|---|---|
| **① Protege 内置插件**（最常用） | Protege 已预装，`Reasoner → HermiT → Start reasoner (Ctrl+R)` | 想边建模边看推理结果 |
| **② Java OWL API 集成** | Maven 引 `org.semanticweb:HermiT`，代码里 `HermiTReasoner` 跑分类/查询 | 开发语义应用 |
| **③ Python Owlready2 集成** | `pip install owlready2`，它内置改良版 HermiT/Pellet | Python 用户快速验证 |

**注意**：如果你只想要"推理一下我的本体文件"，最省事路径 = 装 Protege（自带 HermiT），或装 Python 的 Owlready2。

**同类的其他推理器（可个人装）**
- **Pellet**：老牌，支持 SROIQ，有独立 CLI；License 较严格（AGPL）
- **ELK**：专攻 OWL 2 EL，百万级公理秒级分类（如 SNOMED CT 级别）
- **Konclude**：**高性能并行推理器**（OWL 2 DL），LGPL 开源，GitHub 有 Release，命令行即可用，大型本体性能显著优于 HermiT

---

## 三、知识图谱平台（可装个人电脑的完整清单）

按"是否支持 RDF/OWL 语义推理"分成两大类，先上总表：

| 平台 | 类型 | 查询语言 | 免费/开源 | 平台支持 | 安装难度 | 是否支持 OWL 推理 |
|---|---|---|---|---|---|---|
| **Apache Jena + Fuseki** | RDF 三元组库 | SPARQL | ✅ 完全开源 | Win/mac/Linux（需 Java） | ⭐ 低 | ✅（可配 RDFS/OWL 推理） |
| **GraphDB（Ontotext）** | RDF 三元组库 | SPARQL | ✅ 有免费版 | Win/mac/Linux | ⭐ 低 | ✅（内置规则+OWL） |
| **Virtuoso** | RDF 三元组库 | SPARQL | ✅ 开源版 | Win/Linux | ⭐⭐ 中 | ⚠️ 弱（规则推理） |
| **RDF4J** | RDF 框架 | SPARQL | ✅ 开源 | 跨平台（Java） | ⭐⭐ 中 | ✅（库形式） |
| **gStore（北大）** | RDF 原生图库 | SPARQL | ✅ 开源(BSD-3) | 主要 Linux，需编译 | ⭐⭐⭐ 高 | ⚠️ 弱 |
| **Neo4j Community** | 属性图数据库 | Cypher | ✅ 社区版免费 | Win/mac/Linux | ⭐ 低 | ❌ 不支持 RDF/OWL |
| **Owlready2 / RDFLib** | Python 库 | 代码/SPARQL | ✅ 开源 | 跨平台 | ⭐ 低 | ✅（Owlready2 带推理） |

> 划分标准一句话：**想玩 OWL/TBox/RBox/ABox + 推理 → 选 RDF 系（Jena/GraphDB）**；只想存"人和人之间的关系图"、不需要语义推理 → 选 **Neo4j**。

### 重点详介 3 个最值得个人装的

**① Apache Jena + Fuseki（最推荐，RDF 系首选）**
- 免费开源（Apache 2.0），Java 运行，Windows 直接解压 zip 即用
- 自带 Web 控制台（`localhost:3030`），可导入 .ttl/.rdf，直接跑 SPARQL
- 配 `jena-fuseki-inf` 可开启 **RDFS 和 OWL 推理**（对应我们的 TBox/RBox 推导）
- 适合：个人知识图谱、把 Protege 建的本体加载进来做查询验证

**② Neo4j Community + Neo4j Desktop（图数据系首选）**
- 社区版免费；Desktop 是图形化管理器，**Windows 10+ 一键安装**，自带数据库实例和浏览器可视化
- 官方参考配置：Intel Core i3 起步（i7 推荐）、内存 2GB 最低（16GB 推荐）、10GB 磁盘
- 用 Cypher 查询，可视化效果好，是学习/原型最顺手的图数据库
- **局限**：属性图模型，不认 RDF/OWL，没有描述逻辑推理——它与前面的本体体系是"两条路"
- ⚠️ 社区版 vs 企业版：学习完全用社区版；Desktop 另给单机个人使用许可证

**③ gStore（国产，北大）**
- 北大王选所研发的原生 RDF 图数据库，BSD-3 开源，面向"百亿三元组"规模
- 用 SPARQL，子图匹配查询
- **注意**：主要面向 Linux，需要 gcc 9.3+、cmake 3.23.2+、boost 等编译工具链，Windows 上体验差，**不推荐新手在 Windows 装**

---

## 四、按场景的选型建议（个人电脑 / Windows）

| 你的目标 | 推荐组合 |
|---|---|
| 学本体论、练 TBox/RBox/ABox | **只装 Protege**（自带 HermiT，够用了） |
| 建好本体想自动推理/查一致性 | Protege + HermiT 或 ELK（插件） |
| 本体 + 数据放一起查 SPARQL | Protege 建本体 → **Jena Fuseki** 加载查询 |
| 做"人物/商品关系图谱"展示 | **Neo4j Desktop**（Community） |
| Python 快速原型/验证 | **Owlready2**（内置 HermiT/Pellet） |
| 超大本体（医学级）推理 | **Konclude** 或 ELK |

**给 Windows 用户的最小安装路径**（按优先级）：
1. **Protege**（自带 JRE + HermiT）→ 本体建模与推理全搞定；
2. **Jena Fuseki**（需先装 JDK 17）→ 本体落地成可查询的知识图谱；
3. 需要可视化图查询再加 **Neo4j Desktop**。

一句话总结：**Protege 负责"定义知识"，HermiT 负责"推导知识"，知识图谱平台负责"存储和查询知识"**——三者串起来就是一个完整的个人知识图谱工作台，且全部免费可装。

## 相关笔记

- [[61-protege-tutorial]]　Protege 从零建本体实操
- [[62-sparql]]　SPARQL 查询实战
- [[63-protege-vs-trilium]]　Protege vs Trilium 选型对比
- [[40-reasoning]]　推理器是如何工作的
- [[50-vocabulary]]　工具与推理器名称速查