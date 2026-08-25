# SPARQL 查询实战：从本体/知识图谱里取数

## 一句话定位

**SPARQL 是 RDF 数据的查询语言（相当于"语义网的 SQL"）**，通过三元组模式匹配来取数；配合推理器，它能查到"未显式存储、由本体规则推出"的隐含事实。本页用家族数据做例子，从入门到推理查询。

---

## 一、SPARQL 的语法骨架

```
PREFIX : <http://example.org/family#>
SELECT ?person ?age
WHERE {
  ?person :hasAge ?age .
}
```

| 关键词 | 作用 |
|---|---|
| `PREFIX` | 定义命名空间缩写（必须，不然写全 IRI）|
| `SELECT` | 返回哪些变量（`*` 返回全部）|
| `WHERE { }` | 写三元组模式（变量以 `?` 开头）|
| `FILTER` | 过滤（比较、正则）|
| `OPTIONAL` | 可选的匹配（左连接）|
| `UNION` | 并集 |
| `ORDER BY / LIMIT` | 排序 / 限量 |

---

## 二、基础查询：选数据

### 全部三元组
```sparql
PREFIX : <http://example.org/family#>
SELECT * WHERE { ?s ?p ?o }
```

### 查某人的孩子
```sparql
PREFIX : <http://example.org/family#>
SELECT ?child WHERE { :zhangsan :hasChild ?child }
```

### 查所有"人"类成员 + 年龄（带过滤）
```sparql
PREFIX : <http://example.org/family#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?person ?age
WHERE {
  ?person a :Person ;
          :hasAge ?age .
  FILTER(?age > 18)
}
ORDER BY DESC(?age)
```

> `;` 表示同一主语继续写，等价于 `?person a :Person . ?person :hasAge ?age .`

---

## 三、图模式：关系查询

### 查"孙子"（两条边，二阶路径）
```sparql
PREFIX : <http://example.org/family#>
SELECT ?grandchild
WHERE {
  :laozhang :hasChild ?parent .
  ?parent   :hasChild ?grandchild .
}
```

### OPTIONAL：查所有人及其年龄（没有年龄也返回人，年龄为空）
```sparql
PREFIX : <http://example.org/family#>
SELECT ?p ?age WHERE {
  ?p a :Person .
  OPTIONAL { ?p :hasAge ?age }
}
```

### UNION：查"是男人或女人"的人
```sparql
PREFIX : <http://example.org/family#>
SELECT DISTINCT ?p WHERE {
  { ?p a :Man } UNION { ?p a :Woman }
}
```

---

## 四、聚合与计数

```sparql
PREFIX : <http://example.org/family#>
SELECT ?person (COUNT(?child) AS ?childCount)
WHERE {
  ?person :hasChild ?child .
}
GROUP BY ?person
HAVING (COUNT(?child) > 1)     # 只要孩子多于 1 个的
```

---

## 五、ASK / CONSTRUCT / DESCRIBE（非 SELECT 查询）

```sparql
# ASK：只问"有没有"
PREFIX : <http://example.org/family#>
ASK { :zhangsan :hasChild :lisi }        # → true / false

# CONSTRUCT：把查询结果建成新的 RDF 图（可用于数据转换/导出）
PREFIX : <http://example.org/family#>
CONSTRUCT { ?p :hasOffspring ?c }
WHERE { ?p :hasChild ?c }
```

---

## 六、推理查询：查"本体推出来的事实"

这是 SPARQL 与本体结合最有价值的部分。要在**带推理的三元组库**（如 Jena Fuseki 的 `-inf` 模式）里跑：

### 查所有祖先（利用 `ancestorOf` 的传递性）
```sparql
PREFIX : <http://example.org/family#>
SELECT ?ancestor WHERE { :zhangsan :ancestorOf ?ancestor }
```
如果只存了"直接父链"，配合 `Transitive(ancestorOf)`，推理器会补出全部祖先——**不推理时这条查询只返回直接祖先。**

### 查所有"父亲"（利用 TBox 充分定义）
```sparql
PREFIX : <http://example.org/family#>
SELECT ?f WHERE { ?f a :Father }
```
配合 `Father ≡ Man ⊓ ∃hasChild.Person` 的推理，能返回**未显式标注**为 Father、但满足定义的男人。

---

## 七、在哪跑 SPARQL

| 环境 | 怎么跑 | 适合 |
|---|---|---|
| **Protege SPARQL Tab** | 菜单 `Tabs → SPARQL Query` | 本体文件快速验证 |
| **Jena Fuseki** | 启动 Web 控制台 `localhost:3030`，选 `-inf` 数据集 | 推理 + 查询一体化 |
| **GraphDB** | Workbench 自带 SPARQL 面板 | 大型知识图谱 |
| **Python rdflib** | `pip install rdflib`，代码里 `graph.query()` | 脚本/管道 |
| **DBpedia endpoint** | 在线 `https://dbpedia.org/sparql` | 练手/公开数据 |

---

## 记忆口诀

- **SELECT + WHERE 三元组** = 基础姿势；
- **FILTER/OPTIONAL/UNION** = 过滤、留空、合并；
- **推理查询** = 到带推理的库（Fuseki -inf）里查，隐含事实才会现身。

一句话总结：**SPARQL 用"三元组模式匹配"取数，普通查询查显式数据、推理查询查隐含数据；建好本体（[[21-owl2]]）+ 灌进带推理的库（[[60-tools]]），它就是你知识图谱的"万能取数口"。**

## 相关笔记

- [[50-vocabulary]]　SPARQL/RDF 词条速查
- [[21-owl2]]　本体的 RDF 序列化（查询的落点）
- [[40-reasoning]]　推理与查询的关系
- [[60-tools]]　Jena Fuseki / GraphDB 安装
- [[61-protege-tutorial]]　Protege 里的 SPARQL Tab
