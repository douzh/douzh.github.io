# 模型集成的数据需求与案例

[39](39-model-integration-engineering.md) 说明多模型集成的工程落地,本文档详细阐述**各模型的数据需求**并配以完整案例。核心洞察:数据准备难度与模型"原生性"正相关——越原生精确的模型(因果、物理)数据越难准备,越隐式模糊的模型(RAG)门槛越低。这是大模型+RAG 最易落地、多模型全集成最难的根因。

---

## 一、符号模型工具(32)的数据需求

### 1.1 RAG 向量库

| 维度 | 需求 |
| --- | --- |
| 数据形式 | 非结构化文本(PDF/HTML/Markdown/纯文本) |
| 数据量 | 数百到数百万文档 |
| 预处理 | 分块(Chunk)→ 嵌入(Embedding)→ 向量索引 |
| 质量要求 | 文本清晰、内容相关、去重 |
| 更新频率 | 增量更新(新文档加入) |
| 获取难度 | ⭐(最低) |

**技术栈**:
- 嵌入模型:OpenAI text-embedding-3 / BGE / Cohere
- 向量库:Pinecone / Weaviate / Chroma / FAISS
- 分块策略:固定长度 / 语义分块 / 递归分块

**案例:企业知识库 RAG**

```
场景:某公司搭建内部知识问答
数据来源:
  - 产品文档(Confluence,5000 篇)
  - 技术规范(PDF,800 份)
  - 历史工单(Jira 导出,20000 条)
  - 内部 Wiki(Markdown,3000 页)

预处理流水线:
  1. 文档解析:PDF→文本,HTML→纯文本
  2. 分块:每块 500 tokens,重叠 50 tokens
  3. 嵌入:用 BGE-large 生成向量
  4. 入库:Pinecone 索引

查询:"我们的 API 限流策略是什么?"
流程:问题→嵌入→向量检索 Top-5→大模型总结
```

### 1.2 Neo4j 知识图谱

| 维度 | 需求 |
| --- | --- |
| 数据形式 | 结构化三元组(实体,关系,实体) |
| 数据量 | 数千到数百万三元组 |
| 预处理 | 实体抽取 + 关系抽取 + 实体消歧 |
| 质量要求 | 实体唯一、关系准确、本体一致 |
| 更新频率 | 增量更新 + 周期全量校验 |
| 获取难度 | ⭐⭐⭐⭐(高,需抽取管线) |

**技术栈**:
- 图数据库:Neo4j / NebulaGraph / TigerGraph
- 抽取: spaCy / LLM 抽取 / GLiNER
- 查询: Cypher

**案例:医疗知识图谱**

```
场景:某医疗 AI 构建疾病-症状-药物图谱
数据来源:
  - 医学教科书(结构化)
  - 临床指南(半结构化)
  - PubMed 论文(非结构化)
  - 电子病历(脱敏)

抽取流水线:
  1. 实体抽取:疾病、症状、药物、检查项
  2. 关系抽取:疾病-有症状-症状、药物-治疗-疾病
  3. 实体消歧:"糖尿病"与"DM"合并
  4. 入库:Neo4j

图谱片段:
  (糖尿病)-[有症状]->(多饮)
  (糖尿病)-[有症状]->(多尿)
  (二甲双胍)-[治疗]->(糖尿病)
  (胰岛素)-[治疗]->(糖尿病)

查询:"糖尿病患者用什么药?"
Cypher:
  MATCH (d:Disease {name:"糖尿病"})<-[:治疗]-(m:Drug)
  RETURN m.name
```

### 1.3 SQL 数据库

| 维度 | 需求 |
| --- | --- |
| 数据形式 | 结构化表格(行列数据) |
| 数据量 | 数万到数亿行 |
| 预处理 | Schema 设计 + 数据清洗 + ETL |
| 质量要求 | 数据一致、无空值、主键唯一 |
| 获取难度 | ⭐⭐(低-中) |

**案例:电商用户分析**

```
场景:Text-to-SQL 查询
数据来源:用户表、订单表、商品表

表结构:
  users(id, name, age, region, register_date)
  orders(id, user_id, product_id, amount, order_date)
  products(id, name, category, price)

查询:"上个月华东地区销量 Top10 商品"
大模型生成 SQL:
  SELECT p.name, SUM(o.amount) as total
  FROM orders o
  JOIN users u ON o.user_id = u.id
  JOIN products p ON o.product_id = p.id
  WHERE u.region = '华东'
    AND o.order_date >= '2025-06-01'
  GROUP BY p.name
  ORDER BY total DESC
  LIMIT 10
```

### 1.4 Code Interpreter

| 维度 | 需求 |
| --- | --- |
| 数据形式 | 代码 + 数据文件(CSV/JSON/Excel) |
| 数据量 | KB 到 GB 级 |
| 预处理 | 数据清洗 + 格式标准化 |
| 质量要求 | 数据可被 pandas 读取 |
| 获取难度 | ⭐⭐(低-中) |

**案例:销售数据分析**

```
场景:用户上传销售 CSV,让大模型分析
数据:sales.csv (date, region, product, quantity, revenue)

用户:"分析哪个地区增长最快,预测下季度"
流程:
  1. 大模型生成 Python 代码
  2. Code Interpreter 执行:
     - 读取 CSV
     - 按地区分组计算增长率
     - 用 statsmodels 做时序预测
  3. 返回结果 + 图表
```

---

## 二、物理仿真工具(34)的数据需求

### 2.1 通用需求

| 维度 | 需求 |
| --- | --- |
| 几何模型 | URDF/SDF/USD 格式的 3D 模型 |
| 物理参数 | 质量、惯性、摩擦、弹性、阻尼 |
| 环境参数 | 重力、空气密度、光照 |
| 控制接口 | 关节角度/力矩输入 |
| 获取难度 | ⭐⭐⭐(中,需厂商或测量) |

### 2.2 MuJoCo

| 维度 | 需求 |
| --- | --- |
| 模型文件 | XML 格式(MJCF) |
| 参数 | 关节类型、惯量、执行器 |
| 数据来源 | 机器人厂商规格表 / CAD 模型 |

**案例:机械臂抓取仿真**

```xml
<!-- robot_arm.xml -->
<mujoco>
  <worldbody>
    <body name="base" pos="0 0 0">
      <inertial mass="2.0" pos="0 0 0.05"
                diaginertia="0.01 0.01 0.01"/>
      <geom type="cylinder" size="0.05 0.1"/>
      <body name="link1" pos="0 0 0.1">
        <joint name="j1" type="hinge" axis="0 1 0">
          <limit range="-3.14 3.14"/>
          <dynamics damping="0.1" friction="0.05"/>
        </joint>
        <inertial mass="1.5" pos="0 0 0.15"
                  diaginertia="0.005 0.005 0.005"/>
        <geom type="capsule" fromto="0 0 0 0 0 0.3" size="0.03"/>
      </body>
    </body>
  </worldbody>
  <actuator>
    <motor joint="j1" gear="1"/>
  </actuator>
</mujoco>
```

```python
import mujoco

model = mujoco.MjModel.from_xml_path("robot_arm.xml")
data = mujoco.MjData(model)

# 仿真 1000 步
for _ in range(1000):
    data.ctrl[0] = 0.5  # 关节力矩
    mujoco.mj_step(model, data)
    print(f"末端位置: {data.body('link1').xpos}")
```

### 2.3 Isaac Sim

| 维度 | 需求 |
| --- | --- |
| 场景文件 | USD 格式 |
| 传感器模型 | 相机/LiDAR 参数 |
| 资产 | 3D 物体模型、材质库 |
| 数据来源 | NVIDIA Omniverse 资产库 |

**案例:自动驾驶仿真**

```
场景:某自动驾驶公司训练感知模型
数据来源:
  - 车辆 3D 模型(USD)
  - 道路场景(城市/高速/停车场)
  - 行人模型 + 动作库
  - 天气系统(雨/雪/雾)
  - 传感器配置(相机/LiDAR/Radar)

仿真流程:
  1. 构建场景:道路 + 车辆 + 行人 + 天气
  2. 配置传感器:8 个相机 + 1 LiDAR
  3. 运行场景:1000 小时多样化数据
  4. 生成标注:GT 边界框、深度图、语义分割
  5. 训练感知模型

输出:
  - 100 万张标注图像
  - 10 万段 LiDAR 点云
  - 5000 个长尾场景(紧急刹车、行人闯入)
```

---

## 三、因果推理工具(35)的数据需求

### 3.1 通用需求

| 维度 | 需求 |
| --- | --- |
| 观测数据 | DataFrame(变量列) |
| 因果图 | 领域专家构建的有向无环图(DAG) |
| 处理变量 | 二值或连续 |
| 结局变量 | 连续/二值/生存时间 |
| 混杂变量 | 影响处理和结局的变量 |
| 获取难度 | ⭐⭐⭐⭐⭐(最高,因果图需专家) |

### 3.2 DoWhy

**案例:药物疗效因果评估**

```python
import dowhy
from dowhy import CausalModel
import pandas as pd

# 数据:1000 名患者
df = pd.DataFrame({
    "age": [...],           # 年龄
    "severity": [...],      # 病情严重度
    "treatment": [...],     # 是否用药(0/1)
    "outcome": [...],       # 康复率(0-1)
})

# 因果图(专家构建)
causal_graph = """
digraph {
    age -> treatment
    age -> outcome
    severity -> treatment
    severity -> outcome
    treatment -> outcome
}
"""

model = CausalModel(
    data=df,
    treatment="treatment",
    outcome="outcome",
    graph=causal_graph
)

# 识别 + 估计
identified = model.identify_effect()
estimate = model.estimate_effect(
    identified,
    method_name="backdoor.propensity_score_matching"
)

print(f"平均处理效应 ATE: {estimate.value:.3f}")
# 输出:平均处理效应 ATE: 0.350

# 反驳检验
refute = model.refute_estimate(
    identified, estimate,
    method_name="placebo_treatment_refuter"
)
```

**数据准备关键点**:
- **因果图是最关键的输入**,决定哪些是混杂变量、如何调整
- 因果图错误 → 结论错误(如遗漏重要混杂变量)
- 因果图来源:① 领域专家手工构建;② 因果发现算法(不可靠,需专家校验)

### 3.3 CausalNex

**案例:客户流失因果分析**

```python
from causalnex.structure import StructureModel
from causalnex.plots import plot_structure

# 数据:客户行为 + 是否流失
df = pd.DataFrame({
    "usage_freq": [...],    # 使用频率
    "support_calls": [...], # 客服来电
    "tenure": [...],        # 在网时长
    "monthly_fee": [...],   # 月费
    "churn": [...],         # 是否流失
})

# 结构学习(从数据学因果图)
sm = StructureModel()
sm.add_edges_from([
    ("usage_freq", "churn"),
    ("support_calls", "churn"),
    ("tenure", "usage_freq"),
    ("monthly_fee", "support_calls"),
])

# 可视化 + 专家校验
plot_structure(sm)

# 干预分析:"如果减少客服来电,流失率降多少?"
from causalnex.inference import InferenceEngine
ie = InferenceEngine(sm)
ie.query({"churn": 1})  # 当前流失概率
ie.do_intervention("support_calls", 0)  # 干预
ie.query({"churn": 1})  # 干预后流失概率
```

**关键点**:CausalNex 能从数据学因果图,但**必须经领域专家校验**——算法发现的因果方向可能错误。

---

## 四、概率推理工具(33)的数据需求

### 4.1 通用需求

| 维度 | 需求 |
| --- | --- |
| 观测数据 | 样本数组/DataFrame |
| 先验分布 | 领域知识编码为概率分布 |
| 模型结构 | 变量间概率依赖关系 |
| 获取难度 | ⭐⭐⭐(中,先验需领域知识) |

### 4.2 PyMC

**案例:贝叶斯 A/B 测试**

```python
import pymc as pm
import numpy as np

# 数据:两组用户的转化记录
n_A, conv_A = 10000, 320      # A 组:10000 访问,320 转化
n_B, conv_B = 10000, 380      # B 组:10000 访问,380 转化

with pm.Model() as ab_test:
    # 先验:无信息 Beta(1,1)
    p_A = pm.Beta("p_A", alpha=1, beta=1)
    p_B = pm.Beta("p_B", alpha=1, beta=1)

    # 似然
    pm.Binomial("obs_A", n=n_A, p=p_A, observed=conv_A)
    pm.Binomial("obs_B", n=n_B, p=p_B, observed=conv_B)

    # 关心的是差异
    delta = pm.Deterministic("delta", p_B - p_A)

    # 推断
    trace = pm.sample(2000, chains=4)

# 结果
print(f"P(B 优于 A) = {(trace.posterior['delta'] > 0).mean():.2%}")
# 输出:P(B 优于 A) = 96.50%
print(f"差值 95% CI: {np.percentile(trace.posterior['delta'], [2.5, 97.5])}")
```

**数据准备关键点**:
- **先验分布是核心输入**,反映领域知识
- 无信息先验(Beta(1,1))让数据说话,但样本少时可能不稳定
- 有信息先验(如基于历史数据)能提升小样本表现,但需领域知识

### 4.3 Pyro

**案例:时序预测(贝叶斯结构时序)**

```python
import pyro
import pyro.distributions as dist
import torch

# 数据:100 天的销量
sales = torch.tensor([120, 135, 128, ...])

def model(sales):
    # 先验:趋势斜率
    trend = pyro.sample("trend", dist.Normal(0, 1))
    # 先验:季节性
    season = pyro.sample("season", dist.Normal(0, 5))
    # 观测
    for t in range(len(sales)):
        mu = trend * t + season * torch.sin(2 * 3.14 * t / 7)
        pyro.sample(f"obs_{t}", dist.Normal(mu, 10),
                    obs=sales[t])

# 推断
pyro.clear_param_store()
svi = pyro.infer.SVI(
    model, guide,
    optim, loss=pyro.infer.Trace_ELBO()
)
for step in range(1000):
    svi.step(sales)
```

---

## 五、类比检索工具(36)的数据需求

### 5.1 通用需求

| 维度 | 需求 |
| --- | --- | --- |
| 案例库 | (情境,行动,结果)三元组 |
| 嵌入方式 | 语义向量化 |
| 索引 | 向量索引(FAISS/Chroma) |
| 更新 | 新案例增量加入 |
| 获取难度 | ⭐⭐⭐(中,需积累) |

### 5.2 案例:医疗病例类比

```python
from langchain.vectorstores import Chroma
from langchain.embeddings import OpenAIEmbeddings

# 案例库:历史病例
cases = [
    {
        "情境": "65岁男性,胸痛3小时,ST段抬高",
        "行动": "立即冠脉造影 + 支架",
        "结果": "康复,住院5天"
    },
    {
        "情境": "72岁女性,胸闷气短,T波倒置",
        "行动": "药物保守治疗",
        "结果": "症状缓解,住院7天"
    },
    # ... 10000 个病例
]

# 向量化
embeddings = OpenAIEmbeddings()
db = Chroma.from_texts(
    [f"{c['情境']} | {c['行动']} | {c['结果']}" for c in cases],
    embeddings
)

# 类比检索:新患者
new_case = "68岁男性,胸痛2小时,ST段抬高"
similar = db.similarity_search(new_case, k=5)
# 返回:最相似的 5 个历史病例
# 大模型基于这些病例生成建议
```

**关键点**:
- 案例库的质量决定类比效果
- 案例需要结构化(情境-行动-结果),不能只是文本
- 越相似的案例越有参考价值,需调优相似度阈值

---

## 六、多智能体(37)的数据需求

### 6.1 通用需求

| 维度 | 需求 |
| --- | --- |
| 角色定义 | 系统提示词 |
| 任务描述 | 目标 + 约束 |
| 协作协议 | 交互规则 |
| 共享状态 | 黑板/消息队列 |
| 获取难度 | ⭐(最低,主要是设计) |

### 6.2 案例:多智能体辩论

```python
from autogen import AssistantAgent, UserProxyAgent, GroupChat

# 角色定义(核心"数据")
analyst = AssistantAgent(
    "分析师",
    system_message="""你是数据分析师。
    职责:分析数据、计算指标。
    输出格式:指标 + 数值 + 解读。"""
)

critic = AssistantAgent(
    "质疑者",
    system_message="""你是批判性思考者。
    职责:质疑分析结论、指出漏洞。
    必须提出至少3个反驳点。"""
)

judge = AssistantAgent(
    "裁判",
    system_message="""你是中立裁判。
    职责:综合双方观点、给出最终结论。
    必须列出双方论点 + 置信度。"""
)

user = UserProxyAgent("用户", human_input_mode="NEVER")

# 协作协议
group = GroupChat(
    agents=[analyst, critic, judge, user],
    messages=[],
    max_round=10
)

manager = AssistantAgent(
    "协调员",
    groupchat=group,
    system_message="管理讨论流程,确保每个角色发言。"
)

# 执行
user.initiate_chat(
    manager,
    message="分析:Q2 华东区销售额同比下降 15%,原因是什么?"
)
```

**关键点**:多智能体的"数据"主要是**角色定义和协作协议**,不是传统数据集。难度在**设计**,不在数据获取。

---

## 七、按数据性质分类的总览

| 数据类型 | 支持的模型 | 获取难度 | 核心难点 |
| --- | --- | --- | --- |
| **非结构化文本** | 32 RAG | ⭐ | 文本清洗、分块策略 |
| **结构化表格** | 32 SQL / 33 / 35 | ⭐⭐ | 数据清洗、Schema 设计 |
| **知识三元组** | 32 图谱 | ⭐⭐⭐⭐ | 实体抽取、关系抽取、消歧 |
| **物理参数 + 几何** | 34 仿真 | ⭐⭐⭐ | 厂商规格、精确测量 |
| **因果图** | 35 因果 | ⭐⭐⭐⭐⭐ | 需领域专家,算法不可靠 |
| **先验分布** | 33 概率 | ⭐⭐⭐ | 需领域知识编码 |
| **案例库** | 36 类比 | ⭐⭐⭐ | 需长期积累,质量决定效果 |
| **角色定义** | 37 涌现 | ⭐ | 主要是设计工作 |

---

## 八、数据准备的难点排序

| 难度 | 数据类型 | 难点 | 对应模型 |
| --- | --- | --- | --- |
| ⭐⭐⭐⭐⭐ | **因果图** | 需领域专家构建,算法发现不可靠 | 35 因果 |
| ⭐⭐⭐⭐ | **知识图谱** | 需从文本抽取实体和关系,人工标注成本高 | 32 符号显式 |
| ⭐⭐⭐⭐ | **物理参数** | 需厂商提供或物理测量,精度影响仿真可信度 | 34 物理 |
| ⭐⭐⭐ | **案例库** | 需长期积累,质量决定类比效果 | 36 类比 |
| ⭐⭐⭐ | **先验分布** | 需领域知识编码,选择不当影响推断 | 33 概率 |
| ⭐⭐ | **结构化表格** | 需清洗和整合,但格式标准 | 32 SQL |
| ⭐ | **非结构化文本** | RAG 直接用,门槛最低 | 32 RAG |
| ⭐ | **角色定义** | 主要是设计工作,无数据获取 | 37 涌现 |

**核心洞察**:**数据获取难度与模型"原生性"正相关**——越原生精确的模型(因果、物理),数据准备越难;越隐式模糊的模型(31 大模型/RAG),数据门槛越低。这也是为什么大模型+RAG 是最易落地的组合,而多模型全集成最难。

---

## 九、完整多模型系统的数据流案例

### 9.1 医疗诊断系统

以医疗诊断为例,展示多模型系统的完整数据流:

```
患者输入(主诉:胸痛3小时)
   │
   ├─ RAG:医学指南文本 ────► 文档库(32 符号)
   │   数据:1000 份临床指南 PDF
   │   作用:提供标准诊疗路径
   │
   ├─ 知识图谱:疾病-症状-药物 ──► 三元组(32 符号显式)
   │   数据:50 万三元组
   │   作用:结构化查询疾病关系
   │
   ├─ 影像识别:CT/MRI ──────► 医学影像数据集(31 隐式)
   │   数据:10 万张标注 CT
   │   作用:识别病灶
   │
   ├─ 检验数据:血常规/生化 ─► 结构化表格(32 SQL)
   │   数据:患者检验结果 DataFrame
   │   作用:异常值检测
   │
   ├─ 因果推断:治疗→结局 ──► 因果图 + 观测数据(35)
   │   数据:10000 例历史病例 + 专家因果图
   │   作用:评估治疗方案的因果效应
   │
   ├─ 概率推断:疾病概率 ───► 先验 + 似然(33)
   │   数据:流行病学先验 + 症状似然
   │   作用:计算各疾病的后验概率
   │
   └─ 类比:历史病例 ───────► 病例库(36)
       数据:50000 例结构化病例
       作用:参考相似病例的诊疗方案
```

**输出示例**:
```json
{
  "诊断": "急性心肌梗死",
  "置信度": 0.92,
  "依据": {
    "影像": "前壁 ST 段抬高",
    "检验": "肌钙蛋白 I 升高",
    "概率推断": "后验概率 92%",
    "类比": "相似病例 348 例,72% 采取急诊 PCI"
  },
  "因果评估": {
    "急诊 PCI vs 保守治疗": "ATE +0.35(康复率)",
    "依据": "DoWhy 分析,混杂已调整"
  },
  "推荐方案": "急诊 PCI",
  "指南参考": "《急性 ST 段抬高型心肌梗死诊断和治疗指南》2019"
}
```

### 9.2 金融风控系统

```
贷款申请输入
   │
   ├─ RAG:风控政策文档 ────► 32 符号
   ├─ 知识图谱:企业关系图谱 ─► 32 符号显式
   ├─ 表格数据:申请人征信 ──► 32 SQL
   ├─ 因果推断:特征→违约 ──► 35 因果
   ├─ 概率推断:违约概率 ───► 33 概率
   └─ 类比:历史违约案例 ───► 36 类比
```

### 9.3 科学发现系统

```
研究问题:某种蛋白质的功能
   │
   ├─ RAG:文献库 ─────────► 32 符号
   ├─ 知识图谱:蛋白质-基因-疾病 ─► 32 符号显式
   ├─ 物理仿真:蛋白质折叠 ──► 34 物理
   ├─ 因果推断:基因敲除→表型 ─► 35 因果
   ├─ 类比:同源蛋白功能 ───► 36 类比
   └─ 多智能体:假说-实验-验证循环 ─► 37 涌现
```

---

## 十、一句话总结

> **各模型的数据需求差异巨大:符号模型(32)需非结构化文本(RAG,⭐)或结构化三元组(知识图谱,⭐⭐⭐⭐);物理仿真(34)需物理参数+几何模型 URDF/CAD(⭐⭐⭐);因果推理(35)需观测数据+因果图(因果图需专家构建,⭐⭐⭐⭐⭐最难);概率推理(33)需观测数据+先验分布(需领域知识,⭐⭐⭐);类比检索(36)需案例库(需积累,⭐⭐⭐);多智能体(37)需角色定义(⭐最易)。数据准备难度与模型"原生性"正相关——越原生精确的模型(因果、物理)数据越难准备,越隐式模糊的模型(RAG)门槛越低。这是大模型+RAG 最易落地、多模型全集成最难的根因,也是 [39](39-model-integration-engineering.md) L1→L2→L3 渐进路径的工程依据:从 RAG 起步,逐步引入更难准备的物理/因果/概率模型。完整多模型系统的数据流以医疗诊断为例:RAG(指南)+ 知识图谱(疾病关系)+ 影像(病灶)+ SQL(检验)+ 因果(治疗效应)+ 概率(疾病概率)+ 类比(历史病例)七种数据协同,输出结构化诊断结果。**
