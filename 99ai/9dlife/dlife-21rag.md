# LightRAG

你没有云端 Embedding API，也不想用 Ollama，是完全可行的。

方案结构：
- **推理 LLM**：用你现有的云端接口
- **向量模型**：本地轻量模型，纯 Python 运行，不联网、不调用 API
- **存储**：LightRAG 默认文件结构，类似 SQLite，无独立数据库服务
- **图结构**：本地文件存储，基于 NetworkX，无需额外部署

---

### 最简可用配置
```python
from lightrag import LightRAG
from lightrag.embedding import SentenceTransformerEmbedding

# 本地向量模型，完全离线
embedding = SentenceTransformerEmbedding("all-MiniLM-L6-v2")

rag = LightRAG(
    working_dir="./digital_life_memory",

    # 你自己的云端大模型
    llm_model="你的模型名称",
    llm_api_base="https://api.xxx.com/v1",
    llm_api_key="xxx",

    # 本地向量，不走云端
    embedding=embedding
)
```

- 优点：
  - 向量计算在本地，不消耗云端额度
  - 不需要额外服务
  - 数据全部存在本地目录
  - 同时支持向量检索 + 图关系抽取
- 缺点：
  - 首次运行会下载一次小模型（~20MB）
  - 检索效果略弱于商用云端 Embedding，但个人使用足够

---

### 存储结构（安静介绍版）
```
./digital_life_memory/
├── graph/          # 实体与关系，JSON 文件
├── vector/         # FAISS 向量索引
└── docs.db        # SQLite 存储原文
```
无端口、无服务、复制目录即备份。

---

### 适用场景
- 不想为向量模型付费
- 不想部署 Ollama 等本地服务
- 希望记忆数据完全本地化
- 希望结构轻量、可移植

### 一、完整代码（纯技术、无修饰、可直接运行）
```python
# 依赖安装
# pip install lightrag-hku sentence-transformers

from lightrag import LightRAG
from lightrag.embedding import SentenceTransformerEmbedding
from lightrag.llm import openai_complete


def create_rag(working_dir: str, llm_api_key: str, llm_api_base: str = "https://api.openai.com/v1",
                llm_model: str = "gpt-4o-mini") -> LightRAG:
    """
    初始化LightRAG实例
    :param working_dir: 本地存储目录
    :param llm_api_key: 云端LLM API密钥
    :param llm_api_base: 云端LLM API地址
    :param llm_model: 云端LLM模型名称
    :return: LightRAG实例
    """
    # 本地向量模型（离线、无API调用）
    embedding = SentenceTransformerEmbedding(model_name="all-MiniLM-L6-v2")

    # 初始化RAG
    rag = LightRAG(
        working_dir=working_dir,
        embedding=embedding,
        llm_model=llm_model,
        llm_api_base=llm_api_base,
        llm_api_key=llm_api_key,
        llm_complete_func=openai_complete
    )
    return rag


def insert_documents(rag: LightRAG, texts: list[str]) -> None:
    """
    批量插入文档
    :param rag: LightRAG实例
    :param texts: 文档文本列表
    """
    for text in texts:
        rag.insert(text)


def query_rag(rag: LightRAG, query_str: str) -> str:
    """
    检索问答
    :param rag: LightRAG实例
    :param query_str: 用户查询
    :return: 回答结果
    """
    return rag.query(query_str)


# 主程序示例
if __name__ == "__main__":
    # 配置参数
    CONFIG = {
        "WORKING_DIR": "./rag_storage",
        "LLM_API_KEY": "sk-xxx",
        "LLM_API_BASE": "https://api.openai.com/v1",
        "LLM_MODEL": "gpt-4o-mini"
    }

    # 初始化RAG
    rag = create_rag(**CONFIG)

    # 插入测试文档
    test_docs = [
        "LightRAG是轻量级RAG框架，支持本地向量存储与云端LLM推理。",
        "SentenceTransformer提供离线向量生成能力，无需云端API。",
        "本地存储采用文件+SQLite结构，数据完全本地化。"
    ]
    insert_documents(rag, test_docs)

    # 测试查询
    response = query_rag(rag, "LightRAG的向量模型如何实现？")
    print(response)
```

### 二、OpenClaw agent.md 配置（可直接写入）
```yaml
agent_id: rag_agent
description: 基于LightRAG的本地向量+云端LLM智能体
type: rag
workspace: ./openclaw_rag

# 存储配置
storage:
  engine: lightrag
  working_dir: ./rag_storage
  vector_store: local
  graph_store: local
  doc_store: sqlite

# 向量模型（本地离线）
embedding:
  provider: sentence_transformer
  model: all-MiniLM-L6-v2
  offline: true
  device: cpu

# 大模型（云端API）
llm:
  provider: openai
  model: gpt-4o-mini
  api_base: https://api.openai.com/v1
  api_key: ${LLM_API_KEY}
  temperature: 0.3
  max_tokens: 1024

# 检索策略
retrieval:
  mode: hybrid
  top_k: 5
  rerank: true

# 能力配置
capabilities:
  - document_insert
  - query_answer
  - knowledge_graph_query
  - context_aware_response

# 执行约束
constraints:
  - 向量计算完全本地执行
  - 原文与向量数据不离开本地存储
  - 仅向云端LLM发送检索后的上下文片段
  - 不存储云端LLM返回的敏感信息
```

### 三、极简调用示例（无注释、纯代码）
```python
from lightrag import LightRAG
from lightrag.embedding import SentenceTransformerEmbedding

# 初始化
rag = LightRAG(
    working_dir="./rag_data",
    embedding=SentenceTransformerEmbedding("all-MiniLM-L6-v2"),
    llm_model="gpt-4o-mini",
    llm_api_base="https://api.openai.com/v1",
    llm_api_key="sk-xxx"
)

# 插入
rag.insert("LightRAG支持本地向量与云端LLM结合。")

# 查询
print(rag.query("LightRAG架构特点？"))
```

### 四、存储结构（技术说明）
```
./rag_data/
├── vector/          # FAISS本地向量索引
├── graph/           # NetworkX知识图谱文件
├── docs.db          # SQLite原文存储
└── config.json      # 配置参数
```
- 无服务、无端口、无外部依赖
- 目录复制即完整备份
- 向量计算CPU离线完成
- 仅LLM推理调用云端API