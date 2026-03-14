# c3-3 向量数据库与语义搜索

## 1. 概述

向量数据库将文本、图像等数据转换为向量表示，通过计算向量相似度实现语义搜索。本章详解 Embedding 技术、相似度算法和主流向量数据库的使用。

## 2. 核心概念

### 2.1 Embedding 原理

```javascript
// 文本 → 向量
const text = "人工智能改变世界";
const embedding = await getEmbedding(text);
// 输出：[0.12, -0.45, 0.78, ..., 0.34] (768 维或 1536 维)

// 相似文本的向量距离相近
const similar1 = "AI 技术影响生活";  // 向量距离近
const different1 = "今天天气不错";    // 向量距离远
```

### 2.2 相似度度量

| 算法 | 公式 | 适用场景 |
|------|------|---------|
| **余弦相似度** | cos(θ) = A·B / (||A||×||B||) | 最常用，忽略向量长度 |
| **欧氏距离** | d = √Σ(Ai-Bi)² | 考虑绝对距离 |
| **点积** | A·B = ΣAiBi | 推荐系统 |

## 3. 主流向量数据库

### 3.1 ChromaDB（轻量级）

```javascript
import { ChromaClient } from 'chromadb';

class ChromaMemory {
  constructor() {
    this.client = new ChromaClient({
      path: 'http://localhost:8000'
    });
  }
  
  async initCollection(name) {
    const collection = await this.client.getOrCreateCollection({
      name: name,
      metadata: { "hnsw:space": "cosine" }
    });
    return collection;
  }
  
  async addMemories(collection, memories) {
    // memories: [{id, text, metadata}]
    await collection.add({
      ids: memories.map(m => m.id),
      documents: memories.map(m => m.text),
      metadatas: memories.map(m => m.metadata),
    });
  }
  
  async searchSimilar(collection, query, limit = 5) {
    const results = await collection.query({
      queryTexts: [query],
      nResults: limit,
    });
    
    return results.documents[0].map((doc, i) => ({
      content: doc,
      score: results.distances[0][i],
      metadata: results.metadatas[0][i]
    }));
  }
}

// 使用示例
const chroma = new ChromaMemory();
const collection = await chroma.initCollection('user_memories');

await chroma.addMemories(collection, [
  { id: 'mem1', text: '用户喜欢简洁的回答', metadata: { userId: 'u123' } },
  { id: 'mem2', text: '用户对 Python 编程感兴趣', metadata: { userId: 'u123' } }
]);

const similar = await chroma.searchSimilar(collection, '简短的回复', 3);
console.log(similar);
```

### 3.2 Pinecone（云端服务）

```javascript
import { Pinecone } from '@pinecone-database/pinecone';

class PineconeMemory {
  constructor(apiKey, environment) {
    this.pc = new Pinecone({ apiKey });
    this.environment = environment;
  }
  
  async initIndex(indexName) {
    await this.pc.createIndex({
      name: indexName,
      dimension: 1536, // OpenAI embedding 维度
      metric: 'cosine'
    });
    
    this.index = this.pc.Index(indexName);
  }
  
  async upsertVectors(vectors) {
    // vectors: [{id, values: [], metadata: {}}]
    await this.index.upsert(vectors);
  }
  
  async querySimilar(queryVector, topK = 5) {
    const response = await this.index.query({
      vector: queryVector,
      topK: topK,
      includeMetadata: true,
      includeValues: false
    });
    
    return response.matches;
  }
}
```

### 3.3 Weaviate（开源强大）

```javascript
import weaviate from 'weaviate-ts-client';

class WeaviateMemory {
  constructor() {
    this.client = weaviate.client({
      scheme: 'http',
      host: 'localhost:8080',
    });
  }
  
  async createClass(className) {
    await this.client.schema
      .classCreator()
      .withClass({
        className: className,
        properties: [
          { name: 'content', dataType: ['text'] },
          { name: 'userId', dataType: ['string'] },
          { name: 'timestamp', dataType: ['date'] }
        ],
        vectorizer: 'text2vec-transformers'
      })
      .do();
  }
  
  async addObject(className, obj) {
    return await this.client.data
      .creator()
      .withClassName(className)
      .withProperties(obj)
      .do();
  }
  
  async semanticSearch(className, query, limit = 5) {
    return await this.client.graphql
      .get()
      .withClassName(className)
      .withNearText({ concepts: [query] })
      .withLimit(limit)
      .do();
  }
}
```

## 4. 实战应用

### 4.1 语义缓存

```javascript
class SemanticCache {
  constructor(vectorDB, similarityThreshold = 0.9) {
    this.vectorDB = vectorDB;
    this.threshold = similarityThreshold;
  }
  
  async getOrGenerate(prompt, generator) {
    // 获取 prompt 的 embedding
    const embedding = await getEmbedding(prompt);
    
    // 查找相似的缓存
    const similar = await this.vectorDB.querySimilar(embedding, 1);
    
    if (similar.length > 0 && similar[0].score > this.threshold) {
      console.log('Cache hit!');
      return similar[0].metadata.response;
    }
    
    console.log('Cache miss, generating...');
    // 生成新回答
    const response = await generator(prompt);
    
    // 存入缓存
    await this.vectorDB.upsertVectors([{
      id: `cache_${Date.now()}`,
      values: embedding,
      metadata: { 
        prompt, 
        response,
        createdAt: Date.now()
      }
    }]);
    
    return response;
  }
}

// 使用示例
const cache = new SemanticCache(pineconeMemory);

const answer1 = await cache.getOrGenerate(
  '什么是机器学习？',
  async (q) => await llm.generate(q)
);

const answer2 = await cache.getOrGenerate(
  '机器学习的定义',  // 语义相似，直接返回缓存
  async (q) => await llm.generate(q)
);
// answer2 === answer1 （命中缓存）
```

### 4.2 智能问答检索

```javascript
class QARetrievalSystem {
  constructor(vectorDB) {
    this.vectorDB = vectorDB;
    this.knowledgeBase = [];
  }
  
  async loadKnowledgeBase(documents) {
    // documents: [{id, question, answer}]
    
    for (const doc of documents) {
      const embedding = await getEmbedding(doc.question);
      
      await this.vectorDB.upsertVectors([{
        id: doc.id,
        values: embedding,
        metadata: {
          question: doc.question,
          answer: doc.answer,
          tags: doc.tags || []
        }
      }]);
    }
  }
  
  async answerQuestion(question) {
    const queryEmbedding = await getEmbedding(question);
    
    // 检索最相似的问题
    const matches = await this.vectorDB.querySimilar(queryEmbedding, 3);
    
    if (matches.length === 0) {
      return {
        answer: '抱歉，我没有找到相关信息。',
        sources: []
      };
    }
    
    // 构建上下文
    const context = matches.map(m => 
      `Q: ${m.metadata.question}\nA: ${m.metadata.answer}`
    ).join('\n\n');
    
    // 让 LLM 综合回答
    const prompt = `
    基于以下信息回答问题：
    
    ${context}
    
    问题：${question}
    
    回答：
    `;
    
    const answer = await llm.generate(prompt);
    
    return {
      answer,
      sources: matches.map(m => m.metadata.question)
    };
  }
}

// 实战应用
const qaSystem = new QARetrievalSystem(weaviateMemory);

await qaSystem.loadKnowledgeBase([
  { 
    id: 'faq1', 
    question: '如何重置密码？', 
    answer: '点击设置→安全→重置密码...' 
  },
  { 
    id: 'faq2', 
    question: '支持哪些支付方式？', 
    answer: '我们支持支付宝、微信、银联...' 
  }
]);

const result = await qaSystem.answerQuestion('密码忘了怎么办？');
console.log(result.answer);
// 自动匹配到"如何重置密码"并给出答案
```

---

**下一节：** [c3-4 混合检索](./c3-4-hybrid-retrieval.md)  
**上一节：** [c3-2 长期记忆：SQLite/Markdown 存储](./c3-2-long-term-memory.md)
