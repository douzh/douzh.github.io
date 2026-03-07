# c3-5 RAG 实现：检索增强生成

## 1. 概述

RAG（Retrieval-Augmented Generation）通过先检索相关知识，再让 LLM 基于知识生成答案，有效减少幻觉、提高准确性。

## 2. 核心架构

```javascript
class RAGSystem {
  constructor(retriever, llm) {
    this.retriever = retriever;  // 检索模块
    this.llm = llm;              // 生成模块
  }
  
  async answer(question, options = {}) {
    // Step 1: 检索相关知识
    const relevantDocs = await this.retrieve(question, options);
    
    // Step 2: 构建增强的 prompt
    const prompt = this.buildPrompt(question, relevantDocs, options);
    
    // Step 3: 生成答案
    const answer = await this.llm.generate(prompt);
    
    // Step 4: 引用来源
    return {
      answer,
      sources: relevantDocs.map(d => d.metadata.source),
      confidence: this.calculateConfidence(relevantDocs)
    };
  }
}
```

## 3. 完整实现

### 3.1 文档加载器

```javascript
class DocumentLoader {
  async loadFromDirectory(dirPath) {
    const files = await fs.readdir(dirPath);
    const documents = [];
    
    for (const file of files) {
      if (file.endsWith('.md') || file.endsWith('.txt')) {
        const content = await fs.readFile(path.join(dirPath, file), 'utf-8');
        
        documents.push({
          id: file,
          content,
          metadata: {
            source: file,
            type: file.endsWith('.md') ? 'markdown' : 'text'
          }
        });
      }
    }
    
    return documents;
  }
  
  chunkDocument(document, chunkSize = 500, overlap = 50) {
    const chunks = [];
    const text = document.content;
    
    for (let i = 0; i < text.length; i += chunkSize - overlap) {
      chunks.push({
        id: `${document.id}_chunk_${chunks.length}`,
        content: text.slice(i, i + chunkSize),
        metadata: {
          ...document.metadata,
          chunkIndex: chunks.length,
          startOffset: i
        }
      });
      
      if (i + chunkSize >= text.length) break;
    }
    
    return chunks;
  }
}
```

### 3.2 向量索引构建

```javascript
class VectorIndexBuilder {
  constructor(vectorDB, embeddingModel) {
    this.vectorDB = vectorDB;
    this.embeddingModel = embeddingModel;
  }
  
  async buildIndex(documents, indexName) {
    console.log(`Building index with ${documents.length} documents...`);
    
    // 批量处理（每批 100 个）
    const batchSize = 100;
    for (let i = 0; i < documents.length; i += batchSize) {
      const batch = documents.slice(i, i + batchSize);
      
      // 并行计算 embedding
      const embeddings = await Promise.all(
        batch.map(doc => this.embeddingModel.embed(doc.content))
      );
      
      // 存入向量数据库
      await this.vectorDB.upsertVectors(
        batch.map((doc, j) => ({
          id: doc.id,
          values: embeddings[j],
          metadata: doc.metadata
        }))
      );
      
      console.log(`Indexed ${Math.min(i + batchSize, documents.length)}/${documents.length}`);
    }
    
    console.log('Index building complete!');
  }
}
```

### 3.3 混合检索器

```javascript
class HybridRetriever {
  constructor(vectorDB, bm25Index) {
    this.vectorDB = vectorDB;
    this.bm25Index = bm25Index;
  }
  
  async retrieve(query, topK = 5) {
    const [vectorResults, bm25Results] = await Promise.all([
      this.vectorSearch(query, topK * 2),
      this.bm25Search(query, topK * 2)
    ]);
    
    // 融合结果
    const fusedResults = this.fuseResults(vectorResults, bm25Results);
    
    return fusedResults.slice(0, topK);
  }
  
  async vectorSearch(query, topK) {
    const queryEmbedding = await getEmbedding(query);
    const matches = await this.vectorDB.querySimilar(queryEmbedding, topK);
    
    return matches.map(m => ({
      id: m.id,
      score: 1 - m.score, // 距离转相似度
      content: m.metadata.content,
      metadata: m.metadata,
      source: 'vector'
    }));
  }
  
  bm25Search(query, topK) {
    const results = this.bm25Index.search(query);
    
    return results.slice(0, topK).map(r => ({
      id: r.doc.id,
      score: r.score,
      content: r.doc.content,
      metadata: r.doc.metadata,
      source: 'bm25'
    }));
  }
  
  fuseResults(vectorResults, bm25Results, k = 60) {
    const scores = new Map();
    
    // 向量检索分数
    vectorResults.forEach((r, i) => {
      scores.set(r.id, {
        ...r,
        vectorScore: 1 / (i + k),
        combinedScore: 1 / (i + k)
      });
    });
    
    // BM25 分数
    bm25Results.forEach((r, i) => {
      const existing = scores.get(r.id);
      if (existing) {
        existing.bm25Score = 1 / (i + k);
        existing.combinedScore += 1 / (i + k);
      } else {
        scores.set(r.id, {
          ...r,
          bm25Score: 1 / (i + k),
          combinedScore: 1 / (i + k)
        });
      }
    });
    
    return Array.from(scores.values())
      .sort((a, b) => b.combinedScore - a.combinedScore);
  }
}
```

## 4. 实战案例

### 4.1 企业知识库问答

```javascript
class EnterpriseQA {
  constructor() {
    this.loader = new DocumentLoader();
    this.indexBuilder = new VectorIndexBuilder(pineconeDB, embeddingModel);
    this.retriever = new HybridRetriever(pineconeDB, bm25Index);
    this.rag = new RAGSystem(this.retriever, llm);
  }
  
  async initialize(knowledgeDir) {
    // 加载文档
    const documents = await this.loader.loadFromDirectory(knowledgeDir);
    
    // 分块
    const chunks = [];
    for (const doc of documents) {
      chunks.push(...this.loader.chunkDocument(doc));
    }
    
    // 构建索引
    await this.indexBuilder.buildIndex(chunks, 'enterprise_knowledge');
  }
  
  async answerQuestion(employeeId, question) {
    // 记录查询日志
    await this.logQuery(employeeId, question);
    
    // RAG 回答
    const result = await this.rag.answer(question, {
      filters: { department: 'engineering' }
    });
    
    return result;
  }
}

// 使用示例
const qa = new EnterpriseQA();
await qa.initialize('./knowledge-base');

const answer = await qa.answerQuestion(
  'emp_123',
  '公司的年假政策是什么？'
);

console.log(answer.answer);
console.log('来源:', answer.sources);
```

### 4.2 智能客服系统

```javascript
class CustomerServiceBot {
  constructor() {
    this.rag = new RAGSystem(retriever, llm);
    this.conversationHistory = new Map();
  }
  
  async handleQuery(sessionId, userQuery) {
    // 获取对话历史
    const history = this.conversationHistory.get(sessionId) || [];
    
    // 构建带上下文的查询
    const contextQuery = this.buildContextualQuery(userQuery, history);
    
    // RAG 回答
    const result = await this.rag.answer(contextQuery, {
      topK: 3,
      includeConversationHistory: true
    });
    
    // 更新历史
    this.updateHistory(sessionId, userQuery, result.answer);
    
    // 检查是否需要人工介入
    if (this.needsHumanIntervention(result)) {
      return {
        type: 'escalate',
        message: '我正在为您转接人工客服...',
        ragResult: result
      };
    }
    
    return {
      type: 'answer',
      message: result.answer,
      sources: result.sources
    };
  }
  
  buildContextualQuery(query, history) {
    if (history.length === 0) return query;
    
    const lastExchange = history.slice(-2);
    return `
    对话历史：
    ${lastExchange.map(e => `用户：${e.user}\n助手：${e.assistant}`).join('\n')}
    
    当前问题：${query}
    `;
  }
  
  needsHumanIntervention(result) {
    // 置信度太低或用户明确要求人工
    return result.confidence < 0.6 || 
           result.answer.includes('抱歉') ||
           result.sources.length === 0;
  }
}
```

## 5. 优化技巧

### 5.1 延迟优化

```javascript
// 预取热门问题
class PrecomputedAnswers {
  constructor() {
    this.cache = new Map();
  }
  
  async precomputeCommonQuestions(faqList) {
    for (const faq of faqList) {
      const answer = await this.rag.answer(faq.question);
      this.cache.set(faq.question.toLowerCase(), answer);
    }
  }
  
  getCachedAnswer(question) {
    return this.cache.get(question.toLowerCase());
  }
}

// 流式输出
async function* streamRAGAnswer(question) {
  // 异步检索
  const retrievalPromise = retriever.retrieve(question);
  
  // 同时开始生成（部分文本）
  const initialText = '正在查找相关信息...';
  yield initialText;
  
  const docs = await retrievalPromise;
  const prompt = buildPrompt(question, docs);
  
  // 流式生成
  for await (const chunk of llm.generateStream(prompt)) {
    yield chunk;
  }
}
```

### 5.2 质量提升

```javascript
// 多阶段重排序
class MultiStageRanker {
  async rerank(query, candidates) {
    // Stage 1: 快速筛选（BM25）
    let filtered = this.bm25Filter(query, candidates, topN=50);
    
    // Stage 2: 语义相似度
    filtered = await this.semanticFilter(query, filtered, topN=20);
    
    // Stage 3: Cross-encoder 精排
    const finalRanking = await this.crossEncoderRerank(query, filtered);
    
    return finalRanking.slice(0, 5);
  }
}

// 答案验证
class AnswerValidator {
  validate(answer, sources) {
    const issues = [];
    
    // 检查是否基于提供的来源
    if (!this.isGrounded(answer, sources)) {
      issues.push('答案可能包含未授权的信息');
    }
    
    // 检查是否有矛盾
    if (this.hasContradictions(answer, sources)) {
      issues.push('答案与来源存在矛盾');
    }
    
    return {
      valid: issues.length === 0,
      issues,
      confidence: 1 - (issues.length * 0.2)
    };
  }
}
```

---

**上一节：** [c3-4 混合检索](./c3-4-hybrid-retrieval.md)  
**c3 模块完成！**
