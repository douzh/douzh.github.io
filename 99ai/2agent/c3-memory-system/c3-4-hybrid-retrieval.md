# c3-4 混合检索：关键词 + 语义

# c3-5 RAG 实现：检索增强生成

由于篇幅限制，这两节提供核心框架和代码示例概要。完整内容将在后续补充。

## c3-4 混合检索核心代码

```javascript
// BM25 + 向量相似度融合
class HybridRetriever {
  async search(query) {
    const [keywordResults, semanticResults] = await Promise.all([
      this.bm25Search(query),      // 关键词匹配
      this.vectorSearch(query)     // 语义匹配
    ]);
    
    // Reciprocal Rank Fusion (RRF)
    return this.reciprocalRankFusion(keywordResults, semanticResults);
  }
  
  reciprocalRankFusion(results1, results2, k = 60) {
    const scores = new Map();
    
    results1.forEach((r, i) => {
      scores.set(r.id, (scores.get(r.id) || 0) + 1 / (i + k));
    });
    
    results2.forEach((r, i) => {
      scores.set(r.id, (scores.get(r.id) || 0) + 1 / (i + k));
    });
    
    return Array.from(scores.entries())
      .sort((a, b) => b[1] - a[1])
      .map(([id, score]) => ({ id, score }));
  }
}
```

## c3-5 RAG 核心流程

```javascript
class RAGSystem {
  async answer(question, contextDocuments) {
    // Step 1: 检索相关文档
    const relevantDocs = await this.retrieve(question, contextDocuments);
    
    // Step 2: 构建 prompt
    const prompt = `
    请基于以下信息回答问题：
    
    ${relevantDocs.map(d => d.content).join('\n\n')}
    
    问题：${question}
    
    回答：
    `;
    
    // Step 3: 生成答案
    return await llm.generate(prompt);
  }
  
  async retrieve(query, documents, topK = 5) {
    // 使用混合检索
    const retriever = new HybridRetriever();
    const results = await retriever.search(query);
    
    return results.slice(0, topK);
  }
}
```

---

**c3 模块剩余文档规划中，敬请期待完整版！**
