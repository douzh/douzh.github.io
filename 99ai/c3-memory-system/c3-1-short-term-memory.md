# c3-1 短期记忆：内存缓存与会话管理

## 1. 概述

短期记忆是 AI智能体在运行过程中临时存储的信息，用于保持对话连贯性和上下文理解。本章详解基于内存的缓存策略、会话管理和 LRU 淘汰算法。

## 2. 核心概念

### 2.1 短期记忆的特点

| 特性 | 说明 | 技术实现 |
|------|------|---------|
| **快速访问** | 毫秒级响应 | 内存存储（RAM） |
| **容量有限** | 受物理内存限制 | LRU/LFU 淘汰策略 |
| **临时性** | 会话结束即消失 | TTL 过期机制 |
| **高频更新** | 随对话持续变化 | 写时复制/增量更新 |

### 2.2 记忆分层模型

```javascript
class MemoryHierarchy {
  constructor() {
    // L1: 工作记忆（当前对话）
    this.workingMemory = {
      capacity: '10-20 轮对话',
      accessTime: '< 1ms',
      content: '当前活跃的上下文'
    };
    
    // L2: 短期缓存（最近会话）
    this.shortTermCache = {
      capacity: '72 小时对话',
      accessTime: '< 10ms',
      content: '近期相关记忆'
    };
    
    // L3: 长期记忆（持久化）
    this.longTermMemory = {
      capacity: '永久',
      accessTime: '100ms-1s',
      content: '重要事实和经历'
    };
  }
}
```

## 3. LRU 缓存实现

### 3.1 基础 LRU 缓存

```javascript
class LRUCache {
  constructor(capacity) {
    this.capacity = capacity;
    this.cache = new Map();
  }
  
  get(key) {
    if (!this.cache.has(key)) {
      return undefined;
    }
    
    // 提升到最新位置
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);
    
    return value;
  }
  
  put(key, value) {
    // 如果已存在，先删除
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } 
    // 如果超出容量，删除最旧的
    else if (this.cache.size >= this.capacity) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
      console.log(`LRU Evict: ${firstKey}`);
    }
    
    // 插入新元素
    this.cache.set(key, value);
  }
  
  // 获取所有键（按访问时间排序）
  keys() {
    return Array.from(this.cache.keys());
  }
  
  // 清空缓存
  clear() {
    this.cache.clear();
  }
  
  // 缓存统计
  stats() {
    return {
      size: this.cache.size,
      capacity: this.capacity,
      utilization: (this.cache.size / this.capacity * 100).toFixed(1) + '%'
    };
  }
}

// 使用示例
const cache = new LRUCache(100); // 容量 100

cache.put('session_1', { messages: [...] });
cache.put('session_2', { messages: [...] });

const session1 = cache.get('session_1'); // 命中，提升优先级
```

### 3.2 带 TTL 的 LRU 缓存

```javascript
class LRUCacheWithTTL {
  constructor(capacity, defaultTTL = 3600000) { // 默认 1 小时
    this.capacity = capacity;
    this.defaultTTL = defaultTTL;
    this.cache = new Map();
    this.timestamps = new Map();
  }
  
  get(key) {
    if (!this.cache.has(key)) {
      return undefined;
    }
    
    // 检查是否过期
    const timestamp = this.timestamps.get(key);
    if (Date.now() - timestamp > this.defaultTTL) {
      this.cache.delete(key);
      this.timestamps.delete(key);
      console.log(`TTL expired: ${key}`);
      return undefined;
    }
    
    // 更新访问时间
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);
    this.timestamps.set(key, Date.now());
    
    return value;
  }
  
  put(key, value, ttl = null) {
    if (this.cache.has(key)) {
      this.cache.delete(key);
    } else if (this.cache.size >= this.capacity) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
      this.timestamps.delete(firstKey);
    }
    
    this.cache.set(key, value);
    this.timestamps.set(key, Date.now());
  }
  
  // 定期清理过期项
  cleanup() {
    const now = Date.now();
    for (const key of this.cache.keys()) {
      if (now - this.timestamps.get(key) > this.defaultTTL) {
        this.cache.delete(key);
        this.timestamps.delete(key);
      }
    }
  }
}

// 启动定期清理
const cache = new LRUCacheWithTTL(100, 72 * 60 * 60 * 1000); // 72 小时 TTL
setInterval(() => cache.cleanup(), 5 * 60 * 1000); // 每 5 分钟清理一次
```

### 3.3 支持权重的高级 LRU

```javascript
class WeightedLRUCache {
  constructor(maxWeight = 1000) {
    this.maxWeight = maxWeight;
    this.currentWeight = 0;
    this.cache = new Map();
    this.weights = new Map();
  }
  
  get(key) {
    if (!this.cache.has(key)) return undefined;
    
    const value = this.cache.get(key);
    this.cache.delete(key);
    this.cache.set(key, value);
    
    return value;
  }
  
  put(key, value, weight = 1) {
    if (this.cache.has(key)) {
      this.currentWeight -= this.weights.get(key);
      this.cache.delete(key);
    }
    
    // 如果超出重量限制，淘汰直到有空间
    while (this.currentWeight + weight > this.maxWeight && this.cache.size > 0) {
      const firstKey = this.cache.keys().next().value;
      const evictWeight = this.weights.get(firstKey);
      
      this.cache.delete(firstKey);
      this.weights.delete(firstKey);
      this.currentWeight -= evictWeight;
      
      console.log(`Weighted LRU Evict: ${firstKey} (-${evictWeight})`);
    }
    
    this.cache.set(key, value);
    this.weights.set(key, weight);
    this.currentWeight += weight;
  }
  
  stats() {
    return {
      items: this.cache.size,
      currentWeight: this.currentWeight,
      maxWeight: this.maxWeight,
      utilization: (this.currentWeight / this.maxWeight * 100).toFixed(1) + '%'
    };
  }
}

// 使用场景：不同大小的对象分配不同权重
const cache = new WeightedLRUCache(1000);

cache.put('small_config', config, 10);      // 小对象
cache.put('large_context', bigData, 100);   // 大对象
cache.put('huge_dataset', hugeData, 500);   // 超大对象

console.log(cache.stats());
// 输出：{ items: 3, currentWeight: 610, maxWeight: 1000, utilization: '61.0%' }
```

## 4. 会话上下文管理

### 4.1 会话管理器

```javascript
class SessionManager {
  constructor(options = {}) {
    this.sessions = new Map();
    this.maxSessions = options.maxSessions || 1000;
    this.sessionTTL = options.sessionTTL || 72 * 60 * 60 * 1000; // 72 小时
    this.maxMessagesPerSession = options.maxMessages || 100;
    
    // 启动定期清理
    setInterval(() => this.cleanup(), 5 * 60 * 1000);
  }
  
  // 创建或获取会话
  getSession(sessionId) {
    if (!this.sessions.has(sessionId)) {
      this.sessions.set(sessionId, {
        id: sessionId,
        createdAt: Date.now(),
        lastAccessedAt: Date.now(),
        messages: [],
        metadata: {}
      });
    }
    
    const session = this.sessions.get(sessionId);
    session.lastAccessedAt = Date.now();
    
    return session;
  }
  
  // 添加消息到会话
  addMessage(sessionId, role, content) {
    const session = this.getSession(sessionId);
    
    session.messages.push({
      role,
      content,
      timestamp: Date.now()
    });
    
    // 保持消息数量在限制内
    if (session.messages.length > this.maxMessagesPerSession) {
      session.messages = session.messages.slice(-this.maxMessagesPerSession);
    }
    
    return session;
  }
  
  // 获取会话上下文（用于 LLM 请求）
  getContext(sessionId, maxRounds = null) {
    const session = this.sessions.get(sessionId);
    if (!session) return [];
    
    const messages = maxRounds 
      ? session.messages.slice(-maxRounds * 2) // 保留最后 N 轮
      : session.messages;
    
    return messages.map(m => ({
      role: m.role,
      content: m.content
    }));
  }
  
  // 清理过期会话
  cleanup() {
    const now = Date.now();
    for (const [id, session] of this.sessions.entries()) {
      if (now - session.lastAccessedAt > this.sessionTTL) {
        this.sessions.delete(id);
        console.log(`Session expired: ${id}`);
      }
    }
    
    // 如果仍然超出容量，淘汰最久未访问的
    while (this.sessions.size > this.maxSessions) {
      let oldestId = null;
      let oldestTime = Infinity;
      
      for (const [id, session] of this.sessions.entries()) {
        if (session.lastAccessedAt < oldestTime) {
          oldestTime = session.lastAccessedAt;
          oldestId = id;
        }
      }
      
      if (oldestId) {
        this.sessions.delete(oldestId);
        console.log(`Session evicted (capacity): ${oldestId}`);
      }
    }
  }
  
  // 会话统计
  stats() {
    return {
      activeSessions: this.sessions.size,
      maxSessions: this.maxSessions,
      totalMessages: Array.from(this.sessions.values())
        .reduce((sum, s) => sum + s.messages.length, 0)
    };
  }
}

// 使用示例
const sessionManager = new SessionManager({
  maxSessions: 1000,
  sessionTTL: 72 * 60 * 60 * 1000,
  maxMessages: 50
});

// 添加对话
sessionManager.addMessage('user_123', 'user', '你好');
sessionManager.addMessage('user_123', 'assistant', '有什么可以帮您？');

// 获取上下文
const context = sessionManager.getContext('user_123');
```

### 4.2 滑动窗口上下文

```javascript
class SlidingWindowContext {
  constructor(windowSize = 10) {
    this.windowSize = windowSize;
    this.messages = [];
    this.summary = ''; // 早期对话的摘要
  }
  
  addMessage(role, content) {
    this.messages.push({ role, content });
    
    // 保持窗口大小
    while (this.messages.length > this.windowSize * 2) {
      // 移除最早的一轮对话（user + assistant）
      const removed = this.messages.splice(0, 2);
      
      // 可选：将移除的对话添加到摘要中
      this.updateSummary(removed);
    }
  }
  
  async updateSummary(removedMessages) {
    const conversationText = removedMessages
      .map(m => `${m.role}: ${m.content}`)
      .join('\n');
    
    const summaryPrompt = `
    总结以下对话的关键信息（50 字以内）：
    
    ${conversationText}
    
    总结：
    `;
    
    const newSummary = await llm.generate(summaryPrompt);
    this.summary += '\n' + newSummary;
    
    // 保持摘要不太长
    if (this.summary.length > 1000) {
      this.summary = this.summary.slice(-1000);
    }
  }
  
  getFullContext() {
    const summaryPart = this.summary ? `[对话摘要]\n${this.summary}\n\n` : '';
    const recentPart = this.messages
      .map(m => `${m.role}: ${m.content}`)
      .join('\n');
    
    return summaryPart + recentPart;
  }
  
  clear() {
    this.messages = [];
    this.summary = '';
  }
}

// 实战应用
const context = new SlidingWindowContext(5); // 保留最近 5 轮

context.addMessage('user', '我想了解机器学习');
context.addMessage('assistant', '机器学习是...');
// ... 继续对话

// 获取完整上下文（包含摘要和最近对话）
const fullContext = context.getFullContext();
```

## 5. 工作记忆优化

### 5.1 注意力机制

```javascript
class AttentionMemory {
  constructor() {
    this.items = [];
    this.attentionScores = new Map();
  }
  
  add(item, priority = 0.5) {
    this.items.push({
      content: item,
      timestamp: Date.now(),
      basePriority: priority,
      accessCount: 0
    });
  }
  
  access(index) {
    const item = this.items[index];
    if (item) {
      item.accessCount++;
      // 提升注意力分数
      const currentScore = this.attentionScores.get(index) || 0;
      this.attentionScores.set(index, currentScore + 0.1);
    }
  }
  
  // 计算综合注意力分数
  calculateScores() {
    const now = Date.now();
    const scores = [];
    
    for (let i = 0; i < this.items.length; i++) {
      const item = this.items[i];
      
      // 时间衰减（越近越高）
      const timeDecay = Math.exp(-(now - item.timestamp) / (60 * 60 * 1000));
      
      // 访问频率（越多越高）
      const frequencyBonus = Math.log(item.accessCount + 1);
      
      // 基础优先级
      const basePriority = item.basePriority;
      
      // 综合分数
      const score = timeDecay * 0.5 + frequencyBonus * 0.3 + basePriority * 0.2;
      scores.push({ index: i, score });
    }
    
    return scores.sort((a, b) => b.score - a.score);
  }
  
  // 获取最重要的 N 项
  getTopN(n = 5) {
    const scores = this.calculateScores();
    return scores.slice(0, n).map(s => this.items[s.index].content);
  }
  
  // 淘汰低优先级项
  prune(minItems = 10) {
    if (this.items.length <= minItems) return;
    
    const scores = this.calculateScores();
    const toRemove = this.items.length - minItems;
    
    // 移除分数最低的
    const removeIndices = scores.slice(-toRemove).map(s => s.index);
    
    for (const idx of removeIndices.sort((a, b) => b - a)) {
      this.items.splice(idx, 1);
    }
  }
}
```

### 5.2 相关性过滤

```javascript
class RelevanceFilter {
  constructor(threshold = 0.6) {
    this.threshold = threshold;
    this.memory = [];
  }
  
  async addMessage(query, response) {
    // 计算与之前记忆的相关性
    const relevanceScores = await Promise.all(
      this.memory.map(async (item, index) => ({
        index,
        score: await this.calculateRelevance(query, item.content)
      }))
    );
    
    // 只保留高相关性的记忆
    const relevantItems = relevanceScores
      .filter(r => r.score >= this.threshold)
      .sort((a, b) => b.score - a.score);
    
    // 添加到记忆
    this.memory.push({
      query,
      response,
      timestamp: Date.now(),
      relevanceScores: Object.fromEntries(
        relevanceScores.map(r => [r.index, r.score])
      )
    });
    
    return relevantItems;
  }
  
  async calculateRelevance(text1, text2) {
    // 简化的相关性计算（可以用向量相似度替代）
    const words1 = new Set(text1.toLowerCase().split(/\s+/));
    const words2 = new Set(text2.toLowerCase().split(/\s+/));
    
    const intersection = [...words1].filter(w => words2.has(w));
    const union = new Set([...words1, ...words2]);
    
    return intersection.length / union.size;
  }
  
  // 获取与当前查询相关的历史
  getRelevantHistory(currentQuery, limit = 5) {
    return this.memory
      .filter(item => {
        const avgRelevance = Object.values(item.relevanceScores)
          .reduce((a, b) => a + b, 0) / Object.keys(item.relevanceScores).length;
        return avgRelevance >= this.threshold;
      })
      .slice(-limit)
      .map(item => ({
        query: item.query,
        response: item.response
      }));
  }
}
```

## 6. 性能优化

### 6.1 内存池化

```javascript
class MemoryPool {
  constructor(blockSize = 1024, poolSize = 100) {
    this.blockSize = blockSize;
    this.freeList = [];
    
    // 预分配内存块
    for (let i = 0; i < poolSize; i++) {
      this.freeList.push(this.allocateBlock());
    }
  }
  
  allocateBlock() {
    return new ArrayBuffer(this.blockSize);
  }
  
  acquire() {
    if (this.freeList.length === 0) {
      // 动态扩展
      this.freeList.push(this.allocateBlock());
    }
    return this.freeList.pop();
  }
  
  release(block) {
    // 清零后返回池
    const view = new Uint8Array(block);
    view.fill(0);
    this.freeList.push(block);
  }
  
  stats() {
    return {
      freeBlocks: this.freeList.length,
      totalBlocks: this.poolSize,
      utilization: ((this.poolSize - this.freeList.length) / this.poolSize * 100).toFixed(1) + '%'
    };
  }
}
```

### 6.2 增量更新

```javascript
class IncrementalMemory {
  constructor() {
    this.baseState = null;
    this.deltas = [];
  }
  
  update(delta) {
    this.deltas.push({
      delta,
      timestamp: Date.now()
    });
    
    // 定期合并 delta 到 base state
    if (this.deltas.length > 100) {
      this.mergeDeltas();
    }
  }
  
  getState() {
    // 实时合并所有 delta
    let state = { ...this.baseState };
    
    for (const { delta } of this.deltas) {
      state = this.applyDelta(state, delta);
    }
    
    return state;
  }
  
  mergeDeltas() {
    // 将累积的 delta 合并到 base state
    let state = { ...this.baseState };
    
    for (const { delta } of this.deltas) {
      state = this.applyDelta(state, delta);
    }
    
    this.baseState = state;
    this.deltas = [];
  }
  
  applyDelta(state, delta) {
    return { ...state, ...delta };
  }
}
```

## 7. 监控与调试

### 7.1 命中率监控

```javascript
class CacheMonitor {
  constructor() {
    this.hits = 0;
    this.misses = 0;
    this.evictions = 0;
    this.accessLog = [];
  }
  
  recordHit(key, accessTime) {
    this.hits++;
    this.logAccess('hit', key, accessTime);
  }
  
  recordMiss(key, accessTime) {
    this.misses++;
    this.logAccess('miss', key, accessTime);
  }
  
  recordEviction(key) {
    this.evictions++;
    console.log(`Cache eviction: ${key}`);
  }
  
  logAccess(type, key, time) {
    this.accessLog.push({
      timestamp: Date.now(),
      type,
      key,
      accessTime: time
    });
    
    // 保持日志大小合理
    if (this.accessLog.length > 1000) {
      this.accessLog = this.accessLog.slice(-500);
    }
  }
  
  // 获取命中率
  hitRate() {
    const total = this.hits + this.misses;
    if (total === 0) return 0;
    return (this.hits / total * 100).toFixed(2) + '%';
  }
  
  // 平均访问时间
  averageAccessTime() {
    if (this.accessLog.length === 0) return 0;
    
    const totalTime = this.accessLog.reduce((sum, log) => sum + log.accessTime, 0);
    return (totalTime / this.accessLog.length).toFixed(2) + 'ms';
  }
  
  // 生成报告
  generateReport() {
    return {
      hits: this.hits,
      misses: this.misses,
      hitRate: this.hitRate(),
      evictions: this.evictions,
      averageAccessTime: this.averageAccessTime(),
      totalAccesses: this.hits + this.misses
    };
  }
}

// 集成到缓存中
class MonitoredLRUCache extends LRUCache {
  constructor(capacity) {
    super(capacity);
    this.monitor = new CacheMonitor();
  }
  
  get(key) {
    const start = performance.now();
    const value = super.get(key);
    const accessTime = performance.now() - start;
    
    if (value !== undefined) {
      this.monitor.recordHit(key, accessTime);
    } else {
      this.monitor.recordMiss(key, accessTime);
    }
    
    return value;
  }
  
  put(key, value) {
    const existed = this.cache.has(key);
    super.put(key, value);
    
    if (!existed && this.cache.size === this.capacity) {
      this.monitor.recordEviction(key);
    }
  }
  
  getStats() {
    return {
      ...super.stats(),
      ...this.monitor.generateReport()
    };
  }
}
```

---

**下一节：** [c3-2 长期记忆：SQLite/Markdown 存储](./c3-2-long-term-memory.md)  
**上一节：** [c3 记忆系统模块](./README.md)
