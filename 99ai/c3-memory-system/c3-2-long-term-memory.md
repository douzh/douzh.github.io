# c3-2 长期记忆：SQLite/Markdown 存储

## 1. 概述

长期记忆是 AI智能体永久保存的重要信息，包括用户偏好、历史对话、知识库等。本章详解基于 SQLite 和 Markdown 文件的持久化存储方案。

## 2. 存储方案对比

### 2.1 SQLite vs Markdown

| 特性 | SQLite | Markdown 文件 |
|------|--------|--------------|
| **查询能力** | SQL 完整支持 | 关键词搜索 |
| **结构化程度** | 高度结构化 | 半结构化 |
| **读写性能** | 高（索引优化） | 中（顺序读取） |
| **人类可读性** | 需工具查看 | 直接阅读 |
| **版本控制** | 二进制 diff | Git友好 |
| **适用场景** | 频繁查询、复杂关系 | 文档存储、知识沉淀 |

### 2.2 混合存储架构

```javascript
class HybridMemoryStorage {
  constructor(dbPath, markdownDir) {
    // SQLite 用于结构化数据
    this.sqlite = new SQLiteDB(dbPath);
    
    // Markdown 用于非结构化文档
    this.markdownStore = new MarkdownStore(markdownDir);
  }
  
  async store(type, data) {
    if (type === 'structured') {
      return await this.sqlite.insert(data);
    } else {
      return await this.markdownStore.save(data);
    }
  }
  
  async retrieve(query) {
    // 同时查询两个存储
    const [sqliteResults, mdResults] = await Promise.all([
      this.sqlite.query(query),
      this.markdownStore.search(query)
    ]);
    
    return this.mergeResults(sqliteResults, mdResults);
  }
}
```

## 3. SQLite 实现

### 3.1 数据库初始化

```javascript
const sqlite3 = require('sqlite3').verbose();
const path = require('path');

class MemoryDatabase {
  constructor(dbPath = './memory.db') {
    this.dbPath = dbPath;
    this.db = null;
  }
  
  async init() {
    return new Promise((resolve, reject) => {
      this.db = new sqlite3.Database(this.dbPath, (err) => {
        if (err) {
          reject(err);
          return;
        }
        
        console.log(`Connected to SQLite: ${this.dbPath}`);
        this.createTables().then(resolve).catch(reject);
      });
    });
  }
  
  async createTables() {
    const queries = [
      // 会话表
      `CREATE TABLE IF NOT EXISTS sessions (
        id TEXT PRIMARY KEY,
        user_id TEXT,
        created_at INTEGER,
        last_accessed INTEGER,
        metadata TEXT
      )`,
      
      // 消息表
      `CREATE TABLE IF NOT EXISTS messages (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        session_id TEXT,
        role TEXT,
        content TEXT,
        timestamp INTEGER,
        FOREIGN KEY(session_id) REFERENCES sessions(id)
      )`,
      
      // 用户记忆表
      `CREATE TABLE IF NOT EXISTS user_memories (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id TEXT,
        category TEXT,
        key TEXT,
        value TEXT,
        importance INTEGER DEFAULT 1,
        created_at INTEGER,
        updated_at INTEGER
      )`,
      
      // 知识库表
      `CREATE TABLE IF NOT EXISTS knowledge_base (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        title TEXT,
        content TEXT,
        tags TEXT,
        source TEXT,
        created_at INTEGER
      )`,
      
      // 创建索引
      `CREATE INDEX IF NOT EXISTS idx_messages_session ON messages(session_id)`,
      `CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp)`,
      `CREATE INDEX IF NOT EXISTS idx_user_memories_user ON user_memories(user_id)`,
      `CREATE INDEX IF NOT EXISTS idx_knowledge_tags ON knowledge_base(tags)`
    ];
    
    for (const query of queries) {
      await this.run(query);
    }
    
    console.log('Database tables created');
  }
  
  run(query, params = []) {
    return new Promise((resolve, reject) => {
      this.db.run(query, params, function(err) {
        if (err) reject(err);
        else resolve({ lastID: this.lastID, changes: this.changes });
      });
    });
  }
  
  all(query, params = []) {
    return new Promise((resolve, reject) => {
      this.db.all(query, params, (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }
  
  get(query, params = []) {
    return new Promise((resolve, reject) => {
      this.db.get(query, params, (err, row) => {
        if (err) reject(err);
        else resolve(row);
      });
    });
  }
  
  close() {
    return new Promise((resolve, reject) => {
      this.db.close((err) => {
        if (err) reject(err);
        else {
          console.log('Database connection closed');
          resolve();
        }
      });
    });
  }
}

// 使用示例
const db = new MemoryDatabase('./data/memory.db');
await db.init();
```

### 3.2 会话管理器

```javascript
class SessionRepository {
  constructor(database) {
    this.db = database;
  }
  
  async createSession(userId, metadata = {}) {
    const sessionId = this.generateId();
    const now = Date.now();
    
    await this.db.run(
      `INSERT INTO sessions (id, user_id, created_at, last_accessed, metadata)
       VALUES (?, ?, ?, ?, ?)`,
      [sessionId, userId, now, now, JSON.stringify(metadata)]
    );
    
    return sessionId;
  }
  
  async addMessage(sessionId, role, content) {
    await this.db.run(
      `INSERT INTO messages (session_id, role, content, timestamp)
       VALUES (?, ?, ?, ?)`,
      [sessionId, role, content, Date.now()]
    );
    
    // 更新会话最后访问时间
    await this.db.run(
      `UPDATE sessions SET last_accessed = ? WHERE id = ?`,
      [Date.now(), sessionId]
    );
  }
  
  async getSessionMessages(sessionId, limit = 100) {
    return await this.db.all(
      `SELECT role, content, timestamp 
       FROM messages 
       WHERE session_id = ? 
       ORDER BY timestamp ASC 
       LIMIT ?`,
      [sessionId, limit]
    );
  }
  
  async getUserSessions(userId, days = 7) {
    const cutoffTime = Date.now() - (days * 24 * 60 * 60 * 1000);
    
    return await this.db.all(
      `SELECT s.*, COUNT(m.id) as message_count
       FROM sessions s
       LEFT JOIN messages m ON s.id = m.session_id
       WHERE s.user_id = ? AND s.last_accessed > ?
       GROUP BY s.id
       ORDER BY s.last_accessed DESC`,
      [userId, cutoffTime]
    );
  }
  
  async deleteOldSessions(days = 30) {
    const cutoffTime = Date.now() - (days * 24 * 60 * 60 * 1000);
    
    const result = await this.db.run(
      `DELETE FROM sessions WHERE last_accessed < ?`,
      [cutoffTime]
    );
    
    console.log(`Deleted ${result.changes} old sessions`);
    return result.changes;
  }
  
  generateId() {
    return `sess_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }
}

// 实战应用
const sessionRepo = new SessionRepository(db);

// 创建会话
const sessionId = await sessionRepo.createSession('user_123', {
  platform: 'wechat',
  language: 'zh-CN'
});

// 添加对话
await sessionRepo.addMessage(sessionId, 'user', '你好');
await sessionRepo.addMessage(sessionId, 'assistant', '有什么可以帮您？');

// 获取历史消息
const messages = await sessionRepo.getSessionMessages(sessionId);
```

### 3.3 用户画像存储

```javascript
class UserProfileRepository {
  constructor(database) {
    this.db = database;
  }
  
  async updatePreference(userId, category, key, value, importance = 1) {
    const now = Date.now();
    
    await this.db.run(
      `INSERT OR REPLACE INTO user_memories 
       (user_id, category, key, value, importance, created_at, updated_at)
       VALUES (
         ?, ?, ?, ?, ?,
         COALESCE((SELECT created_at FROM user_memories WHERE user_id=? AND key=?), ?),
         ?
       )`,
      [userId, category, key, JSON.stringify(value), importance,
       userId, key, now, now]
    );
  }
  
  async getUserProfile(userId) {
    const memories = await this.db.all(
      `SELECT category, key, value, importance
       FROM user_memories
       WHERE user_id = ?
       ORDER BY importance DESC, updated_at DESC`,
      [userId]
    );
    
    // 按类别组织
    const profile = {};
    for (const memory of memories) {
      if (!profile[memory.category]) {
        profile[memory.category] = {};
      }
      profile[memory.category][memory.key] = {
        value: JSON.parse(memory.value),
        importance: memory.importance
      };
    }
    
    return profile;
  }
  
  async getImportantMemories(userId, minImportance = 3) {
    return await this.db.all(
      `SELECT category, key, value
       FROM user_memories
       WHERE user_id = ? AND importance >= ?`,
      [userId, minImportance]
    );
  }
  
  // 自动学习用户偏好
  async learnFromInteraction(userId, interaction) {
    // 从对话中提取偏好
    const preferences = await this.extractPreferences(interaction);
    
    for (const [category, prefs] of Object.entries(preferences)) {
      for (const [key, value] of Object.entries(prefs)) {
        await this.updatePreference(userId, category, key, value, 2);
      }
    }
  }
  
  async extractPreferences(interaction) {
    // 使用 LLM 提取偏好
    const prompt = `
    从以下对话中提取用户偏好：
    
    ${JSON.stringify(interaction, null, 2)}
    
    输出格式（JSON）：
    {
      "preferences": {
        "communication_style": {"tone": "formal"},
        "topics": {"interested": ["technology", "science"]},
        ...
      }
    }
    `;
    
    const result = await llm.generate(prompt);
    return JSON.parse(result).preferences;
  }
}

// 使用示例
const userProfile = new UserProfileRepository(db);

// 记录用户偏好
await userProfile.updatePreference(
  'user_123',
  'communication_style',
  'response_length',
  'concise',
  3 // 高重要性
);

await userProfile.updatePreference(
  'user_123',
  'topics',
  'interested',
  ['AI', 'programming', 'music'],
  2
);

// 获取完整画像
const profile = await userProfile.getUserProfile('user_123');
console.log(profile);
/*
{
  communication_style: {
    response_length: { value: 'concise', importance: 3 }
  },
  topics: {
    interested: { value: ['AI', 'programming', 'music'], importance: 2 }
  }
}
*/
```

## 4. Markdown 文件存储

### 4.1 文件组织结构

```
memory/markdown/
├── users/
│   ├── user_123/
│   │   ├── profile.md           # 用户基本信息
│   │   ├── preferences.md       # 偏好设置
│   │   └── conversations/
│   │       ├── 2026-03.md       # 按月归档
│   │       └── 2026-02.md
│   │
│   └── user_456/
│       └── ...
│
├── knowledge/
│   ├── technology/
│   │   ├── ai-basics.md
│   │   └── programming-tips.md
│   ├── science/
│   └── daily-life/
│
└── shared/
    ├── common-knowledge.md
    └── faq.md
```

### 4.2 Markdown 存储器

```javascript
const fs = require('fs').promises;
const path = require('path');
const matter = require('gray-matter');

class MarkdownStore {
  constructor(baseDir) {
    this.baseDir = baseDir;
    this.index = new Map(); // 内存索引
  }
  
  async save(filePath, content, metadata = {}) {
    const fullPath = path.join(this.baseDir, filePath);
    await fs.mkdir(path.dirname(fullPath), { recursive: true });
    
    // 添加 Front Matter
    const fileContent = matter.stringify(content, {
      ...metadata,
      updatedAt: new Date().toISOString(),
      createdAt: metadata.createdAt || new Date().toISOString()
    });
    
    await fs.writeFile(fullPath, fileContent);
    
    // 更新索引
    await this.updateIndex(filePath, metadata);
    
    return fullPath;
  }
  
  async load(filePath) {
    const fullPath = path.join(this.baseDir, filePath);
    const fileContent = await fs.readFile(fullPath, 'utf-8');
    
    const { data, content } = matter(fileContent);
    
    return {
      metadata: data,
      content: content
    };
  }
  
  async search(query, options = {}) {
    // 构建索引（如果还没有）
    if (this.index.size === 0) {
      await this.buildIndex();
    }
    
    const results = [];
    const keywords = query.toLowerCase().split(/\s+/);
    
    for (const [filePath, meta] of this.index.entries()) {
      let score = 0;
      
      // 标题匹配
      if (meta.title) {
        const titleLower = meta.title.toLowerCase();
        for (const keyword of keywords) {
          if (titleLower.includes(keyword)) {
            score += 3;
          }
        }
      }
      
      // 标签匹配
      if (meta.tags) {
        const tags = Array.isArray(meta.tags) ? meta.tags : meta.tags.split(',');
        for (const keyword of keywords) {
          if (tags.some(tag => tag.toLowerCase().includes(keyword))) {
            score += 2;
          }
        }
      }
      
      // 内容匹配（可选，较慢）
      if (options.searchContent) {
        const content = await this.load(filePath);
        const contentLower = content.content.toLowerCase();
        for (const keyword of keywords) {
          if (contentLower.includes(keyword)) {
            score += 1;
          }
        }
      }
      
      if (score > 0) {
        results.push({
          filePath,
          metadata: meta,
          score,
          preview: await this.getPreview(filePath, query)
        });
      }
    }
    
    return results.sort((a, b) => b.score - a.score);
  }
  
  async buildIndex() {
    const files = await this.walkDirectory(this.baseDir);
    
    for (const file of files) {
      if (file.endsWith('.md')) {
        try {
          const { metadata } = await this.load(file);
          this.index.set(file, metadata);
        } catch (error) {
          console.error(`Failed to index ${file}:`, error);
        }
      }
    }
    
    console.log(`Indexed ${this.index.size} markdown files`);
  }
  
  async walkDirectory(dir) {
    const files = [];
    
    async function walk(currentDir) {
      const entries = await fs.readdir(currentDir, { withFileTypes: true });
      
      for (const entry of entries) {
        const fullPath = path.join(currentDir, entry.name);
        
        if (entry.isDirectory()) {
          await walk(fullPath);
        } else if (entry.isFile()) {
          files.push(fullPath);
        }
      }
    }
    
    await walk(dir);
    return files;
  }
  
  async getPreview(filePath, query, length = 200) {
    const { content } = await this.load(filePath);
    
    // 找到包含查询关键词的片段
    const index = content.toLowerCase().indexOf(query.toLowerCase());
    
    if (index === -1) {
      return content.substring(0, length) + '...';
    }
    
    const start = Math.max(0, index - 50);
    const end = Math.min(content.length, index + length);
    
    return (start > 0 ? '...' : '') + 
           content.substring(start, end) + 
           (end < content.length ? '...' : '');
  }
  
  async updateIndex(filePath, metadata) {
    this.index.set(filePath, metadata);
  }
}

// 使用示例
const mdStore = new MarkdownStore('./memory/markdown');

// 保存用户笔记
await mdStore.save(
  'users/user_123/notes/project-ideas.md',
  `
  # 项目创意
  
  ## AI助手增强版
  - 支持多模态输入
  - 集成 MCP 协议
  - 本地记忆管理
  `,
  {
    title: '项目创意',
    tags: ['ideas', 'AI', 'projects'],
    category: 'notes'
  }
);

// 搜索相关内容
const results = await mdStore.search('AI助手', {
  searchContent: true
});

console.log(results);
/*
[
  {
    filePath: 'users/user_123/notes/project-ideas.md',
    metadata: { title: '项目创意', tags: ['ideas', 'AI', 'projects'] },
    score: 5,
    preview: '...AI助手增强版 - 支持多模态输入...'
  }
]
*/
```

### 4.3 对话归档系统

```javascript
class ConversationArchiver {
  constructor(markdownStore) {
    this.mdStore = markdownStore;
  }
  
  async archiveSession(sessionId, messages) {
    if (messages.length === 0) return;
    
    // 生成归档文件名（按月份）
    const firstMsgTime = messages[0].timestamp;
    const date = new Date(firstMsgTime);
    const monthStr = `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}`;
    
    const filePath = `conversations/${monthStr}/${sessionId}.md`;
    
    // 格式化对话内容
    const content = this.formatConversation(messages);
    
    // 提取关键信息作为 metadata
    const metadata = await this.extractMetadata(messages);
    
    await this.mdStore.save(filePath, content, {
      title: `对话归档 - ${sessionId}`,
      date: monthStr,
      messageCount: messages.length,
      ...metadata
    });
    
    return filePath;
  }
  
  formatConversation(messages) {
    let content = '# 对话记录\n\n';
    
    for (const msg of messages) {
      const time = new Date(msg.timestamp).toLocaleString('zh-CN');
      const role = msg.role === 'user' ? '👤 用户' : '🤖 助手';
      
      content += `## ${role} - ${time}\n\n`;
      content += `${msg.content}\n\n`;
      
      // 分隔线
      content += '---\n\n';
    }
    
    return content;
  }
  
  async extractMetadata(messages) {
    // 使用 LLM 提取主题和关键词
    const conversationText = messages
      .slice(0, 10) // 只用前 10 条
      .map(m => `${m.role}: ${m.content}`)
      .join('\n');
    
    const prompt = `
    分析以下对话，提取：
    1. 主要话题（1-3 个）
    2. 关键词（3-5 个）
    3. 情感倾向（positive/neutral/negative）
    
    对话：
    ${conversationText}
    
    输出 JSON 格式。
    `;
    
    const result = await llm.generate(prompt);
    const analysis = JSON.parse(result);
    
    return {
      topics: analysis.topics || [],
      keywords: analysis.keywords || [],
      sentiment: analysis.sentiment || 'neutral'
    };
  }
  
  async searchInArchive(userId, query, month = null) {
    const searchPath = month 
      ? `users/${userId}/conversations/${month}`
      : `users/${userId}/conversations`;
    
    return await this.mdStore.search(query, {
      basePath: searchPath,
      searchContent: true
    });
  }
}

// 实战应用
const archiver = new ConversationArchiver(mdStore);

// 定期归档（如每天凌晨）
async function dailyArchiving() {
  const sessions = await getActiveSessions(); // 自定义函数
  
  for (const session of sessions) {
    const messages = await sessionRepo.getSessionMessages(session.id);
    await archiver.archiveSession(session.id, messages);
  }
}

// 搜索历史对话
const results = await archiver.searchInArchive(
  'user_123',
  'Python 编程',
  '2026-03'
);
```

## 5. 数据同步与备份

### 5.1 双写机制

```javascript
class DualWriteStorage {
  constructor(sqliteDB, markdownStore) {
    this.sqlite = sqliteDB;
    this.mdStore = markdownStore;
  }
  
  async storeMemory(memory) {
    const timestamp = Date.now();
    
    try {
      // 同时写入两个存储
      await Promise.all([
        // SQLite 存储
        this.sqlite.run(
          `INSERT INTO long_term_memories 
           (user_id, type, content, metadata, timestamp)
           VALUES (?, ?, ?, ?, ?)`,
          [memory.userId, memory.type, memory.content, 
           JSON.stringify(memory.metadata), timestamp]
        ),
        
        // Markdown 归档
        this.mdStore.save(
          `memories/${memory.userId}/${memory.id}.md`,
          memory.content,
          {
            type: memory.type,
            tags: memory.tags,
            createdAt: new Date(timestamp).toISOString()
          }
        )
      ]);
      
      console.log(`Memory stored: ${memory.id}`);
      
    } catch (error) {
      console.error('Dual write failed:', error);
      throw error;
    }
  }
}
```

### 5.2 定期备份

```javascript
class BackupManager {
  constructor(dbPath, backupDir) {
    this.dbPath = dbPath;
    this.backupDir = backupDir;
  }
  
  async createBackup() {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const backupFile = path.join(
      this.backupDir, 
      `backup_${timestamp}.db`
    );
    
    await fs.mkdir(this.backupDir, { recursive: true });
    
    // 复制数据库文件
    await fs.copyFile(this.dbPath, backupFile);
    
    // 压缩备份
    await this.compress(backupFile);
    
    console.log(`Backup created: ${backupFile}`);
    
    // 清理旧备份（保留最近 7 个）
    await this.cleanupOldBackups();
    
    return backupFile;
  }
  
  async compress(filePath) {
    // 使用 zlib 或其他压缩库
    // 简化示例
    const gzipped = filePath + '.gz';
    // ... 压缩逻辑
    return gzipped;
  }
  
  async cleanupOldBackups(keepCount = 7) {
    const backups = await fs.readdir(this.backupDir);
    const sorted = backups.sort().reverse();
    
    for (let i = keepCount; i < sorted.length; i++) {
      await fs.unlink(path.join(this.backupDir, sorted[i]));
    }
  }
}
```

---

**下一节：** [c3-3 向量数据库与语义搜索](./c3-3-vector-database.md)  
**上一节：** [c3-1 短期记忆：内存缓存与会话管理](./c3-1-short-term-memory.md)
