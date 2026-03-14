# c7-2 技能注册与发现机制

## 1. 概述

技能注册与发现是 MCP 生态系统的核心，让 AI智能体能够动态发现和加载可用工具。本章详解技能注册表、元数据管理和查询机制。

## 2. 技能元数据

### 2.1 技能描述格式

```typescript
interface SkillMetadata {
  // 基本信息
  name: string;              // 技能名称（唯一标识）
  version: string;           // 语义化版本号
  description: string;       // 人类可读的描述
  
  // 作者信息
  author: {
    name: string;
    email?: string;
    url?: string;
  };
  
  // 技术细节
  interface: {
    type: 'mcp' | 'http' | 'websocket';
    endpoint?: string;       // HTTP/WebSocket 端点
    transport?: 'stdio' | 'sse'; // MCP 传输方式
  };
  
  // 输入输出
  inputSchema: JSONSchema;   // 参数验证 schema
  outputSchema?: JSONSchema; // 返回值 schema
  
  // 分类标签
  tags: string[];            // 搜索标签
  category: string;          // 所属分类
  
  // 依赖关系
  dependencies?: {
    [skillName: string]: string; // 依赖的技能及版本
  };
  
  // 权限要求
  permissions: Permission[];
  
  // 统计信息
  stats?: {
    downloads: number;
    rating: number;
    lastUpdated: string;
  };
}

// 示例：天气技能
const weatherSkill: SkillMetadata = {
  name: 'weather-service',
  version: '1.2.0',
  description: '获取全球任意城市的实时天气预报和历史数据',
  
  author: {
    name: 'WeatherTeam',
    email: 'support@weather.example.com',
    url: 'https://weather.example.com'
  },
  
  interface: {
    type: 'mcp',
    transport: 'stdio'
  },
  
  inputSchema: {
    type: 'object',
    properties: {
      location: {
        type: 'string',
        description: '城市名称或经纬度'
      },
      days: {
        type: 'integer',
        minimum: 1,
        maximum: 14,
        default: 1
      }
    },
    required: ['location']
  },
  
  tags: ['天气', '预报', '气象'],
  category: 'data-services',
  
  permissions: ['network-access'],
  
  stats: {
    downloads: 15420,
    rating: 4.8,
    lastUpdated: '2026-03-01T00:00:00Z'
  }
};
```

## 3. 注册中心实现

### 3.1 内存注册表

```typescript
class SkillRegistry {
  private skills: Map<string, SkillMetadata>;
  private listeners: Set<(event: RegistryEvent) => void>;
  
  constructor() {
    this.skills = new Map();
    this.listeners = new Set();
  }
  
  // 注册技能
  register(metadata: SkillMetadata): void {
    if (this.skills.has(metadata.name)) {
      throw new Error(`Skill "${metadata.name}" already registered`);
    }
    
    // 验证元数据
    this.validateMetadata(metadata);
    
    // 存储
    this.skills.set(metadata.name, metadata);
    
    // 通知监听器
    this.emit({ type: 'REGISTER', skill: metadata });
    
    console.log(`✅ Registered skill: ${metadata.name} v${metadata.version}`);
  }
  
  // 注销技能
  unregister(skillName: string, reason?: string): void {
    const skill = this.skills.get(skillName);
    
    if (!skill) {
      throw new Error(`Skill "${skillName}" not found`);
    }
    
    this.skills.delete(skillName);
    this.emit({ type: 'UNREGISTER', skill, reason });
    
    console.log(`❌ Unregistered skill: ${skillName}${reason ? ` (${reason})` : ''}`);
  }
  
  // 更新技能
  update(skillName: string, newMetadata: SkillMetadata): void {
    const oldMetadata = this.skills.get(skillName);
    
    if (!oldMetadata) {
      throw new Error(`Skill "${skillName}" not found`);
    }
    
    // 版本号必须升级
    if (!this.isVersionHigher(newMetadata.version, oldMetadata.version)) {
      throw new Error('New version must be higher than current version');
    }
    
    this.skills.set(skillName, newMetadata);
    this.emit({ type: 'UPDATE', oldSkill: oldMetadata, newSkill: newMetadata });
  }
  
  // 查询技能
  get(skillName: string): SkillMetadata | undefined {
    return this.skills.get(skillName);
  }
  
  // 列出所有技能
  list(options?: ListOptions): SkillMetadata[] {
    let skills = Array.from(this.skills.values());
    
    // 按类别过滤
    if (options?.category) {
      skills = skills.filter(s => s.category === options.category);
    }
    
    // 按标签过滤
    if (options?.tags) {
      skills = skills.filter(s => 
        options.tags.some(tag => s.tags.includes(tag))
      );
    }
    
    // 排序
    if (options?.sortBy) {
      skills.sort((a, b) => {
        switch (options.sortBy) {
          case 'name':
            return a.name.localeCompare(b.name);
          case 'rating':
            return (b.stats?.rating || 0) - (a.stats?.rating || 0);
          case 'downloads':
            return (b.stats?.downloads || 0) - (a.stats?.downloads || 0);
          default:
            return 0;
        }
      });
    }
    
    return skills;
  }
  
  // 搜索技能
  search(query: string): SkillMetadata[] {
    const queryLower = query.toLowerCase();
    
    return Array.from(this.skills.values()).filter(skill => {
      // 在名称、描述、标签中搜索
      return (
        skill.name.toLowerCase().includes(queryLower) ||
        skill.description.toLowerCase().includes(queryLower) ||
        skill.tags.some(tag => tag.toLowerCase().includes(queryLower))
      );
    });
  }
  
  // 添加事件监听
  on(event: 'register' | 'unregister' | 'update', callback: (e: RegistryEvent) => void) {
    this.listeners.add(callback as any);
  }
  
  private emit(event: RegistryEvent) {
    this.listeners.forEach(listener => listener(event));
  }
  
  private validateMetadata(metadata: SkillMetadata) {
    if (!metadata.name || !metadata.version || !metadata.description) {
      throw new Error('Missing required fields: name, version, description');
    }
    
    if (!/^\d+\.\d+\.\d+$/.test(metadata.version)) {
      throw new Error('Version must be in semver format (e.g., 1.0.0)');
    }
  }
  
  private isVersionHigher(newVer: string, oldVer: string): boolean {
    const newParts = newVer.split('.').map(Number);
    const oldParts = oldVer.split('.').map(Number);
    
    for (let i = 0; i < 3; i++) {
      if (newParts[i] > oldParts[i]) return true;
      if (newParts[i] < oldParts[i]) return false;
    }
    
    return false;
  }
}

// 使用示例
const registry = new SkillRegistry();

// 注册技能
registry.register(weatherSkill);

// 搜索技能
const results = registry.search('天气');
console.log(`Found ${results.length} skills`);

// 列出高评分技能
const topRated = registry.list({
  sortBy: 'rating'
});
```

### 3.2 持久化注册表（SQLite）

```typescript
import Database from 'better-sqlite3';

class PersistentSkillRegistry {
  private db: Database.Database;
  
  constructor(dbPath: string = './skills.db') {
    this.db = new Database(dbPath);
    this.initDatabase();
  }
  
  private initDatabase() {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS skills (
        name TEXT PRIMARY KEY,
        version TEXT NOT NULL,
        description TEXT NOT NULL,
        author TEXT NOT NULL,
        category TEXT,
        tags TEXT,
        interface TEXT,
        input_schema TEXT,
        output_schema TEXT,
        permissions TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );
      
      CREATE TABLE IF NOT EXISTS skill_stats (
        skill_name TEXT PRIMARY KEY,
        downloads INTEGER DEFAULT 0,
        rating REAL DEFAULT 0,
        rating_count INTEGER DEFAULT 0,
        FOREIGN KEY (skill_name) REFERENCES skills(name)
      );
      
      CREATE INDEX idx_category ON skills(category);
      CREATE INDEX idx_tags ON skills(tags);
    `);
  }
  
  register(metadata: SkillMetadata) {
    const stmt = this.db.prepare(`
      INSERT OR REPLACE INTO skills 
      (name, version, description, author, category, tags, interface, input_schema, output_schema, permissions, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    `);
    
    stmt.run(
      metadata.name,
      metadata.version,
      metadata.description,
      JSON.stringify(metadata.author),
      metadata.category,
      JSON.stringify(metadata.tags),
      JSON.stringify(metadata.interface),
      JSON.stringify(metadata.inputSchema),
      JSON.stringify(metadata.outputSchema),
      JSON.stringify(metadata.permissions)
    );
    
    // 初始化统计数据
    const statsStmt = this.db.prepare(`
      INSERT OR IGNORE INTO skill_stats (skill_name)
      VALUES (?)
    `);
    
    statsStmt.run(metadata.name);
  }
  
  get(skillName: string): SkillMetadata | null {
    const row = this.db.prepare('SELECT * FROM skills WHERE name = ?').get(skillName);
    
    if (!row) return null;
    
    return {
      name: row.name,
      version: row.version,
      description: row.description,
      author: JSON.parse(row.author),
      category: row.category,
      tags: JSON.parse(row.tags),
      interface: JSON.parse(row.interface),
      inputSchema: JSON.parse(row.input_schema),
      outputSchema: JSON.parse(row.output_schema),
      permissions: JSON.parse(row.permissions)
    };
  }
  
  search(query: string): SkillMetadata[] {
    const rows = this.db.prepare(`
      SELECT * FROM skills
      WHERE name LIKE ? OR description LIKE ? OR tags LIKE ?
      ORDER BY name
    `).all(`%${query}%`, `%${query}%`, `%${query}%`);
    
    return rows.map(row => this.rowToMetadata(row));
  }
  
  list(options?: ListOptions): SkillMetadata[] {
    let sql = 'SELECT * FROM skills';
    const params: any[] = [];
    
    if (options?.category) {
      sql += ' WHERE category = ?';
      params.push(options.category);
    }
    
    sql += ' ORDER BY ';
    
    switch (options?.sortBy) {
      case 'name':
        sql += 'name';
        break;
      case 'updated_at':
        sql += 'updated_at DESC';
        break;
      default:
        sql += 'name';
    }
    
    const rows = this.db.prepare(sql).all(...params);
    return rows.map(row => this.rowToMetadata(row));
  }
  
  private rowToMetadata(row: any): SkillMetadata {
    return {
      name: row.name,
      version: row.version,
      description: row.description,
      author: JSON.parse(row.author),
      category: row.category,
      tags: JSON.parse(row.tags),
      interface: JSON.parse(row.interface),
      inputSchema: JSON.parse(row.input_schema),
      outputSchema: JSON.parse(row.output_schema),
      permissions: JSON.parse(row.permissions)
    };
  }
}
```

## 4. 分布式服务发现

### 4.1 Consul 集成

```typescript
import Consul from 'consul';

class DistributedSkillRegistry {
  private consul: Consul.Consul;
  private serviceName: string;
  
  constructor(consulHost: string, serviceName: string) {
    this.consul = new Consul({ host: consulHost });
    this.serviceName = serviceName;
  }
  
  // 注册技能服务
  async registerService(skillMetadata: SkillMetadata, port: number) {
    await this.consul.agent.service.register({
      name: `${this.serviceName}-${skillMetadata.name}`,
      id: `${this.serviceName}-${skillMetadata.name}-${skillMetadata.version}`,
      port,
      address: 'localhost',
      tags: [
        `version:${skillMetadata.version}`,
        `category:${skillMetadata.category}`,
        ...skillMetadata.tags
      ],
      meta: {
        description: skillMetadata.description,
        author: JSON.stringify(skillMetadata.author),
        inputSchema: JSON.stringify(skillMetadata.inputSchema)
      },
      check: {
        http: `http://localhost:${port}/health`,
        interval: '10s',
        timeout: '5s'
      }
    });
    
    console.log(`✅ Service registered: ${skillMetadata.name}`);
  }
  
  // 发现技能服务
  async discoverSkills(query?: {
    category?: string;
    tags?: string[];
  }): Promise<ServiceInstance[]> {
    const services = await this.consul.health.service({
      service: this.serviceName,
      passing: true
    });
    
    let instances = services.map(s => ({
      name: this.extractSkillName(s.Service.Service),
      version: this.extractVersion(s.Service.Tags),
      address: s.Service.Address,
      port: s.Service.Port,
      metadata: this.parseMetadata(s.Service.Meta),
      healthy: true
    }));
    
    // 过滤
    if (query?.category) {
      instances = instances.filter(i => i.metadata.category === query.category);
    }
    
    if (query?.tags) {
      instances = instances.filter(i =>
        query.tags!.some(tag => i.metadata.tags.includes(tag))
      );
    }
    
    return instances;
  }
  
  // 注销服务
  async deregisterService(skillName: string) {
    const serviceId = `${this.serviceName}-${skillName}`;
    await this.consul.agent.service.deregister(serviceId);
    console.log(`❌ Service deregistered: ${skillName}`);
  }
  
  private extractSkillName(serviceName: string): string {
    return serviceName.replace(`${this.serviceName}-`, '');
  }
  
  private extractVersion(tags: string[]): string {
    const versionTag = tags.find(t => t.startsWith('version:'));
    return versionTag?.split(':')[1] || 'unknown';
  }
  
  private parseMetadata(meta: any): any {
    try {
      return {
        description: meta.description,
        author: JSON.parse(meta.author),
        category: meta.category || 'general',
        tags: []
      };
    } catch {
      return {};
    }
  }
}

// 使用示例
const distributedRegistry = new DistributedSkillRegistry(
  'localhost:8500',
  'mcp-skills'
);

// 注册服务
await distributedRegistry.registerService(weatherSkill, 3000);

// 发现天气类技能
const weatherServices = await distributedRegistry.discoverSkills({
  category: 'data-services',
  tags: ['天气']
});

for (const service of weatherServices) {
  console.log(`Found: ${service.name} v${service.version} at ${service.address}:${service.port}`);
}
```

---

**下一节：** [c7-3 热插拔机制](./c7-3-hot-swappable.md)  
**上一节：** [c7-1 MCP 协议详解](./c7-1-mcp-protocol.md)
