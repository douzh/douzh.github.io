# c7-6 依赖注入与 c7-7 技能市场架构

## c7-6 依赖注入（DI）

### IoC 容器实现

```typescript
// ioc-container.ts

/**
 * 依赖注入令牌
 */
export class InjectionToken<T = any> {
  constructor(public readonly name: string) {}
}

/**
 * 依赖提供者
 */
export interface Provider<T = any> {
  provide: InjectionToken<T> | string;
  useClass?: new (...args: any[]) => T;
  useValue?: T;
  useFactory?: (...args: any[]) => T;
  inject?: (InjectionToken<any> | string)[];
}

/**
 * IoC 容器
 */
export class Container {
  private providers = new Map<string, Provider>();
  private instances = new Map<string, any>();
  private singletons = new Set<string>();
  
  /**
   * 注册提供者
   */
  register<T>(provider: Provider<T>, singleton = false) {
    const token = typeof provider.provide === 'string' 
      ? provider.provide 
      : provider.provide.name;
    
    this.providers.set(token, provider);
    
    if (singleton) {
      this.singletons.add(token);
    }
  }
  
  /**
   * 获取依赖
   */
  get<T>(token: InjectionToken<T> | string): T {
    const tokenStr = typeof token === 'string' ? token : token.name;
    
    // 检查是否已有实例
    if (this.instances.has(tokenStr)) {
      return this.instances.get(tokenStr);
    }
    
    const provider = this.providers.get(tokenStr);
    
    if (!provider) {
      throw new Error(`No provider found for ${tokenStr}`);
    }
    
    let instance: T;
    
    // 根据提供者类型创建实例
    if (provider.useClass) {
      // 自动注入构造函数依赖
      const dependencies = this.resolveDependencies(provider.useClass);
      instance = new provider.useClass(...dependencies);
    } else if (provider.useFactory) {
      // 工厂方法创建
      const deps = provider.inject?.map(dep => this.get(dep)) || [];
      instance = provider.useFactory(...deps);
    } else if (provider.useValue) {
      // 直接使用值
      instance = provider.useValue;
    } else {
      throw new Error(`Invalid provider configuration for ${tokenStr}`);
    }
    
    // 单例缓存
    if (this.singletons.has(tokenStr)) {
      this.instances.set(tokenStr, instance);
    }
    
    return instance;
  }
  
  /**
   * 解析构造函数依赖
   */
  private resolveDependencies(constructor: new (...args: any[]) => any) {
    const params = Reflect.getMetadata('design:paramtypes', constructor) || [];
    
    return params.map((paramType: any) => {
      const token = paramType.name;
      return this.get(token);
    });
  }
  
  /**
   * 清空容器
   */
  clear() {
    this.providers.clear();
    this.instances.clear();
    this.singletons.clear();
  }
}

// 使用示例
const HTTP_CLIENT = new InjectionToken('HttpClient');
const LOGGER = new InjectionToken('Logger');
const API_KEY = new InjectionToken('ApiKey');

class HttpClient {
  constructor(private apiKey: string) {}
  
  async get(url: string) {
    return fetch(url, {
      headers: { 'Authorization': `Bearer ${this.apiKey}` }
    });
  }
}

class Logger {
  log(message: string) {
    console.log(`[LOG] ${message}`);
  }
}

class WeatherService {
  constructor(
    private http: HttpClient,
    private logger: Logger
  ) {}
  
  async getWeather(location: string) {
    this.logger.log(`Fetching weather for ${location}`);
    const response = await this.http.get(`/weather?location=${location}`);
    return response.json();
  }
}

// 配置容器
const container = new Container();

container.register({
  provide: API_KEY,
  useValue: process.env.WEATHER_API_KEY
}, true);

container.register({
  provide: HTTP_CLIENT,
  useClass: HttpClient,
  inject: [API_KEY]
});

container.register({
  provide: LOGGER,
  useClass: Logger
});

container.register({
  provide: 'WeatherService',
  useClass: WeatherService,
  inject: [HTTP_CLIENT, LOGGER]
});

// 使用
const weatherService = container.get('WeatherService');
const weather = await weatherService.getWeather('北京');
```

## c7-7 技能市场架构

### 核心接口定义

```typescript
// marketplace-interface.ts

/**
 * 技能市场接口
 */
export interface SkillMarketplace {
  /**
   * 发布技能
   */
  publish(skill: SkillPackage): Promise<string>;
  
  /**
   * 搜索技能
   */
  search(query: SearchQuery): Promise<SkillResult[]>;
  
  /**
   * 下载技能
   */
  download(skillId: string): Promise<SkillPackage>;
  
  /**
   * 评价技能
   */
  rate(skillId: string, rating: number, review?: string): Promise<void>;
  
  /**
   * 获取统计信息
   */
  getStats(skillId: string): Promise<SkillStats>;
  
  /**
   * 获取分类列表
   */
  getCategories(): Promise<Category[]>;
  
  /**
   * 获取热门技能
   */
  getTopSkills(limit?: number): Promise<SkillResult[]>;
}

/**
 * 技能包
 */
export interface SkillPackage {
  id: string;
  metadata: SkillMetadata;
  code: string;
  version: string;
  publishedAt: string;
  author: AuthorInfo;
  license: string;
  downloads: number;
}

/**
 * 搜索结果
 */
export interface SkillResult extends SkillMetadata {
  id: string;
  rating: number;
  reviewCount: number;
  downloadCount: number;
  lastUpdated: string;
  verified?: boolean;
}

/**
 * 搜索查询
 */
export interface SearchQuery {
  keyword?: string;
  category?: string;
  tags?: string[];
  minRating?: number;
  sortBy?: 'relevance' | 'rating' | 'downloads' | 'newest';
  sortOrder?: 'asc' | 'desc';
  limit?: number;
  offset?: number;
}

/**
 * 统计信息
 */
export interface SkillStats {
  totalDownloads: number;
  totalRatings: number;
  averageRating: number;
  dailyActiveUsers: number;
  successRate: number;
  averageResponseTime: number;
}

/**
 * 分类信息
 */
export interface Category {
  id: string;
  name: string;
  description: string;
  icon?: string;
  skillCount: number;
  parentCategoryId?: string;
}
```

### CDN 分发系统

```typescript
// cdn-distribution.ts

class SkillCDN {
  private cdnProvider: CDNProvider;
  private registry: SkillRegistry;
  
  constructor(cdnProvider: CDNProvider, registry: SkillRegistry) {
    this.cdnProvider = cdnProvider;
    this.registry = registry;
  }
  
  /**
   * 上传技能到 CDN
   */
  async upload(skillId: string, packagePath: string): Promise<string> {
    // 1. 验证技能
    const skill = await this.registry.get(skillId);
    if (!skill) {
      throw new Error(`Skill ${skillId} not found`);
    }
    
    // 2. 压缩打包
    const tarball = await this.createTarball(packagePath);
    
    // 3. 上传到 CDN
    const cdnUrl = await this.cdnProvider.upload(tarball, {
      contentType: 'application/gzip',
      metadata: {
        skillId,
        version: skill.version,
        uploadedAt: Date.now()
      }
    });
    
    // 4. 更新技能元数据
    await this.registry.update(skillId, {
      downloadUrl: cdnUrl,
      cdnProvider: this.cdnProvider.name
    });
    
    // 5. 预热的 CDN 边缘节点
    await this.warmupCache(cdnUrl);
    
    return cdnUrl;
  }
  
  /**
   * 从 CDN 下载技能
   */
  async download(skillId: string): Promise<Buffer> {
    const skill = await this.registry.get(skillId);
    
    if (!skill.downloadUrl) {
      throw new Error(`Skill ${skillId} has no download URL`);
    }
    
    // 从最近的边缘节点下载
    const buffer = await this.cdnProvider.download(skill.downloadUrl);
    
    // 解压
    const extracted = await this.extractTarball(buffer);
    
    return extracted;
  }
  
  /**
   * 增量更新
   */
  async incrementalUpdate(skillId: string, newVersion: string): Promise<void> {
    const current = await this.registry.get(skillId);
    
    // 计算差异
    const diff = await this.calculateDiff(current.version, newVersion);
    
    // 只上传变更的部分
    const patchUrl = await this.cdnProvider.upload(diff.patch, {
      contentType: 'application/octet-stream',
      metadata: {
        type: 'patch',
        baseVersion: current.version,
        targetVersion: newVersion
      }
    });
    
    await this.registry.update(skillId, {
      patches: [...(current.patches || []), {
        from: current.version,
        to: newVersion,
        url: patchUrl
      }]
    });
  }
  
  private async createTarball(sourcePath: string): Promise<Buffer> {
    const tar = require('tar');
    const zlib = require('zlib');
    
    return new Promise((resolve, reject) => {
      const chunks: Buffer[] = [];
      
      tar.pack(sourcePath)
        .pipe(zlib.createGzip())
        .on('data', chunk => chunks.push(chunk))
        .on('end', () => resolve(Buffer.concat(chunks)))
        .on('error', reject);
    });
  }
  
  private async extractTarball(buffer: Buffer): Promise<Buffer> {
    const tar = require('tar');
    const zlib = require('zlib');
    
    return new Promise((resolve, reject) => {
      const chunks: Buffer[] = [];
      
      zlib.createGunzip(buffer)
        .pipe(tar.extract())
        .on('data', chunk => chunks.push(chunk))
        .on('end', () => resolve(Buffer.concat(chunks)))
        .on('error', reject);
    });
  }
  
  private async warmupCache(url: string): Promise<void> {
    // 请求所有边缘节点预热
    await this.cdnProvider.purgeCache(url);
  }
}
```

---

**c7 模块全部完成！** 🎉
