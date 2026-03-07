# c7 其他主题概要

## c7-4 单文件脚本开发

```javascript
// single-file-skill.js
/**
 * @name weather-skill
 * @version 1.0.0
 * @description 简单的天气查询技能
 */

// 元数据定义
export const metadata = {
  name: 'weather-skill',
  version: '1.0.0',
  description: '获取城市天气信息'
};

// 参数定义
export const parameters = {
  type: 'object',
  properties: {
    location: { type: 'string' }
  },
  required: ['location']
};

// 执行函数
export async function execute(params) {
  const weather = await fetchWeather(params.location);
  
  return {
    content: [{
      type: 'text',
      text: `${params.location}今天${weather.condition}`
    }]
  };
}

// 辅助函数
async function fetchWeather(location) {
  // 实际 API 调用
  return { condition: '晴朗', temp: 25 };
}
```

## c7-5 接口设计最佳实践

```typescript
// 良好的接口设计
interface SkillInterface {
  // 清晰的命名
  execute(input: WeatherInput): Promise<WeatherOutput>;
  
  // 完整的类型定义
  validate(input: any): input is WeatherInput;
  
  // 错误处理
  handleError(error: Error): FallbackResponse;
}

// 输入验证
class WeatherInputValidator {
  static validate(input: any): input is WeatherInput {
    return (
      typeof input === 'object' &&
      typeof input.location === 'string' &&
      input.location.length > 0
    );
  }
}
```

## c7-6 依赖注入

```typescript
// IoC 容器
class SkillContainer {
  private dependencies: Map<string, any>;
  
  register<T>(token: string, dependency: T) {
    this.dependencies.set(token, dependency);
  }
  
  get<T>(token: string): T {
    const dep = this.dependencies.get(token);
    if (!dep) {
      throw new Error(`Dependency ${token} not found`);
    }
    return dep;
  }
}

// 使用示例
const container = new SkillContainer();

container.register('logger', new Logger());
container.register('httpClient', new HttpClient());

class WeatherSkill {
  constructor(
    private logger = container.get('logger'),
    private http = container.get('httpClient')
  ) {}
  
  async execute(input: any) {
    this.logger.info('Executing weather skill');
    const response = await this.http.get('/weather');
    return response.data;
  }
}
```

## c7-7 技能市场架构

```typescript
// 技能市场核心接口
interface SkillMarketplace {
  // 发布技能
  publish(skill: SkillPackage): Promise<string>;
  
  // 搜索技能
  search(query: SearchQuery): Promise<SkillResult[]>;
  
  // 下载技能
  download(skillId: string): Promise<SkillPackage>;
  
  // 评价技能
  rate(skillId: string, rating: number, review?: string): Promise<void>;
  
  // 统计信息
  getStats(skillId: string): Promise<SkillStats>;
}

// CDN 分发
class SkillCDN {
  async upload(skillId: string, packagePath: string) {
    // 上传到 CDN
    const cdnUrl = await cdn.upload(packagePath);
    
    // 更新技能元数据
    await registry.update(skillId, { downloadUrl: cdnUrl });
  }
  
  async download(skillId: string) {
    const skill = await registry.get(skillId);
    return await fetch(skill.downloadUrl);
  }
}
```

---

**c7 模块核心内容完成！**
