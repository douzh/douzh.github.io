# c7-5 接口定义与抽象

## 1. 概述

良好的接口设计是技能生态系统的基石。本章详解如何设计清晰、可扩展的技能接口。

## 2. TypeScript 接口设计

### 2.1 基础 Skill 接口

```typescript
// skill-interface.ts

/**
 * 技能执行输入参数
 */
export interface SkillInput<T = any> {
  /** 参数对象 */
  params: T;
  
  /** 用户上下文信息 */
  context?: {
    userId: string;
    sessionId: string;
    platform: string;
    locale?: string;
  };
  
  /** 超时时间（毫秒） */
  timeout?: number;
}

/**
 * 技能执行结果
 */
export interface SkillOutput<T = any> {
  /** 是否成功 */
  success: boolean;
  
  /** 输出内容（给用户的展示） */
  content: ContentItem[];
  
  /** 结构化数据（供后续处理） */
  data?: T;
  
  /** 错误信息（如果失败） */
  error?: {
    code: string;
    message: string;
    details?: any;
  };
  
  /** 执行元数据 */
  metadata?: {
    executionTime: number;
    version: string;
    cached?: boolean;
  };
}

/**
 * 内容项类型
 */
export type ContentItem = 
  | TextContent
  | ImageContent
  | VideoContent
  | AudioContent
  | InteractiveContent;

export interface TextContent {
  type: 'text';
  text: string;
}

export interface ImageContent {
  type: 'image';
  url: string;
  caption?: string;
}

export interface VideoContent {
  type: 'video';
  url: string;
  thumbnail?: string;
  duration?: number;
}

export interface AudioContent {
  type: 'audio';
  url: string;
  duration?: number;
  title?: string;
}

export interface InteractiveContent {
  type: 'interactive';
  buttons?: Button[];
  options?: SelectOption[];
  form?: FormField[];
}

interface Button {
  id: string;
  text: string;
  action: 'callback' | 'link' | 'submit';
  value?: string;
  url?: string;
}

interface SelectOption {
  label: string;
  value: string;
}

interface FormField {
  name: string;
  label: string;
  type: 'text' | 'number' | 'select' | 'date';
  required?: boolean;
  options?: SelectOption[];
}

/**
 * 技能接口定义
 */
export interface Skill<TInput = any, TOutput = any> {
  /** 技能元数据 */
  readonly metadata: SkillMetadata;
  
  /** 参数 Schema */
  readonly parameters: JSONSchema;
  
  /** 执行函数 */
  execute(input: SkillInput<TInput>): Promise<SkillOutput<TOutput>>;
  
  /** 验证参数 */
  validate?(params: any): params is TInput;
  
  /** 初始化（可选） */
  initialize?(): Promise<void>;
  
  /** 销毁（可选） */
  destroy?(): Promise<void>;
}

/**
 * 技能元数据
 */
export interface SkillMetadata {
  /** 唯一标识 */
  name: string;
  
  /** 版本号 */
  version: string;
  
  /** 描述 */
  description: string;
  
  /** 作者信息 */
  author?: {
    name: string;
    email?: string;
    url?: string;
  };
  
  /** 分类标签 */
  tags?: string[];
  
  /** 所属分类 */
  category?: string;
  
  /** 依赖的其他技能 */
  dependencies?: Record<string, string>;
  
  /** 权限要求 */
  permissions?: Permission[];
}

/**
 * 权限类型
 */
export type Permission = 
  | 'network-access'
  | 'file-read'
  | 'file-write'
  | 'database-access'
  | 'execute-command';

/**
 * JSON Schema 定义
 */
export interface JSONSchema {
  type: string;
  properties?: Record<string, any>;
  required?: string[];
  additionalProperties?: boolean;
}
```

### 2.2 实现示例

```typescript
// weather-skill.ts
import { Skill, SkillInput, SkillOutput, SkillMetadata } from './skill-interface';

export class WeatherSkill implements Skill<WeatherParams, WeatherData> {
  readonly metadata: SkillMetadata = {
    name: 'weather-skill',
    version: '1.0.0',
    description: '获取全球任意城市的实时天气预报',
    author: {
      name: 'WeatherTeam',
      email: 'support@weather.example.com'
    },
    tags: ['天气', '预报', '气象'],
    category: 'data-services',
    permissions: ['network-access']
  };
  
  readonly parameters = {
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
      },
      unit: {
        type: 'string',
        enum: ['celsius', 'fahrenheit'],
        default: 'celsius'
      }
    },
    required: ['location']
  };
  
  async initialize() {
    console.log('Weather skill initialized');
  }
  
  validate(params: any): params is WeatherParams {
    return (
      typeof params === 'object' &&
      typeof params.location === 'string' &&
      (!params.days || typeof params.days === 'number')
    );
  }
  
  async execute(input: SkillInput<WeatherParams>): Promise<SkillOutput<WeatherData>> {
    const startTime = Date.now();
    
    try {
      // 参数验证
      if (!this.validate(input.params)) {
        throw new Error('Invalid parameters');
      }
      
      // 调用天气 API
      const weatherData = await this.fetchWeather(
        input.params.location,
        input.params.days,
        input.params.unit
      );
      
      // 格式化输出
      const content = this.formatOutput(weatherData, input.params);
      
      return {
        success: true,
        content,
        data: weatherData,
        metadata: {
          executionTime: Date.now() - startTime,
          version: this.metadata.version
        }
      };
      
    } catch (error) {
      return {
        success: false,
        content: [{
          type: 'text',
          text: `获取天气失败：${error.message}`
        }],
        error: {
          code: 'WEATHER_API_ERROR',
          message: error.message
        },
        metadata: {
          executionTime: Date.now() - startTime,
          version: this.metadata.version
        }
      };
    }
  }
  
  private async fetchWeather(location: string, days: number = 1, unit: string = 'celsius'): Promise<WeatherData> {
    // 实际 API 调用
    const response = await fetch(`https://api.weather.com/v3/w/conditions/local?location=${location}`);
    const data = await response.json();
    
    return {
      location,
      current: {
        temp: this.convertTemp(data.temperature, unit),
        condition: data.wxPhraseShort,
        humidity: data.relativeHumidity,
        wind: data.windSpeed
      },
      forecast: data.forecasts?.slice(0, days) || []
    };
  }
  
  private formatOutput(data: WeatherData, params: any) {
    const unitSymbol = params.unit === 'celsius' ? '°C' : '°F';
    
    let text = `📍 ${data.location} 天气\n\n`;
    text += `当前：${data.current.temp}${unitSymbol} ${data.current.condition}\n`;
    text += `湿度：${data.current.humidity}% | 风力：${data.current.wind}级\n`;
    
    return [{ type: 'text' as const, text }];
  }
  
  private convertTemp(celsius: number, unit: string): number {
    return unit === 'fahrenheit' ? (celsius * 9/5 + 32) : celsius;
  }
}

interface WeatherParams {
  location: string;
  days?: number;
  unit?: 'celsius' | 'fahrenheit';
}

interface WeatherData {
  location: string;
  current: {
    temp: number;
    condition: string;
    humidity: number;
    wind: number;
  };
  forecast: any[];
}
```

## 3. 装饰器模式

```typescript
// decorators.ts

/**
 * 技能装饰器
 */
export function SkillComponent(options: {
  name: string;
  version: string;
  description: string;
}) {
  return function <T extends new (...args: any[]) => {}>(constructor: T) {
    return class extends constructor {
      readonly metadata = {
        name: options.name,
        version: options.version,
        description: options.description,
        createdAt: new Date().toISOString()
      };
    };
  };
}

/**
 * 参数验证装饰器
 */
export function ValidateParams(schema: any) {
  return function (
    target: any,
    propertyKey: string,
    descriptor: PropertyDescriptor
  ) {
    const originalMethod = descriptor.value;
    
    descriptor.value = async function (...args: any[]) {
      const input = args[0];
      
      // 验证参数
      const ajv = require('ajv').default;
      const validate = new ajv().compile(schema);
      
      if (!validate(input.params)) {
        throw new Error(`Validation failed: ${validate.errors}`);
      }
      
      return await originalMethod.apply(this, args);
    };
    
    return descriptor;
  };
}

/**
 * 缓存装饰器
 */
export function Cache(ttl: number) {
  const cache = new Map<string, { data: any; timestamp: number }>();
  
  return function (
    target: any,
    propertyKey: string,
    descriptor: PropertyDescriptor
  ) {
    const originalMethod = descriptor.value;
    
    descriptor.value = async function (...args: any[]) {
      const cacheKey = JSON.stringify(args[0]);
      const cached = cache.get(cacheKey);
      
      if (cached && Date.now() - cached.timestamp < ttl) {
        return { ...cached.data, metadata: { ...cached.data.metadata, cached: true } };
      }
      
      const result = await originalMethod.apply(this, args);
      cache.set(cacheKey, { data: result, timestamp: Date.now() });
      
      return result;
    };
    
    return descriptor;
  };
}

// 使用装饰器
@SkillComponent({
  name: 'enhanced-weather',
  version: '2.0.0',
  description: '增强版天气技能（带缓存）'
})
export class EnhancedWeatherSkill {
  @ValidateParams(weatherSchema)
  @Cache(5 * 60 * 1000) // 5 分钟缓存
  async execute(input: SkillInput<WeatherParams>) {
    // 实际执行逻辑
  }
}
```

---

**下一节：** [c7-6 依赖注入](./c7-6-dependency-injection.md)  
**上一节：** [c7-4 单文件脚本开发](./c7-4-single-file-scripting.md)
