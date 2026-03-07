# c7-4 单文件脚本开发

## 1. 概述

单文件脚本是最简单的技能开发方式，零配置、即插即用。本章详解如何快速开发和部署单文件技能。

## 2. 基础结构

### 2.1 标准单文件模板

```javascript
/**
 * @name weather-skill
 * @version 1.0.0
 * @description 简单的天气查询技能
 * @author YourName <your.email@example.com>
 * 
 * @mcp true
 * @mcp_transport stdio
 */

// ============ 元数据定义 ============
export const metadata = {
  name: 'weather-skill',
  version: '1.0.0',
  description: '获取全球任意城市的实时天气预报',
  author: {
    name: 'YourName',
    email: 'your.email@example.com'
  },
  tags: ['天气', '预报', '气象'],
  category: 'data-services'
};

// ============ 参数定义 ============
export const parameters = {
  type: 'object',
  properties: {
    location: {
      type: 'string',
      description: '城市名称或经纬度坐标',
      examples: ['北京', '上海', '39.9°N, 116.4°E']
    },
    days: {
      type: 'integer',
      minimum: 1,
      maximum: 14,
      default: 1,
      description: '预报天数（1-14 天）'
    },
    unit: {
      type: 'string',
      enum: ['celsius', 'fahrenheit'],
      default: 'celsius',
      description: '温度单位'
    }
  },
  required: ['location']
};

// ============ 执行函数 ============
export async function execute(params) {
  try {
    // 参数验证
    validateParams(params);
    
    // 调用天气 API
    const weatherData = await fetchWeather(
      params.location,
      params.days,
      params.unit
    );
    
    // 格式化输出
    return formatOutput(weatherData, params);
    
  } catch (error) {
    console.error('Weather skill error:', error);
    throw error;
  }
}

// ============ 辅助函数 ============

function validateParams(params) {
  if (!params.location) {
    throw new Error('Location is required');
  }
  
  if (params.days && (params.days < 1 || params.days > 14)) {
    throw new Error('Days must be between 1 and 14');
  }
}

async function fetchWeather(location, days, unit) {
  // 实际 API 调用（示例使用模拟数据）
  const mockData = {
    location: '北京',
    current: {
      temp: 25,
      condition: '晴朗',
      humidity: 45,
      wind: 12
    },
    forecast: []
  };
  
  // 生成未来几天的预报
  for (let i = 0; i < days; i++) {
    mockData.forecast.push({
      date: new Date(Date.now() + i * 24 * 60 * 60 * 1000),
      temp_max: 25 + Math.random() * 5,
      temp_min: 15 + Math.random() * 3,
      condition: ['晴朗', '多云', '小雨'][Math.floor(Math.random() * 3)]
    });
  }
  
  return mockData;
}

function formatOutput(data, params) {
  const unitSymbol = params.unit === 'celsius' ? '°C' : '°F';
  
  let output = `📍 ${data.location} 天气\n\n`;
  output += `当前：${data.current.temp}${unitSymbol} ${data.current.condition}\n`;
  output += `湿度：${data.current.humidity}% | 风力：${data.current.wind}级\n\n`;
  
  if (params.days > 1) {
    output += `未来${params.days}天预报:\n`;
    for (const day of data.forecast) {
      const dateStr = day.date.toLocaleDateString('zh-CN');
      output += `${dateStr}: ${day.temp_min}-${day.temp_max}${unitSymbol} ${day.condition}\n`;
    }
  }
  
  return {
    content: [{ type: 'text', text: output }],
    data: data // 结构化数据供后续使用
  };
}

// ============ MCP 服务器启动代码 ============
if (import.meta.url === `file://${process.argv[1]}`) {
  // 作为独立进程运行
  const { McpServer } = await import('@modelcontextprotocol/sdk/server/mcp.js');
  const { StdioServerTransport } = await import('@modelcontextprotocol/sdk/server/stdio.js');
  
  const server = new McpServer({
    name: metadata.name,
    version: metadata.version
  });
  
  server.tool(
    'get_weather',
    metadata.description,
    parameters,
    execute
  );
  
  const transport = new StdioServerTransport();
  await server.connect(transport);
  
  console.error(`✅ Weather skill running on stdio`);
}
```

## 3. 快速测试

### 3.1 本地测试脚本

```javascript
// test-weather.js
import { execute } from './weather-skill.js';

async function test() {
  console.log('🧪 Testing weather skill...\n');
  
  // 测试用例 1：基本查询
  const result1 = await execute({ location: '北京' });
  console.log('Test 1 - Basic query:');
  console.log(result1.content[0].text);
  console.log();
  
  // 测试用例 2：多日预报
  const result2 = await execute({ 
    location: '上海',
    days: 3 
  });
  console.log('Test 2 - 3-day forecast:');
  console.log(result2.content[0].text);
  console.log();
  
  // 测试用例 3：错误处理
  try {
    await execute({}); // 缺少必需参数
  } catch (error) {
    console.log('Test 3 - Error handling:');
    console.log('✅ Correctly caught error:', error.message);
  }
}

test();
```

### 3.2 使用 npx 直接运行

```bash
# 无需安装，直接运行
npx weather-skill

# 或者全局安装
npm install -g weather-skill
weather-skill
```

## 4. 发布到 NPM

### 4.1 package.json 配置

```json
{
  "name": "@your-org/weather-skill",
  "version": "1.0.0",
  "description": "Weather forecast skill for AI assistants",
  "type": "module",
  "main": "weather-skill.js",
  "bin": {
    "weather-skill": "./weather-skill.js"
  },
  "scripts": {
    "start": "node weather-skill.js",
    "test": "node test-weather.js"
  },
  "keywords": [
    "mcp",
    "skill",
    "weather",
    "ai-assistant"
  ],
  "author": "Your Name",
  "license": "MIT",
  "dependencies": {
    "@modelcontextprotocol/sdk": "^0.1.0"
  },
  "engines": {
    "node": ">=18.0.0"
  }
}
```

### 4.2 发布流程

```bash
# 1. 登录 NPM
npm login

# 2. 测试包
npm pack --dry-run

# 3. 发布
npm publish --access public

# 4. 验证发布
npm view @your-org/weather-skill
```

---

**下一节：** [c7-5 接口设计](./c7-5-interface-design.md)  
**上一节：** [c7-3 热插拔机制](./c7-3-hot-swappable.md)
