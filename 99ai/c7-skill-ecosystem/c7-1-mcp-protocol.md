# c7-1 MCP 协议详解

## 1. 概述

Model Context Protocol (MCP) 是一种基于 JSON-RPC 的扩展协议，用于标准化 AI智能体与外部工具/服务的交互。本章详解 MCP 协议规范、SDK 使用和实战应用。

## 2. 协议基础

### 2.1 JSON-RPC 扩展

```javascript
// MCP 请求格式
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "method": "tools/call",
  "params": {
    "name": "get_weather",
    "arguments": {
      "location": "北京"
    }
  }
}

// MCP 响应格式
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "北京今天晴朗，温度 25°C"
      }
    ]
  }
}

// 错误响应
{
  "jsonrpc": "2.0",
  "id": "req_123",
  "error": {
    "code": -32602,
    "message": "Invalid params: location is required"
  }
}
```

### 2.2 核心方法

```javascript
class MCPProtocol {
  // 支持的方法列表
  static methods = {
    // 工具相关
    'tools/list': '获取可用工具列表',
    'tools/call': '调用指定工具',
    
    // 资源相关
    'resources/list': '获取可用资源列表',
    'resources/read': '读取资源内容',
    
    // 提示相关
    'prompts/list': '获取提示模板列表',
    'prompts/get': '获取特定提示模板'
  };
  
  // 标准错误码
  static errorCodes = {
    PARSE_ERROR: -32700,
    INVALID_REQUEST: -32600,
    METHOD_NOT_FOUND: -32601,
    INVALID_PARAMS: -32602,
    INTERNAL_ERROR: -32603,
    TOOL_NOT_FOUND: -32001,
    RESOURCE_NOT_FOUND: -32002
  };
}
```

## 3. TypeScript SDK 实现

### 3.1 MCP Server 基础

```typescript
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { z } from 'zod';

// 创建 MCP 服务器
const server = new McpServer({
  name: 'weather-server',
  version: '1.0.0'
});

// 注册工具
server.tool(
  'get_weather',
  '获取指定城市的天气信息',
  {
    location: z.string().describe('城市名称，如"北京"'),
    unit: z.enum(['celsius', 'fahrenheit']).optional().default('celsius')
  },
  async ({ location, unit }) => {
    // 实际天气 API 调用
    const weather = await fetchWeather(location);
    
    return {
      content: [
        {
          type: 'text',
          text: `${location}今天${weather.condition}，温度${weather.temp}°${unit === 'celsius' ? 'C' : 'F'}`
        }
      ]
    };
  }
);

// 启动服务器
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error('MCP Server running on stdio');
}

main();
```

### 3.2 多工具管理

```typescript
class MultiToolMCPServer {
  private server: McpServer;
  private tools: Map<string, ToolHandler>;
  
  constructor(name: string, version: string) {
    this.server = new McpServer({ name, version });
    this.tools = new Map();
    this.setupHandlers();
  }
  
  registerTool(name: string, description: string, schema: any, handler: ToolHandler) {
    this.tools.set(name, handler);
    
    this.server.tool(name, description, schema, async (args) => {
      try {
        const result = await handler(args);
        return {
          content: [{ type: 'text', text: JSON.stringify(result, null, 2) }]
        };
      } catch (error) {
        return {
          content: [{ 
            type: 'text', 
            text: `Error executing ${name}: ${error.message}` 
          }],
          isError: true
        };
      }
    });
  }
  
  private setupHandlers() {
    // 自定义 tools/list 处理
    this.server.server.registerMethod('tools/list', async () => {
      return {
        tools: Array.from(this.tools.entries()).map(([name, handler]) => ({
          name,
          description: handler.description,
          inputSchema: handler.schema
        }))
      };
    });
  }
  
  async start() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error(`MCP Server "${this.server.name}" started`);
  }
}

// 使用示例
const multiServer = new MultiToolMCPServer('my-tools', '1.0.0');

multiServer.registerTool(
  'calculate_bmi',
  '计算身体质量指数',
  {
    weight: z.number().describe('体重 (kg)'),
    height: z.number().describe('身高 (cm)')
  },
  async ({ weight, height }) => {
    const bmi = weight / Math.pow(height / 100, 2);
    return {
      bmi: bmi.toFixed(2),
      category: this.getBmiCategory(bmi)
    };
  }
);

multiServer.start();
```

## 4. 实战案例

### 4.1 文件操作 MCP 服务

```typescript
import fs from 'fs/promises';
import path from 'path';

class FileSystemMCP {
  private baseDir: string;
  
  constructor(baseDir: string) {
    this.baseDir = path.resolve(baseDir);
  }
  
  async createServer() {
    const server = new McpServer({
      name: 'filesystem-server',
      version: '1.0.0'
    });
    
    // 读取文件
    server.tool(
      'read_file',
      '读取文件内容',
      {
        filePath: z.string().describe('文件路径（相对于 baseDir）')
      },
      async ({ filePath }) => {
        const safePath = this.validatePath(filePath);
        const content = await fs.readFile(safePath, 'utf-8');
        
        return {
          content: [{ type: 'text', text: content }]
        };
      }
    );
    
    // 写入文件
    server.tool(
      'write_file',
      '写入文件内容',
      {
        filePath: z.string(),
        content: z.string()
      },
      async ({ filePath, content }) => {
        const safePath = this.validatePath(filePath);
        await fs.mkdir(path.dirname(safePath), { recursive: true });
        await fs.writeFile(safePath, content);
        
        return {
          content: [{ type: 'text', text: `Successfully wrote to ${filePath}` }]
        };
      }
    );
    
    // 列出目录
    server.tool(
      'list_directory',
      '列出目录内容',
      {
        dirPath: z.string()
      },
      async ({ dirPath }) => {
        const safePath = this.validatePath(dirPath);
        const entries = await fs.readdir(safePath, { withFileTypes: true });
        
        const fileList = entries.map(entry => ({
          name: entry.name,
          type: entry.isDirectory() ? 'directory' : 'file'
        }));
        
        return {
          content: [{ type: 'text', text: JSON.stringify(fileList, null, 2) }]
        };
      }
    );
    
    return server;
  }
  
  private validatePath(filePath: string): string {
    const resolved = path.resolve(this.baseDir, filePath);
    
    if (!resolved.startsWith(this.baseDir)) {
      throw new Error('Access denied: Path outside base directory');
    }
    
    return resolved;
  }
}

// 启动服务
const fsServer = new FileSystemMCP('./workspace');
fsServer.createServer().then(async (server) => {
  const transport = new StdioServerTransport();
  await server.connect(transport);
});
```

### 4.2 Web 搜索 MCP 服务

```typescript
class WebSearchMCP {
  async createServer() {
    const server = new McpServer({
      name: 'web-search',
      version: '1.0.0'
    });
    
    // Google 搜索
    server.tool(
      'google_search',
      '执行 Google 搜索',
      {
        query: z.string().describe('搜索关键词'),
        numResults: z.number().min(1).max(10).default(5)
      },
      async ({ query, numResults }) => {
        const results = await this.googleSearch(query, numResults);
        
        return {
          content: [{
            type: 'text',
            text: results.map(r => 
              `Title: ${r.title}\nURL: ${r.url}\nSnippet: ${r.snippet}`
            ).join('\n\n')
          }]
        };
      }
    );
    
    // 网页抓取
    server.tool(
      'scrape_url',
      '抓取网页内容',
      {
        url: z.string().url(),
        selector: z.string().optional()
      },
      async ({ url, selector }) => {
        const content = await this.scrapeUrl(url, selector);
        
        return {
          content: [{ type: 'text', text: content }]
        };
      }
    );
    
    return server;
  }
  
  private async googleSearch(query: string, numResults: number) {
    // 集成 Search API
    const response = await fetch(
      `https://serpapi.com/search.json?q=${encodeURIComponent(query)}&num=${numResults}`,
      { headers: { Authorization: `Bearer ${process.env.SERPAPI_KEY}` } }
    );
    
    const data = await response.json();
    return data.organic_results || [];
  }
  
  private async scrapeUrl(url: string, selector?: string) {
    const puppeteer = require('puppeteer');
    const browser = await puppeteer.launch({ headless: true });
    const page = await browser.newPage();
    
    await page.goto(url, { waitUntil: 'networkidle2' });
    
    const content = selector 
      ? await page.$eval(selector, el => el.textContent)
      : await page.content();
    
    await browser.close();
    return content;
  }
}
```

## 5. Python SDK 实现

```python
from mcp.server import Server
from mcp.server.stdio import stdio_server
from pydantic import BaseModel, Field

# 创建服务器
server = Server("python-weather-server")

# 定义参数模型
class WeatherParams(BaseModel):
    location: str = Field(..., description="城市名称")
    unit: str = Field(default="celsius", description="温度单位")

@server.call_tool()
async def get_weather(name: str, arguments: dict) -> list:
    """获取天气信息"""
    params = WeatherParams(**arguments)
    
    # 模拟天气数据
    weather_data = {
        "北京": {"temp": 25, "condition": "晴朗"},
        "上海": {"temp": 28, "condition": "多云"}
    }
    
    city_data = weather_data.get(params.location, {"temp": 20, "condition": "未知"})
    
    return [{
        "type": "text",
        "text": f"{params.location}今天{city_data['condition']}，温度{city_data['temp']}°C"
    }]

async def main():
    async with stdio_server() as streams:
        await server.run(
            streams[0],
            streams[1],
            server.create_initialization_options()
        )

if __name__ == "__main__":
    import asyncio
    asyncio.run(main())
```

## 6. 部署与运维

### 6.1 Docker 容器化

```dockerfile
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .

EXPOSE 3000

CMD ["node", "dist/mcp-server.js"]
```

```yaml
# docker-compose.yml
version: '3.8'

services:
  mcp-server:
    build: .
    environment:
      - NODE_ENV=production
      - API_KEY=${API_KEY}
    volumes:
      - ./workspace:/app/workspace
    networks:
      - mcp-network
    
  mcp-gateway:
    image: mcp/gateway:latest
    ports:
      - "3000:80"
    depends_on:
      - mcp-server
    networks:
      - mcp-network
```

### 6.2 健康检查

```typescript
class MCPMonitor {
  private healthCheckInterval: NodeJS.Timeout;
  
  constructor(private serverUrl: string) {}
  
  startHealthCheck(intervalMs = 30000) {
    this.healthCheckInterval = setInterval(async () => {
      try {
        const response = await fetch(`${this.serverUrl}/health`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            jsonrpc: '2.0',
            method: 'ping',
            id: 'health_check'
          })
        });
        
        const result = await response.json();
        console.log('MCP Server health:', result.result?.status);
        
      } catch (error) {
        console.error('MCP Server health check failed:', error);
      }
    }, intervalMs);
  }
  
  stopHealthCheck() {
    clearInterval(this.healthCheckInterval);
  }
}
```

---

**下一节：** [c7-2 技能注册与发现](./c7-2-skill-registry.md)  
**上一节：** [c7 技能生态系统模块](./README.md)
