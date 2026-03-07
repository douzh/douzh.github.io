# MCP 开发指南

本文档面向 MCP 开发者，介绍如何查询工具列表、编写代码集成 MCP 服务。

---

## 一、获取工具列表

### 核心原理：通过 `tools/list` 方法

MCP 协议定义了标准的工具发现机制，客户端可以通过 **JSON-RPC** 的 `tools/list` 方法查询服务器提供的所有工具。

#### 请求格式
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list"
}
```

#### 响应格式
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {
        "name": "mcp_everything_get-sum",
        "description": "Returns the sum of two numbers",
        "inputSchema": {
          "type": "object",
          "properties": {
            "a": {"type": "number"},
            "b": {"type": "number"}
          },
          "required": ["a", "b"]
        }
      }
    ]
  }
}
```

---

## 二、命令行获取工具列表

### 方式 1: 使用 curl（HTTP 模式）

```powershell
# 1. 先启动 MCP Server（HTTP 模式）
npx @modelcontextprotocol/server-everything streamableHttp --port 3000

# 2. 在另一个终端执行
curl http://localhost:3000/mcp -Method POST -ContentType "application/json" -Body '{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list"
}'
```

### 方式 2: 使用 PowerShell

```powershell
$response = Invoke-WebRequest -Uri "http://localhost:3000/mcp" `
  -Method POST `
  -ContentType "application/json" `
  -Body '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/list"
  }'

$response.Content | ConvertFrom-Json | ConvertTo-Json -Depth 10
```

### 方式 3: 使用专用工具 mcp-cli

```bash
# 安装
pip install mcp-tool

# 配置 ~/.mcp.json 或 ./.claude/mcp.json
{
  "mcpServers": {
    "everything": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-everything"]
    }
  }
}

# 查看所有可用工具
mcp-tool list

# 查看特定服务器的工具
mcp-tool everything list

# 查看工具参数
mcp-tool everything get-sum --help
```

---

## 三、在代码中获取

### Python 示例

```python
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

async def list_tools():
    async with stdio_client(
        command="npx",
        args=["-y", "@modelcontextprotocol/server-everything"]
    ) as (read, write):
        async with ClientSession(read, write) as session:
            # 初始化
            await session.initialize()
            
            # 获取工具列表
            tools = await session.list_tools()
            
            for tool in tools:
                print(f"工具名：{tool.name}")
                print(f"描述：{tool.description}")
                print(f"参数：{tool.inputSchema}")
                print("---")

asyncio.run(list_tools())
```

### Node.js 示例

```javascript
const { Client } = require('@modelcontextprotocol/sdk');

async function getTools() {
  const client = new Client({
    name: 'example-client',
    version: '1.0.0'
  });

  // 连接到服务器
  await client.connect({
    command: 'npx',
    args: ['-y', '@modelcontextprotocol/server-everything']
  });

  // 获取工具列表
  const tools = await client.listTools();
  
  console.log(JSON.stringify(tools, null, 2));
}

getTools();
```

---

## 四、快速测试脚本（PowerShell）

创建 `get-mcp-tools.ps1`：

```powershell
# 启动服务器
$serverProcess = Start-Process npx -ArgumentList "@modelcontextprotocol/server-everything", "streamableHttp", "--port", "3000" -PassThru -NoNewWindow

Start-Sleep -Seconds 3  # 等待服务器启动

try {
    $response = Invoke-WebRequest -Uri "http://localhost:3000/mcp" `
        -Method POST `
        -ContentType "application/json" `
        -Body '{
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/list"
        }'
    
    $tools = $response.Content | ConvertFrom-Json
    $tools.result.tools | ForEach-Object {
        Write-Host "工具：$($_.name)"
        Write-Host "描述：$($_.description)"
        Write-Host "---"
    }
}
finally {
    Stop-Process -Id $serverProcess.Id -Force
}
```

---

## 五、调用原理

### 执行流程
```
AI 助手 → IDE 内置 MCP Client → JSON-RPC 请求 → MCP Server (Node.js 进程) → 执行工具 → 返回结果
```

### JSON-RPC 请求格式
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "mcp_everything_get-sum",
    "arguments": {
      "a": 15,
      "b": 27
    }
  }
}
```

### 响应格式
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "The sum of 15 and 27 is 42."
      }
    ]
  }
}
```

---

## 六、Claude Desktop 配置

配置文件位置：
- **Windows**: `C:\Users\你的用户名\AppData\Roaming\Claude\claude_desktop_config.json`
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "everything": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-everything"]
    }
  }
}
```

---

## 九、多语言 MCP 开发

MCP 不只是 Node.js/TypeScript！官方已支持 **10 种编程语言**的 SDK。

### 9.1 支持的语言列表

| 语言 | SDK 状态 | 特点 | 适用场景 |
|------|---------|------|----------|
| **TypeScript/JavaScript** | ✅ 官方 | 最早支持，生态最丰富 | Web 开发、全栈项目 |
| **Python** | ✅ 官方 | AI/ML 生态，语法简洁 | 数据分析、AI 应用 |
| **Go** | ✅ 官方 | 高性能，编译为二进制 | 后端服务、高并发场景 |
| **Java** | ✅ 官方 | 企业级，类型安全 | 企业应用、大型系统 |
| **Kotlin** | ✅ 官方 | 与 Java 互操作 | Android、现代 Java 项目 |
| **C#** | ✅ 官方 | .NET 生态 | Windows 应用、企业系统 |
| **Rust** | ✅ 官方 | 高性能、内存安全 | 系统编程、高性能组件 |
| **Ruby** | ✅ 官方 | 简洁优雅 | Web 开发、脚本工具 |
| **PHP** | ✅ 官方 | Web 开发 | 网站后端、CMS |
| **Swift** | ✅ 官方 | Apple 生态 | iOS/macOS 应用 |

### 9.2 为什么官方示例多用 Node.js/Python？

**原因分析：**

1. **AI 开发者偏好**
   - Python 是 AI/ML 领域的主流语言
   - JavaScript/TypeScript 在前端和工具链中占主导

2. **快速原型开发**
   - Python/JS 适合快速开发和演示
   - 脚本语言无需编译，调试方便

3. **历史原因**
   - MCP 最初由 AI 社区推动
   - 早期采用者多为 Python/JS 开发者

### 9.3 各语言快速开始

#### **Python MCP Server**

```python
# 安装
pip install mcp

# 创建简单的数学服务器
from mcp.server.fastmcp import FastMCP

mcp = FastMCP("Math")

@mcp.tool()
def add(a: int, b: int) -> int:
    """两个数相加"""
    return a + b

if __name__ == "__main__":
    mcp.run()
```

**启动方式：**
```bash
python math_server.py
# 或使用 uv
uv run math_server.py
```

#### **Go MCP Server**

```go
// 安装
// 注意：Go SDK 由社区维护
// github.com/mark3labs/mcp-go

package main

import (
    "github.com/mark3labs/mcp-go/server"
)

func main() {
    s := server.NewMCPServer(
        "MyServer",
        "1.0.0",
    )
    
    s.AddTool(server.Tool{
        Name: "calculate",
        Description: "执行计算",
        Handler: calculateHandler,
    })
    
    s.ServeStdio()
}
```

**启动方式：**
```bash
go get github.com/mark3labs/mcp-go
go run main.go
# 或编译为二进制
go build -o mcp-server
./mcp-server
```

#### **Java MCP Server**

```java
// Maven 依赖
<dependency>
    <groupId>io.modelcontextprotocol</groupId>
    <artifactId>mcp</artifactId>
    <version>0.7.0</version>
</dependency>

// 实现示例
public class MyMCPServer {
    public static void main(String[] args) {
        MCPServer server = new MCPServer("MyServer");
        
        server.addTool("calculate", 
            "执行计算",
            (arguments) -> {
                // 实现逻辑
                return result;
            });
        
        server.start();
    }
}
```

**启动方式：**
```bash
# 编译
mvn clean package
# 运行
java -jar target/my-mcp-server.jar
```

#### **Rust MCP Server**

```rust
// Cargo.toml
[dependencies]
mcp-server = "0.1"
tokio = { version = "1", features = ["full"] }

// 实现示例
use mcp_server::{Server, Tool};

#[tokio::main]
async fn main() {
    let mut server = Server::new("MyServer");
    
    server.add_tool(Tool::new(
        "calculate",
        "执行计算",
        calculate_handler,
    ));
    
    server.serve().await.unwrap();
}
```

**启动方式：**
```bash
cargo build --release
./target/release/mcp-server
```

### 9.4 多语言对比

| 特性 | Node.js | Python | Go | Java | Rust |
|------|---------|--------|-----|------|------|
| **启动速度** | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **运行性能** | ⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **开发效率** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |
| **部署便利** | ⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **生态丰富** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **类型安全** | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |

### 9.5 选型建议

**选择 Python，如果你：**
- 做 AI/ML 相关项目
- 需要快速原型开发
- 团队熟悉 Python

**选择 Go，如果你：**
- 需要高性能和高并发
- 希望简单部署（单个二进制文件）
- 构建后端服务

**选择 Java/Kotlin，如果你：**
- 企业级应用
- 已有 Java 技术栈
- 需要强类型和稳定性

**选择 Rust，如果你：**
- 追求极致性能
- 关注内存安全
- 系统级编程需求

**选择 Node.js/TypeScript，如果你：**
- Web 开发背景
- 需要丰富的 npm 生态
- 全栈 JavaScript 项目

---

## 十、高级主题

### 1. 自定义 MCP Server

创建自己的 MCP Server 需要：
1. 实现 MCP 协议规范
2. 提供 `tools/list` 接口
3. 实现 `tools/call` 接口
4. 支持 stdio 或 HTTP 传输

### 2. 工具发现机制

MCP 支持三种发现机制：
- **工具级发现**: 通过 `list_tools()` 方法查询
- **服务级发现**: 通过 URI 解析（如 `mcp://api.service.com`）
- **注册表发现**: MCP Registry 管理工具元信息

### 3. 安全考虑

- 命令和参数验证
- 工作目录限制
- 环境变量隔离
- 访问权限控制

---

## 十一、调试技巧

### 1. 启用日志

```bash
# 设置日志级别
export MCP_LOG_LEVEL=debug
npx @modelcontextprotocol/server-everything
```

### 2. 使用 Inspector 工具

```bash
# 安装 MCP Inspector
npx @modelcontextprotocol/inspector

# 通过 Web 界面调试
```

### 3. 常见问题排查

**问题：工具列表为空**
- 检查 Server 是否正确启动
- 确认连接已建立（需要先 initialize）
- 验证 Server 是否提供工具

**问题：连接失败**
- 检查端口是否被占用
- 确认防火墙设置
- 验证命令和参数是否正确

---

## 十二、参考资源

### 官方文档
- [MCP Specification](https://modelcontextprotocol.io/specification)
- [MCP SDK](https://github.com/modelcontextprotocol/sdk)
- [MCP Servers](https://github.com/modelcontextprotocol/servers)
- [MCP 多语言 SDK 文档](https://modelcontextprotocol.io/docs/sdk)

### 社区工具
- [mcp-cli](https://github.com/philschmid/mcp-cli) - 命令行工具
- [MCP Inspector](https://github.com/modelcontextprotocol/inspector) - 调试工具
- [mcp-go](https://github.com/mark3labs/mcp-go) - Go 语言实现（社区维护）
- [Awesome-MCP-ZH](https://gitcode.com/gh_mirrors/aw/Awesome-MCP-ZH) - 中文社区精选

### 相关文档
- [MCP 使用指南](./mcp-user-guide.md) - 用户使用文档
- [MCP 执行结果](./mcp-execution-results.md) - 实际执行示例

---

**文档生成时间:** 2026-03-06  
**MCP Server 版本:** latest  
**执行环境:** Windows 25H2
