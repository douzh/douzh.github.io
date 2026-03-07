# MCP 使用指南

## 一、MCP 简介

**MCP (Model Context Protocol)** 是一种开放标准，允许 AI 模型通过统一的界面使用外部工具和服务。

### 核心价值
- 把"模型能力"接到真实系统里
- 提供标准化的工具调用接口
- 支持多种传输协议（stdio, SSE, HTTP）

---

## 二、快速开始

### 前置要求
- Node.js 18+
- npm 或 npx

### 安装方式

#### 方式 A：临时使用（推荐测试）
```bash
npx @modelcontextprotocol/server-everything
```

#### 方式 B：全局安装（推荐常用）
```bash
npm install -g @modelcontextprotocol/server-everything@latest
```

---

## 三、配置 MCP 服务

### 在 IDE 中添加 MCP 服务

![添加 MCP 服务界面](../images/add-mcp-service.png)

#### 填写说明

**名称**  
给这个 MCP 服务起个名字，比如：
- `everything`
- `filesystem`
- `github`

**类型**  
选择 **STDIO**（默认推荐），这是最常用的模式。

**命令** ⭐  
这里填写启动 MCP Server 的命令：
```
npx
```

**参数**  
这里填写命令的执行参数（空格隔开）：
```
-y @modelcontextprotocol/server-everything
```

### 完整配置示例

#### 示例 1: 使用 everything MCP

- **名称:** `everything`
- **类型:** `STDIO` ✓
- **命令:** `npx`
- **参数:** `-y @modelcontextprotocol/server-everything`

#### 示例 2: 使用文件系统 MCP

- **名称:** `filesystem`
- **类型:** `STDIO`
- **命令:** `npx`
- **参数:** `-y @modelcontextprotocol/server-filesystem /Users/yourname/Desktop`

#### 示例 3: 使用 GitHub MCP

- **名称:** `github`
- **类型:** `STDIO`
- **命令:** `npx`
- **参数:** `-y @modelcontextprotocol/server-github`

---

## 四、命令解析

```
npx -y @modelcontextprotocol/server-everything
│   │  └─────────────────────────────────┘
│   │              MCP Server 包名
│   └──────────── 自动确认（无需手动确认）
└──────────────── 从 npm registry 运行包
```

### ⚠️ 注意事项

1. **命令和参数要分开填**
   - ❌ 错误：命令填 `npx -y @modelcontextprotocol/server-everything`
   - ✅ 正确：命令填 `npx`，参数填 `-y @modelcontextprotocol/server-everything`

2. **确保已安装 Node.js**
   ```bash
   node --version
   ```

3. **首次运行会自动下载**
   第一次使用时，npx 会自动下载 MCP Server 包，可能需要几秒钟。

---

## 五、可用工具列表

| 工具名称 | 功能描述 | 参数 |
|---------|---------|------|
| mcp_everything_get-sum | 计算两数之和 | a (number), b (number) |
| mcp_everything_get-env | 获取环境变量 | random_string (string) |
| mcp_everything_echo | 回显消息 | message (string) |
| mcp_everything_get-tiny-image | 获取小图片 | random_string (string) |
| mcp_everything_trigger-long-running-operation | 模拟长时操作 | duration (number), steps (number) |
| mcp_everything_simulate-research-query | 模拟研究查询 | topic (string) |
| mcp_everything_get-structured-content | 获取结构化内容 | location (enum) |
| mcp_everything_toggle-simulated-logging | 切换日志记录 | random_string (string) |
| mcp_everything_toggle-subscriber-updates | 切换订阅更新 | random_string (string) |
| mcp_everything_gzip-file-as-resource | 压缩文件 | data (uri), name (string), outputType (enum) |

---

## 六、使用示例

### 示例 1: 数学计算
**调用:**
```
mcp_everything_get-sum(a=15, b=27)
```

**返回:**
```
The sum of 15 and 27 is 42.
```

### 示例 2: 获取环境变量
**调用:**
```
mcp_everything_get-env(random_string="test123")
```

**返回:**
```json
{
  "ALLUSERSPROFILE": "C:\\ProgramData",
  "APPDATA": "C:\\Users\\douzh\\AppData\\Roaming",
  "COMPUTERNAME": "DOUZH-THINKPAD",
  "HOME": "D:\\mycloud",
  "JAVA_HOME": "C:\\Program Files\\Java\\jdk1.8.0_202",
  "NODE": "C:\\nodejs\\node.exe",
  "NVM_HOME": "C:\\nvm",
  "OS": "Windows_NT",
  "USERNAME": "douzh"
}
```

---

## 七、启动模式

### 1. stdio 模式（默认）
```bash
npx @modelcontextprotocol/server-everything stdio
```
- 通过标准输入输出通信
- Claude Desktop、VSCode 等使用此模式

### 2. SSE 模式
```bash
npx @modelcontextprotocol/server-everything sse --port 3000
```
- HTTP + SSE 传输
- 可通过网络访问

### 3. Streamable HTTP 模式
```bash
npx @modelcontextprotocol/server-everything streamableHttp --port 3000
```
- 支持流式响应的 HTTP

---

## 八、命令行调用示例

### PowerShell 调用（HTTP 模式）
```powershell
curl http://localhost:3000/mcp -Method POST -ContentType "application/json" -Body '{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "mcp_everything_get-sum",
    "arguments": {
      "a": 100,
      "b": 256
    }
  }
}'
```

---

## 九、实际应用场景

### 1. AI 辅助开发
- AI 通过 MCP 工具获取系统信息
- 执行文件操作
- 调用外部 API

### 2. 测试和调试
- 启动服务器进行测试
- 用浏览器或 Postman 测试接口

### 3. 自定义集成
- 将 MCP 集成到自己的应用中
- 让 AI 可以调用自定义服务

---

## 十、快速开始命令总结

```bash
# 1. 检查 Node.js
node --version

# 2. 直接运行（无需安装）
npx @modelcontextprotocol/server-everything

# 3. 全局安装后使用
npm install -g @modelcontextprotocol/server-everything
npx @modelcontextprotocol/server-everything stdio

# 4. 查看帮助
npx @modelcontextprotocol/server-everything --help
```

---

## 十一、常见问题

### Q1: npx 命令执行失败
**解决:** 检查网络连接，确保能够访问 npm registry

### Q2: 配置解析错误
**解决:** 验证 JSON 格式，确保引号和括号匹配

### Q3: 如何在命令行直接使用？
**答:** MCP 使用进程间通信（stdio），通常通过 MCP client（如 IDE）调用。如需手动测试，建议启动 HTTP/SSE 模式后使用 curl 或 Postman。

### Q4: 如何查看某个 MCP Server 提供哪些工具？
**答:** 有三种方式：
1. 使用 `tools/list` JSON-RPC 方法查询（最标准）
2. 使用 mcp-cli 等专用工具
3. 查看该 MCP Server 的官方文档

### Q5: 为什么我的工具列表是空的？
**答:** 可能的原因：
1. MCP Server 未正确启动
2. 连接尚未建立（需要先 initialize）
3. 该 Server 本身不提供任何工具（可能只提供 resources 或 prompts）

---

**文档生成时间:** 2026-03-06  
**MCP Server 版本:** latest  
**执行环境:** Windows 25H2

**相关文档:**
- [MCP 开发指南](./mcp-development-guide.md) - 开发者专用文档
- [MCP 执行结果](./mcp-execution-results.md) - 实际执行示例
