# Lingma 开发扩展指南

本文档面向开发者，介绍如何扩展和集成 Lingma 智能体。

---

## 一、架构概览

### 1.1 Lingma 的能力层级

Lingma 的能力可以分为三个层级，从纯数字到物理世界：

```
层级 1: 纯数字世界（原生支持）
- 语言理解、代码生成、文档创作
- 知识问答、逻辑推理、数学计算
- 代码分析、Bug 检测、优化建议

层级 2: 操作系统（IDE 支持）
- 文件操作、命令执行、进程管理
- 项目结构分析、代码导航
- 终端命令、脚本执行

层级 3: 物理世界（通过 MCP）
- 硬件控制、IoT 设备、工业系统
- 打印机、传感器、音视频设备
- 智能家居、机器人、自动化系统
```

**关键说明：**
- **层级 1** 是 AI 的原生能力，无需外部支持
- **层级 2** 依赖 IDE 或平台提供的工具
- **层级 3** 通过 MCP 协议连接物理设备

### 1.2 整体架构

```
┌──────────────────────────────────────────┐
│            用户交互层                     │
│   (对话界面、IDE 集成、API 接口)            │
└──────────────────────────────────────────┘
                    ↓
┌──────────────────────────────────────────┐
│         Lingma 核心引擎                    │
├──────────────────────────────────────────┤
│  意图识别  │  任务规划  │  工具匹配      │
│  上下文管理 │  决策引擎  │  结果生成      │
└──────────────────────────────────────────┘
                    ↓
┌──────────────────────────────────────────┐
│          MCP 客户端层                      │
├──────────────────────────────────────────┤
│  工具发现  │  参数提取  │  调用路由      │
│  错误处理  │  重试机制  │  结果聚合      │
└──────────────────────────────────────────┘
                    ↓
┌──────────────────────────────────────────┐
│          MCP 服务层                        │
├──────────────────────────────────────────┤
│ everything │ filesystem │ github │ ...   │
└──────────────────────────────────────────┘
```

### 1.2 工作流程

```
用户请求
  ↓
1. 意图理解（语义分析、关键词提取）
  ↓
2. 任务识别（判断是否需要工具）
  ↓
3. 工具匹配（查询可用工具、语义相似度）
  ↓
4. 参数提取（从自然语言提取）
  ↓
5. 工具调用（发送请求、等待响应）
  ↓
6. 结果生成（解析返回、格式化输出）
```

---

## 二、工具匹配机制

### 2.1 匹配算法

```python
# Lingma 的工具匹配逻辑

def match_tool(user_query, available_tools):
    """
    根据用户查询匹配工具
    """
    # 步骤 1: 意图识别
    intent = analyze_intent(user_query)
    # 示例："计算 15 加 27" → intent = "math.addition"
    
    # 步骤 2: 工具描述匹配
    candidates = []
    for tool in available_tools:
        similarity = semantic_similarity(
            user_query,
            f"{tool.name}: {tool.description}"
        )
        if similarity > 0.6:  # 阈值
            candidates.append((tool, similarity))
    
    # 步骤 3: 参数可行性验证
    valid_candidates = []
    for tool, score in candidates:
        params = extract_params(user_query, tool.inputSchema)
        if validate_params(params, tool.inputSchema):
            valid_candidates.append((tool, score))
    
    # 步骤 4: 选择最佳工具
    if valid_candidates:
        best_tool = max(valid_candidates, key=lambda x: x[1])
        return best_tool[0]
    
    return None
```

### 2.2 实际案例分析

#### **案例 1：数学计算**

```
用户："帮我计算 15 加 27 的和"

匹配过程：
1. 关键词提取：计算、15、27、和
2. 意图识别：数学加法运算
3. 扫描工具池：
   - mcp_everything_get-sum
     描述："Returns the sum of two numbers"
     相似度：0.92 ✓
   - mcp_everything_get-env
     描述："Returns all environment variables"
     相似度：0.15 ✗
4. 参数提取：a=15, b=27
5. 验证通过 → 选择 mcp_everything_get-sum
```

#### **案例 2：文件操作**

```
用户："读取 README.md 的内容"

匹配过程：
1. 关键词：读取、README.md、内容
2. 意图：文件读取操作
3. 扫描工具池：
   - mcp_filesystem_read_file
     描述："Read file content"
     相似度：0.95 ✓
4. 参数提取：path="README.md"
5. 验证通过 → 选择 mcp_filesystem_read_file
```

---

## 三、配置和部署

### 3.1 基础配置

#### **IDE 集成配置**
```json
{
  "lingma": {
    "enabled": true,
    "model": "qwen-max",
    "temperature": 0.7,
    "max_tokens": 4096,
    "context_window": 32768
  }
}
```

#### **MCP 服务配置**
```json
{
  "mcpServers": {
    "everything": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-everything"]
    },
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/workspace"]
    },
    "github": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-github"]
    }
  }
}
```

### 3.2 环境变量

```bash
# LLM API 配置
LINGMA_API_KEY=your-api-key
LINGMA_API_URL=https://api.lingma.ai/v1
LINGMA_MODEL=qwen-max

# MCP 配置
LINGMA_MCP_ENABLED=true
LINGMA_MCP_TIMEOUT=30000
LINGMA_MCP_MAX_RETRIES=2

# 日志配置
LINGMA_LOG_LEVEL=info
LINGMA_LOG_FILE=lingma.log
```

---

## 四、调试和监控

### 4.1 日志查看

```bash
# 查看实时日志
tail -f lingma.log

# 查看错误日志
grep "ERROR" lingma.log | tail -20

# 查看工具调用记录
grep "TOOL_CALL" lingma.log
```

### 4.2 性能监控

```python
# 关键指标
- 响应时间（平均 < 2 秒）
- 工具调用成功率（> 95%）
- 上下文命中率（> 80%）
- 用户满意度评分
```

### 4.3 常见问题排查

#### **问题 1：工具调用失败**

```
症状：Lingma 无法调用 MCP 工具

排查步骤：
1. 检查 MCP Server 是否启动
   ps aux | grep mcp
2. 验证连接配置
   cat config.json | grep mcpServers
3. 查看错误日志
   tail -f lingma.log | grep "MCP"
4. 重启 MCP 服务
   npx @modelcontextprotocol/server-everything
```

#### **问题 2：响应速度慢**

```
症状：Lingma 响应时间超过 5 秒

排查步骤：
1. 检查网络延迟
   ping api.lingma.ai
2. 查看模型负载
   检查 API 限流状态
3. 优化上下文长度
   减少不必要的历史信息
4. 启用缓存
   配置响应缓存策略
```

---

## 五、扩展开发

### 5.1 自定义 MCP 工具

#### **创建自定义工具**

```python
# 创建自定义 MCP 工具
from mcp.server.fastmcp import FastMCP

mcp = FastMCP("MyCustomTools")

@mcp.tool()
def hello(name: str) -> str:
    """打招呼工具"""
    return f"Hello, {name}!"

@mcp.tool()
def calculate_bmi(weight: float, height: float) -> float:
    """计算 BMI 指数"""
    return weight / (height ** 2)

if __name__ == "__main__":
    mcp.run()
```

#### **工具注册**

```json
{
  "mcpServers": {
    "custom": {
      "command": "python",
      "args": ["/path/to/my_mcp_server.py"]
    }
  }
}
```

### 5.2 技能扩展方式

Lingma 支持通过以下方式扩展技能：

1. **MCP 工具** - 连接外部服务
2. **自定义插件** - Python/Node.js 插件
3. **API 集成** - RESTful API 调用
4. **脚本执行** - Shell/Python 脚本

---

## 六、高级集成

### 6.1 Agent 集成 MCP

参考详细文档：[Agent 对接 MCP 指南](./mcp-integration-guide.md)

#### **快速示例**

```python
from mcp import ClientSession
from mcp.client.stdio import stdio_client

class AIAgent:
    async def initialize(self):
        # 创建 stdio 连接
        async with stdio_client(
            command="npx",
            args=["-y", "@modelcontextprotocol/server-everything"]
        ) as (read, write):
            # 创建会话
            async with ClientSession(read, write) as session:
                # 初始化
                await session.initialize()
                
                # 获取工具列表
                tools = await session.list_tools()
                
                # 存储工具信息供 LLM 使用
                self.tool_definitions = self.format_tools_for_llm(tools)
```

### 6.2 物理设备集成

#### **硬件控制示例**

```python
from mcp.server.fastmcp import FastMCP
import serial  # 串口通信
import RPi.GPIO as GPIO  # 树莓派 GPIO

mcp = FastMCP("PhysicalWorld")

@mcp.tool()
def control_motor(speed: int, direction: str) -> str:
    """控制电机"""
    # 通过 GPIO 控制电机
    GPIO.output(MOTOR_PIN, speed)
    GPIO.output(DIR_PIN, direction == "forward")
    return f"电机已设置：{speed} RPM, {direction}"

@mcp.tool()
def read_temperature(sensor_pin: int) -> float:
    """读取温度传感器"""
    # 读取 DS18B20 温度传感器
    temp = read_ds18b20(sensor_pin)
    return temp
```

#### **安全控制**

```python
@mcp.tool()
def control_device(device: str, action: str):
    # 1. 权限检查
    if not check_permission(user, device):
        return "权限不足"
    
    # 2. 安全检查
    if not is_safe_action(action):
        return "危险操作，已拒绝"
    
    # 3. 用户确认
    if requires_confirmation(action):
        if not confirm_with_user():
            return "操作已取消"
    
    # 4. 执行并记录
    result = execute(device, action)
    log_operation(device, action, result)
    return result
```

---

## 七、性能优化

### 7.1 响应时间优化

#### **缓存策略**
```python
# 工具调用结果缓存
from functools import lru_cache

@lru_cache(maxsize=100)
def cached_tool_call(tool_name: str, params_hash: str):
    """缓存工具调用结果"""
    return call_tool(tool_name, params_hash)
```

#### **并发调用**
```python
# 多个工具并发调用
import asyncio

async def parallel_tool_calls(tools_to_call):
    """并发调用多个工具"""
    tasks = [
        call_tool(tool['name'], tool['params'])
        for tool in tools_to_call
    ]
    results = await asyncio.gather(*tasks)
    return results
```

### 7.2 资源管理

#### **连接池**
```python
# MCP 连接池管理
class MCPConnectionPool:
    def __init__(self, max_connections=10):
        self.pool = asyncio.Queue(maxsize=max_connections)
    
    async def get_connection(self, server_config):
        """获取连接"""
        return await self.pool.get()
    
    async def release_connection(self, connection):
        """释放连接"""
        await self.pool.put(connection)
```

---

## 八、测试

### 8.1 单元测试

```python
import unittest

class TestLingmaTools(unittest.TestCase):
    
    def test_math_calculation(self):
        """测试数学计算工具"""
        result = call_tool("mcp_everything_get-sum", {"a": 15, "b": 27})
        self.assertEqual(result, 42)
    
    def test_file_read(self):
        """测试文件读取工具"""
        result = call_tool("mcp_filesystem_read_file", {"path": "test.txt"})
        self.assertIsInstance(result, str)
    
    def test_tool_matching(self):
        """测试工具匹配算法"""
        tools = get_available_tools()
        matched = match_tool("计算 15 加 27", tools)
        self.assertEqual(matched.name, "mcp_everything_get-sum")

if __name__ == "__main__":
    unittest.main()
```

### 8.2 集成测试

```python
async def test_integration():
    """集成测试：完整流程"""
    # 1. 初始化
    agent = AIAgent()
    await agent.initialize()
    
    # 2. 发送请求
    response = await agent.chat("帮我计算 100 加 200")
    
    # 3. 验证结果
    assert "300" in response
    
    # 4. 清理
    await agent.close()
```

---

## 九、相关资源

### 9.1 官方文档
- [Lingma 官方文档](https://lingma.ai/docs)
- [MCP 规范](https://modelcontextprotocol.io/specification)
- [MCP SDK](https://github.com/modelcontextprotocol/sdk)

### 9.2 本项目文档
- [MCP 使用指南](../mcp/mcp-user-guide.md)
- [MCP 开发指南](../mcp/mcp-development-guide.md)
- [Lingma 用户指南](./lingma-user-guide.md)
- [Agent 对接 MCP 指南](./mcp-integration-guide.md)

### 9.3 社区资源
- [Awesome-MCP-ZH](https://gitcode.com/gh_mirrors/aw/Awesome-MCP-ZH)
- [Lingma 社区论坛](https://community.lingma.ai)
- [GitHub 讨论区](https://github.com/lingma-ai/discussions)

---

**文档版本:** 1.0  
**最后更新:** 2026-03-07  
**维护团队:** One AI Team  
**联系方式:** support@lingma.ai
