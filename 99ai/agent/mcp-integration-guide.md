# Agent 对接和使用 MCP 指南

本文档面向 AI Agent 开发者，详细介绍如何在 Agent 中集成和使用 MCP (Model Context Protocol) 服务。

---

## 一、MCP 与 Agent 的关系

### 1.1 核心概念

```
┌─────────────────┐
│   AI Agent      │  ← 智能决策层（理解意图、规划任务）
├─────────────────┤
│  MCP Client     │  ← 协议适配层（工具发现、调用路由）
├─────────────────┤
│  MCP Server     │  ← 功能提供层（具体工具实现）
├─────────────────┤
│ External Tools  │  ← 实际执行层（文件系统、API、数据库等）
└─────────────────┘
```

**MCP 对 Agent 的价值：**
- ✅ **标准化工具接口** - 统一的工具调用协议
- ✅ **动态工具发现** - 运行时获取可用工具列表
- ✅ **类型安全调用** - 参数验证和错误处理
- ✅ **多服务集成** - 轻松接入多个工具源

---

## 二、Agent 集成 MCP 的完整流程

### 2.1 架构设计

#### **方案 A：内置 MCP Client（推荐）**

```python
class AIAgent:
    def __init__(self):
        # 1. 初始化 LLM
        self.llm = LLM()
        
        # 2. 初始化 MCP Client 管理器
        self.mcp_manager = MCPClientManager()
        
        # 3. 连接 MCP 服务
        self.mcp_manager.connect("everything", 
            command="npx",
            args=["-y", "@modelcontextprotocol/server-everything"]
        )
        
        # 4. 加载工具列表
        self.tools = self.mcp_manager.list_tools()
    
    def execute(self, user_query: str):
        # 5. Agent 理解意图
        intent = self.llm.analyze_intent(user_query)
        
        # 6. 匹配合适的工具
        matched_tool = self.match_tool(intent, self.tools)
        
        # 7. 提取参数
        params = self.extract_params(user_query, matched_tool)
        
        # 8. 通过 MCP Client 调用工具
        result = self.mcp_manager.call_tool(
            name=matched_tool.name,
            arguments=params
        )
        
        # 9. 返回结果给用户
        return self.format_response(result)
```

#### **方案 B：使用 MCP SDK**

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

---

## 三、工具发现和管理

### 3.1 工具发现机制

```python
class ToolRegistry:
    def __init__(self):
        self.tools = {}  # name -> ToolDefinition
        self.categories = {}  # category -> [tool_names]
    
    def register_mcp_server(self, name: str, config: dict):
        """注册 MCP 服务"""
        # 1. 启动连接
        client = MCPClient(config)
        client.connect()
        
        # 2. 获取工具列表
        tools_response = client.list_tools()
        
        # 3. 注册到工具池
        for tool in tools_response.tools:
            self.tools[tool.name] = ToolDefinition(
                name=tool.name,
                description=tool.description,
                input_schema=tool.inputSchema,
                source=name,  # 标记来源
                client=client
            )
            
            # 4. 自动分类（基于名称或描述）
            category = self.auto_categorize(tool)
            if category not in self.categories:
                self.categories[category] = []
            self.categories[category].append(tool.name)
    
    def auto_categorize(self, tool) -> str:
        """自动分类工具"""
        name = tool.name.lower()
        desc = tool.description.lower()
        
        if "file" in name or "read" in name or "write" in name:
            return "filesystem"
        elif "env" in name or "system" in name:
            return "system"
        elif "math" in name or "calculate" in name:
            return "math"
        elif "github" in name or "issue" in name:
            return "development"
        else:
            return "general"
```

### 3.2 工具信息格式化（供 LLM 使用）

```python
def format_tools_for_llm(self, tools) -> str:
    """
    将工具列表格式化为 LLM 可理解的提示词
    """
    prompt = "## 可用工具列表\n\n"
    
    for tool in tools:
        prompt += f"### {tool.name}\n"
        prompt += f"**功能**: {tool.description}\n"
        prompt += f"**参数**:\n"
        
        for param_name, param_schema in tool.inputSchema.get('properties', {}).items():
            required = param_name in tool.inputSchema.get('required', [])
            prompt += f"  - `{param_name}`: {param_schema.get('description', '无描述')}\n"
            prompt += f"    类型：{param_schema.get('type', 'any')}\n"
            if required:
                prompt += f"    **必需参数**\n"
        
        prompt += "\n"
    
    return prompt
```

**示例输出：**

```markdown
## 可用工具列表

### mcp_everything_get-sum
**功能**: Returns the sum of two numbers
**参数**:
  - `a`: First number
    类型：number
    **必需参数**
  - `b`: Second number
    类型：number
    **必需参数**

### mcp_everything_get-env
**功能**: Returns all environment variables
**参数**:
  - `random_string`: Random string for no-parameter mcp tool
    类型：string
```

---

## 四、意图理解和工具匹配

### 4.1 语义匹配算法

```python
class ToolMatcher:
    def __init__(self, llm):
        self.llm = llm
        self.tool_embeddings = {}  # 预计算工具描述向量
    
    def match_tool(self, user_query: str, tools: list) -> Optional[ToolDefinition]:
        """
        根据用户查询匹配最佳工具
        """
        # 方法 1: 使用 LLM 直接判断
        tool_name = self.llm.ask(f"""
根据用户查询，从以下工具中选择最合适的一个：

用户查询：{user_query}

可用工具：
{self.format_tools_brief(tools)}

只返回工具名称，如果没有匹配的工具返回 null。
""")
        
        if tool_name and tool_name != "null":
            return self.get_tool_by_name(tool_name)
        
        # 方法 2: 语义相似度（备用）
        query_embedding = self.embed(user_query)
        best_match = None
        best_score = 0.0
        
        for tool in tools:
            tool_embedding = self.tool_embeddings.get(tool.name)
            if tool_embedding:
                score = cosine_similarity(query_embedding, tool_embedding)
                if score > best_score and score > 0.6:  # 阈值
                    best_score = score
                    best_match = tool
        
        return best_match
    
    def format_tools_brief(self, tools: list) -> str:
        """简洁格式供 LLM 快速浏览"""
        return "\n".join([
            f"- {t.name}: {t.description}" 
            for t in tools
        ])
```

### 4.2 参数提取

```python
class ParameterExtractor:
    def __init__(self, llm):
        self.llm = llm
    
    def extract(self, user_query: str, tool_schema: dict) -> dict:
        """
        从用户查询中提取工具调用参数
        """
        # 使用 LLM 提取结构化参数
        prompt = f"""
从以下文本中提取参数值：

用户文本：{user_query}

目标工具参数定义：
{json.dumps(tool_schema, indent=2)}

请以 JSON 格式返回提取的参数，例如：
{{
  "param1": value1,
  "param2": value2
}}

如果某个参数无法提取，不要包含在结果中。
"""
        
        response = self.llm.ask(prompt)
        params = json.loads(response)
        
        # 验证参数类型
        validated_params = self.validate_types(params, tool_schema)
        
        return validated_params
    
    def validate_types(self, params: dict, schema: dict) -> dict:
        """验证和转换参数类型"""
        validated = {}
        properties = schema.get('properties', {})
        
        for key, value in params.items():
            if key in properties:
                expected_type = properties[key].get('type')
                
                # 类型转换
                if expected_type == 'number':
                    validated[key] = float(value) if '.' in str(value) else int(value)
                elif expected_type == 'integer':
                    validated[key] = int(value)
                elif expected_type == 'boolean':
                    validated[key] = str(value).lower() in ['true', '1', 'yes']
                else:
                    validated[key] = str(value)
        
        return validated
```

---

## 五、工具调用和执行

### 5.1 统一的调用接口

```python
class ToolInvoker:
    def __init__(self, mcp_manager: MCPClientManager):
        self.mcp_manager = mcp_manager
        self.call_history = []  # 记录调用历史用于调试
    
    async def invoke(self, tool_name: str, arguments: dict) -> ToolResult:
        """
        调用指定工具并返回结果
        """
        start_time = time.time()
        
        try:
            # 1. 获取对应的 MCP Client
            client = self.mcp_manager.get_client_for_tool(tool_name)
            
            # 2. 调用工具
            result = await client.call_tool(
                name=tool_name,
                arguments=arguments
            )
            
            # 3. 记录成功调用
            self.call_history.append({
                "tool": tool_name,
                "arguments": arguments,
                "result": "success",
                "duration": time.time() - start_time
            })
            
            return ToolResult(
                success=True,
                data=result.content,
                error=None
            )
            
        except Exception as e:
            # 4. 处理错误
            error_result = ToolResult(
                success=False,
                data=None,
                error=str(e)
            )
            
            self.call_history.append({
                "tool": tool_name,
                "arguments": arguments,
                "result": "error",
                "error": str(e),
                "duration": time.time() - start_time
            })
            
            return error_result
```

### 5.2 错误处理和重试

```python
class RobustToolInvoker:
    def __init__(self, mcp_manager, max_retries=2):
        self.mcp_manager = mcp_manager
        self.max_retries = max_retries
    
    async def invoke_with_retry(self, tool_name: str, arguments: dict):
        """
        带重试的工具调用
        """
        last_error = None
        
        for attempt in range(self.max_retries + 1):
            try:
                result = await self.invoke(tool_name, arguments)
                
                if result.success:
                    return result
                
                # 如果是可重试的错误
                if self.is_retryable_error(result.error):
                    last_error = result.error
                    await asyncio.sleep(2 ** attempt)  # 指数退避
                    continue
                else:
                    return result
                    
            except Exception as e:
                last_error = e
                if attempt < self.max_retries:
                    await asyncio.sleep(2 ** attempt)
                else:
                    return ToolResult(success=False, error=str(e))
        
        return ToolResult(success=False, error=f"Max retries exceeded: {last_error}")
    
    def is_retryable_error(self, error: str) -> bool:
        """判断错误是否可重试"""
        retryable_keywords = ["timeout", "connection", "temporary", "network"]
        return any(keyword in error.lower() for keyword in retryable_keywords)
```

---

## 六、Agent 完整示例

### 6.1 智能助手 Agent

```python
import asyncio
from typing import List, Optional

class MCPEnabledAgent:
    """
    支持 MCP 工具调用的 AI Agent
    """
    
    def __init__(self, llm, mcp_configs: dict):
        """
        Args:
            llm: LLM 实例
            mcp_configs: MCP 服务配置字典
                {
                    "everything": {
                        "command": "npx",
                        "args": ["-y", "@modelcontextprotocol/server-everything"]
                    }
                }
        """
        self.llm = llm
        self.mcp_configs = mcp_configs
        self.mcp_manager = MCPClientManager()
        self.tool_registry = ToolRegistry()
        self.matcher = ToolMatcher(llm)
        self.extractor = ParameterExtractor(llm)
        self.invoker = ToolInvoker(self.mcp_manager)
    
    async def initialize(self):
        """初始化 Agent"""
        print("正在初始化 Agent...")
        
        # 1. 连接所有配置的 MCP 服务
        for name, config in self.mcp_configs.items():
            print(f"  连接 MCP 服务：{name}")
            self.mcp_manager.connect(name, **config)
        
        # 2. 注册工具
        self.tool_registry.register_all_from_manager(self.mcp_manager)
        
        # 3. 生成工具提示词
        self.system_prompt = self.build_system_prompt()
        
        print(f"初始化完成，共加载 {len(self.tool_registry.tools)} 个工具")
    
    def build_system_prompt(self) -> str:
        """构建系统提示词"""
        tools_description = self.tool_registry.format_for_llm()
        
        return f"""你是一个智能助手，可以调用以下工具来帮助用户：

{tools_description}

## 调用规则：
1. 仔细分析用户需求
2. 选择最合适的工具
3. 准确提取参数
4. 调用工具并返回结果
5. 如果工具调用失败，尝试其他方法或告知用户

## 响应格式：
- 如果需要调用工具，返回：
  TOOL_CALL: <tool_name>
  PARAMETERS: <json_params>
  
- 如果直接回答，正常回复
"""
    
    async def chat(self, user_message: str) -> str:
        """
        处理用户消息
        """
        # 1. LLM 判断是否需要调用工具
        response = await self.llm.generate(
            system=self.system_prompt,
            user=user_message,
            max_tokens=500
        )
        
        # 2. 解析响应
        if "TOOL_CALL:" in response:
            # 提取工具调用信息
            tool_name = self.extract_tool_name(response)
            params = self.extract_parameters(response)
            
            # 3. 调用工具
            result = await self.invoker.invoke(tool_name, params)
            
            # 4. 处理结果
            if result.success:
                # 将工具结果反馈给用户
                final_response = await self.llm.generate(
                    system="你是一个助手，请根据工具执行结果回答用户问题。",
                    user=f"用户问题：{user_message}\n工具结果：{result.data}"
                )
                return final_response
            else:
                return f"工具调用失败：{result.error}"
        else:
            # 直接回答
            return response
    
    def extract_tool_name(self, response: str) -> str:
        """从响应中提取工具名称"""
        for line in response.split('\n'):
            if line.startswith("TOOL_CALL:"):
                return line.split(":")[1].strip()
        raise ValueError("No tool call found")
    
    def extract_parameters(self, response: str) -> dict:
        """从响应中提取参数"""
        for line in response.split('\n'):
            if line.startswith("PARAMETERS:"):
                json_str = line.split(":", 1)[1].strip()
                return json.loads(json_str)
        return {}
    
    async def close(self):
        """关闭 Agent"""
        await self.mcp_manager.close_all()


# 使用示例
async def main():
    # 初始化 LLM（伪代码）
    llm = LLM(api_key="your-key")
    
    # 配置 MCP 服务
    mcp_configs = {
        "everything": {
            "command": "npx",
            "args": ["-y", "@modelcontextprotocol/server-everything"]
        },
        "filesystem": {
            "command": "npx",
            "args": ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
        }
    }
    
    # 创建 Agent
    agent = MCPEnabledAgent(llm, mcp_configs)
    
    # 初始化
    await agent.initialize()
    
    # 对话
    while True:
        user_input = input("You: ")
        if user_input.lower() in ["quit", "exit"]:
            break
        
        response = await agent.chat(user_input)
        print(f"Agent: {response}")
    
    # 清理
    await agent.close()

if __name__ == "__main__":
    asyncio.run(main())
```

---

## 七、调试和监控

### 7.1 调用日志

```python
class ToolCallLogger:
    def __init__(self, log_file: str = "tool_calls.log"):
        self.log_file = log_file
    
    def log_call(self, tool_name: str, arguments: dict, result: ToolResult, duration: float):
        """记录工具调用"""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "tool": tool_name,
            "arguments": arguments,
            "success": result.success,
            "result": result.data if result.success else None,
            "error": result.error if not result.success else None,
            "duration_ms": int(duration * 1000)
        }
        
        with open(self.log_file, "a") as f:
            f.write(json.dumps(log_entry) + "\n")
    
    def get_statistics(self) -> dict:
        """获取调用统计"""
        stats = {
            "total_calls": 0,
            "success_count": 0,
            "error_count": 0,
            "avg_duration_ms": 0,
            "tools_usage": {}
        }
        
        with open(self.log_file, "r") as f:
            for line in f:
                entry = json.loads(line)
                stats["total_calls"] += 1
                
                if entry["success"]:
                    stats["success_count"] += 1
                else:
                    stats["error_count"] += 1
                
                tool = entry["tool"]
                if tool not in stats["tools_usage"]:
                    stats["tools_usage"][tool] = 0
                stats["tools_usage"][tool] += 1
        
        return stats
```

### 7.2 性能监控

```python
class PerformanceMonitor:
    def __init__(self):
        self.metrics = {
            "tool_calls": Counter("tool_calls_total", "Total tool calls", ["tool", "success"]),
            "call_duration": Histogram("tool_call_duration_seconds", "Tool call duration", ["tool"]),
            "active_connections": Gauge("mcp_active_connections", "Active MCP connections")
        }
    
    def record_call(self, tool_name: str, duration: float, success: bool):
        """记录调用指标"""
        self.metrics["tool_calls"].labels(tool=tool_name, success=success).inc()
        self.metrics["call_duration"].labels(tool=tool_name).observe(duration)
    
    def update_connection_count(self, count: int):
        """更新连接数"""
        self.metrics["active_connections"].set(count)
```

---

## 八、最佳实践

### 8.1 工具命名规范

```
格式：<来源>_<功能>_<操作>

示例：
- mcp_everything_get-sum
- mcp_filesystem_read_file
- mcp_github_list_issues
```

### 8.2 错误处理策略

```python
# 推荐做法
try:
    result = await invoker.invoke(tool_name, params)
    if result.success:
        return format_result(result.data)
    else:
        # 具体错误处理
        if "timeout" in result.error:
            return "操作超时，请重试"
        elif "permission" in result.error:
            return "权限不足，无法执行"
        else:
            return f"执行失败：{result.error}"
except Exception as e:
    logger.exception("Unexpected error")
    return "发生未知错误，请联系管理员"
```

### 8.3 安全考虑

```python
# 1. 参数验证
def validate_params(params: dict, schema: dict) -> bool:
    # 检查必需参数
    for required in schema.get('required', []):
        if required not in params:
            return False
    
    # 检查参数类型
    for key, value in params.items():
        expected_type = schema.get('properties', {}).get(key, {}).get('type')
        if not is_type_match(value, expected_type):
            return False
    
    # 检查危险操作（如文件删除）
    if involves_dangerous_operation(params):
        require_user_confirmation()
    
    return True

# 2. 访问控制
class AccessControl:
    def __init__(self):
        self.allowed_tools = set()  # 白名单
        self.blocked_tools = set()  # 黑名单
    
    def can_call(self, tool_name: str) -> bool:
        if tool_name in self.blocked_tools:
            return False
        if self.allowed_tools and tool_name not in self.allowed_tools:
            return False
        return True
```

---

## 九、常见问题

### Q1: 如何处理多个 MCP 服务的工具冲突？
**答**: 
- 使用工具名前缀区分（如 `mcp_filesystem_read_file` vs `mcp_github_read_file`）
- 在工具定义中添加 `source` 字段标记来源
- 建立工具路由表，根据名称自动路由到正确的 MCP Client

### Q2: 工具调用失败如何处理？
**答**:
- 实现重试机制（网络错误可重试）
- 提供友好的错误提示
- 记录详细日志便于调试
- 对于关键操作，提供降级方案

### Q3: 如何优化工具匹配速度？
**答**:
- 预计算工具描述的向量嵌入
- 建立关键词索引
- 使用缓存存储常用工具映射
- 对工具进行分类，缩小搜索范围

### Q4: MCP Server 断开连接怎么办？
**答**:
- 实现自动重连机制
- 监听连接状态事件
- 提供离线降级模式
- 在 Agent 初始化时检查所有连接

---

## 十、相关资源

### 官方文档
- [MCP Specification](https://modelcontextprotocol.io/specification)
- [MCP SDK](https://github.com/modelcontextprotocol/sdk)
- [MCP Servers](https://github.com/modelcontextprotocol/servers)

### 社区资源
- [Awesome-MCP-ZH](https://gitcode.com/gh_mirrors/aw/Awesome-MCP-ZH)
- [mcp-go](https://github.com/mark3labs/mcp-go)
- [LangChain MCP Adapters](https://python.langchain.com/docs/integrations/tools/mcp)

### 本项目文档
- [MCP 使用指南](../mcp/mcp-user-guide.md)
- [MCP 开发指南](../mcp/mcp-development-guide.md)
- [MCP 执行结果](../mcp/mcp-execution-results.md)

---

**文档版本:** 1.0  
**最后更新:** 2026-03-07  
**维护团队:** One AI Team
