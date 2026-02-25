# ai agent

## 介绍

**AI Agent（人工智能体/智能代理）** 是以大语言模型（LLM）为核心，具备**自主感知、规划、记忆、工具调用与迭代优化**能力，能独立完成复杂多步骤任务的智能系统，核心是从“被动响应”升级为“主动执行”。

### 一、核心定义与本质
- **标准公式**：**AI Agent = LLM（大脑） + 规划 + 记忆 + 工具调用**
- **本质**：从“工具型AI”升级为“代理型AI”，从“人指挥AI”转向“AI自主/辅助完成复杂任务”
- **OpenAI定义**：以LLM为大脑驱动，具备自主理解、感知、规划、记忆和使用工具能力，可自动化执行复杂任务的系统

### 二、五大核心能力
1. **自主性**：无需全程人工干预，可独立启动、执行、完成多步骤任务
2. **规划能力**：将复杂目标拆解为子任务，动态调整执行路径，处理模糊指令
3. **记忆能力**：短期上下文记忆+长期知识库/向量库记忆，支持多轮交互与经验复用
4. **工具调用**：自主调用API、数据库、RPA、浏览器、代码解释器等外部工具
5. **迭代优化**：基于执行结果反馈，持续优化决策与动作，提升任务成功率

### 三、标准技术架构（感知-决策-行动闭环）
| 模块 | 核心功能 | 技术实现 |
|:--- |:--- |:--- |
| **感知层（Perception）** | 采集环境/任务信息 | 文本、语音、图像、传感器、API数据接入 |
| **决策层（Brain）** | 推理、规划、记忆管理 | LLM+规划引擎+记忆库（短期/长期） |
| **执行层（Action）** | 输出决策、调用工具 | API调用、代码执行、RPA、设备控制 |
| **调度层（Orchestration）** | 管理循环、状态、上下文 | 任务编排、记忆管理、执行流控制 |

### 四、与传统AI/大模型的核心区别
| 对比维度 | 传统AI/大模型 | AI Agent |
|:--- |:--- |:--- |
| **交互方式** | 被动响应、一问一答 | 主动感知、自主执行、多轮交互 |
| **任务能力** | 单步、简单任务 | 多步骤、复杂任务，可拆解目标 |
| **工具调用** | 有限/无，需人工辅助 | 自主调用多类工具 |
| **自主性** | 无，需全程指令 | 高，自主决策与执行 |
| **记忆** | 短期上下文，易遗忘 | 短期+长期记忆，可积累经验 |

### 五、典型应用场景
- **个人助理**：自主规划旅行、管理日程、处理邮件、撰写报告
- **企业服务**：客服智能体、财务/法务自动化、数据分析与报表生成
- **开发运维**：代码生成、Bug排查、自动化测试、云资源管理
- **行业垂直**：医疗诊断辅助、工业设备监控、金融投研分析
- **多智能体协同**：多Agent分工协作完成复杂项目（如软件开发、内容创作）

### 六、主流开发框架
- **LangChain**：最流行的Agent开发框架，支持工具调用、记忆、规划
- **AutoGPT**：开源自主Agent，可自主设定目标、规划并执行任务
- **BabyAGI**：轻量级自主Agent，专注任务拆解与执行
- **CrewAI**：多Agent协作框架，支持角色分工与团队协作

### 七、未来趋势
- **多模态融合**：整合文本、语音、图像、视频等多模态感知与输出
- **多智能体协同**：从单体Agent走向团队化、专业化的Multi-Agent系统
- **行业深度定制**：垂直领域专用Agent，具备更强领域知识与工具能力
- **自主进化**：具备自我学习、自我优化、自我迭代的持续进化能力

## 开发

你需要的 AI Agent 开发入门清单已整理完成，包含**核心组件选型、工具调用配置、最小可运行示例**，覆盖从环境搭建到第一个自主 Agent 运行的全流程，新手可直接跟着操作：

### 一、AI Agent 开发核心组件选型（新手友好版）
| 组件类型       | 核心作用                | 新手推荐选型                          | 补充说明                              |
|----------------|-------------------------|---------------------------------------|---------------------------------------|
| **大语言模型** | Agent 的“大脑”（推理/规划） | OpenAI GPT-4o（易接入）/ 讯飞星火（国产） | 优先选支持函数调用的模型，是工具调用的基础 |
| **开发框架**   | 快速搭建 Agent 能力     | LangChain（Python 版）                | 生态最完善，文档丰富，新手易上手      |
| **记忆存储**   | 短期/长期记忆管理       | 内置 Memory（短期）/ Chroma（向量库，长期） | 入门先不用向量库，用短期记忆即可      |
| **工具集**     | Agent 可调用的外部能力  | SerpAPI（搜索）/ Python 函数（自定义） | 先从“搜索工具”入手，最易验证效果      |
| **运行环境**   | 代码执行基础            | Python 3.10+ / Anaconda               | 版本建议≥3.10，避免依赖兼容问题       |

### 二、环境搭建与依赖安装（一步到位）
#### 1. 基础环境准备
确保已安装 Python 3.10+，执行以下命令安装核心依赖：
```bash
# 安装 LangChain 核心库 + OpenAI 适配器 + 环境变量管理
pip install langchain langchain-openai python-dotenv
# （可选）安装搜索工具依赖（后续示例用）
pip install langchain-community google-search-results
```

#### 2. 密钥配置（关键）
新建 `.env` 文件，填入你的 API 密钥（需提前注册对应平台获取）：
```env
# OpenAI 密钥（https://platform.openai.com/）
OPENAI_API_KEY=your-openai-api-key
# （可选）SerpAPI 密钥（https://serpapi.com/，用于搜索工具）
SERPAPI_API_KEY=your-serpapi-key
```

### 三、最小可运行 AI Agent 示例（自主完成“查天气+总结”任务）
这个示例实现一个**能自主调用搜索工具、规划步骤、输出结果**的基础 Agent，核心能力：接收“查XX城市今日天气并总结”的指令，自主拆解任务→调用搜索工具→整理结果。

```python
# 导入核心库
import os
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain.agents import create_openai_functions_agent, AgentExecutor
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain_community.tools import SerpAPIWrapper
from langchain_core.tools import Tool

# 加载环境变量（读取 .env 文件中的密钥）
load_dotenv()

# 步骤1：初始化 LLM（Agent 大脑）
llm = ChatOpenAI(
    model="gpt-3.5-turbo",  # 新手先用 gpt-3.5-turbo，成本低、响应快
    temperature=0,  # 推理更稳定，避免随机性
    api_key=os.getenv("OPENAI_API_KEY")
)

# 步骤2：定义 Agent 可调用的工具（这里先加“全网搜索”）
# 工具1：SerpAPI 搜索工具
search_tool = SerpAPIWrapper(serpapi_api_key=os.getenv("SERPAPI_API_KEY"))
# 封装工具（指定名称、描述、调用函数，LLM 会根据描述判断是否调用）
tools = [
    Tool(
        name="Search",
        description="用于搜索实时信息（如天气、新闻、股价等），当需要获取最新数据时必须调用此工具",
        func=search_tool.run
    )
]

# 步骤3：定义 Agent 提示词（核心：告诉 Agent 如何思考、规划、使用工具）
prompt = ChatPromptTemplate.from_messages([
    ("system", "你是一个自主的 AI 助手，能自主判断是否需要调用工具完成任务。如果没有实时信息就无法回答，必须调用 Search 工具；如果已有信息，直接回答即可。"),
    ("user", "{input}"),  # 用户输入
    MessagesPlaceholder(variable_name="agent_scratchpad"),  # Agent 思考过程（关键，存储中间推理）
])

# 步骤4：创建 Agent 并配置执行器
agent = create_openai_functions_agent(llm, tools, prompt)
agent_executor = AgentExecutor(
    agent=agent,
    tools=tools,
    verbose=True,  # 开启详细日志，能看到 Agent 的思考、调用工具的全过程
    handle_parsing_errors="返回到上一步重新思考"  # 容错处理，新手友好
)

# 步骤5：运行 Agent（测试任务）
if __name__ == "__main__":
    # 输入任务：让 Agent 自主完成“查北京2026年2月25日天气并总结”
    result = agent_executor.invoke({"input": "查询北京2026年2月25日的天气情况，并总结成100字以内的文字"})
    # 输出最终结果
    print("\n===== Agent 最终回答 =====")
    print(result["output"])
```

### 四、关键代码解释（新手必看）
1. **LLM 初始化**：`ChatOpenAI` 是 Agent 的核心，`temperature=0` 保证推理逻辑稳定，避免 Agent 乱决策；
2. **工具定义**：`Tool` 类需明确 `name`（工具名）、`description`（关键！LLM 靠这个判断是否调用工具）、`func`（工具执行函数）；
3. **提示词模板**：`agent_scratchpad` 是 LangChain Agent 的核心占位符，用于存储 Agent 的思考过程（比如“我需要调用 Search 工具查北京天气”）；
4. **AgentExecutor**：`verbose=True` 能让你清晰看到 Agent 的执行流程（思考→调用工具→获取结果→总结），方便调试。

### 五、运行与验证步骤
1. 替换 `.env` 文件中的 `OPENAI_API_KEY` 和 `SERPAPI_API_KEY` 为你的真实密钥；
2. 运行代码，控制台会输出 Agent 的完整执行过程（示例日志）：
   ```
   > Entering new AgentExecutor chain...
   我需要调用 Search 工具来查询北京2026年2月25日的天气。
   Invoking: `Search` with `北京2026年2月25日天气`
   ...（工具返回结果）
   > Finished chain.
   ===== Agent 最终回答 =====
   北京2026年2月25日天气：晴，气温-2℃~8℃，东北风2级，空气质量优，体感偏冷，建议穿厚外套。
   ```
3. 若运行失败，优先检查：密钥是否正确、网络是否能访问 OpenAI/SerpAPI、Python 版本是否≥3.10。

### 六、进阶扩展方向（入门后可尝试）
1. **添加自定义工具**：比如写一个“本地文件读写工具”，让 Agent 能读取/保存总结结果；
2. **加入长期记忆**：集成 Chroma 向量库，让 Agent 记住历史对话；
3. **多工具组合**：比如“搜索天气→调用日历→规划出行建议”；
4. **切换国产模型**：替换 LLM 为讯飞星火/百度文心一言，适配国内环境。

### 总结
1. 新手开发 AI Agent 的核心是：**LLM（大脑）+ 工具（手脚）+ 框架（骨架）**，先从“调用单一工具完成简单任务”入手；
2. LangChain + OpenAI GPT-3.5/4o 是最易上手的组合，优先掌握“工具定义”和“提示词设计”；
3. 运行时开启 `verbose=True` 是调试关键，能清晰看到 Agent 的思考和执行过程。

如果运行过程中遇到具体报错（比如密钥问题、工具调用失败），可以告诉我具体错误信息，我帮你定位解决。

## java开发

你现在想了解**Java生态下的AI Agent开发工具**，我会从「核心框架、工具调用、记忆管理、LLM对接」四个维度，整理适合Java开发者的AI Agent技术栈，包含入门示例和关键配置，新手可直接落地：

### 一、Java AI Agent 核心工具选型（新手友好）
| 组件类型       | 核心作用                | 新手推荐选型                          | 补充说明                              |
|----------------|-------------------------|---------------------------------------|---------------------------------------|
| **Agent框架**  | 快速搭建Agent能力（规划/工具调用） | LangChain4j（最成熟）| 对标Python版LangChain，专为Java设计  |
| **LLM对接**    | 连接大模型（GPT/国产模型）| LangChain4j + OpenAI/讯飞星火SDK      | 支持函数调用、流式输出，适配Java生态  |
| **记忆存储**   | 短期/长期记忆管理       | 内置Memory（短期）/ Redis/Chroma Java客户端 | 入门先用内置Memory，进阶用向量库      |
| **工具集**     | 调用外部工具/API        | OkHttp（HTTP调用）+ LangChain4j Tool接口 | 自定义工具只需实现简单接口            |
| **运行环境**   | 基础依赖                | JDK 17+ / Maven/Gradle                | 推荐JDK 17，兼容大部分新特性          |

### 二、环境搭建（Maven依赖）
新建Maven项目，在`pom.xml`中添加核心依赖（LangChain4j是Java Agent开发的核心）：
```xml
<dependencies>
    <!-- LangChain4j 核心库（Agent能力） -->
    <dependency>
        <groupId>dev.langchain4j</groupId>
        <artifactId>langchain4j</artifactId>
        <version>0.32.0</version> <!-- 优先用最新稳定版 -->
    </dependency>

    <!-- OpenAI 对接（LLM大脑） -->
    <dependency>
        <groupId>dev.langchain4j</groupId>
        <artifactId>langchain4j-open-ai</artifactId>
        <version>0.32.0</version>
    </dependency>

    <!-- 环境变量管理（密钥配置） -->
    <dependency>
        <groupId>io.github.cdimascio</groupId>
        <artifactId>java-dotenv</artifactId>
        <version>5.2.2</version>
    </dependency>

    <!-- HTTP工具（自定义工具调用） -->
    <dependency>
        <groupId>com.squareup.okhttp3</groupId>
        <artifactId>okhttp</artifactId>
        <version>4.12.0</version>
    </dependency>
</dependencies>
```

### 三、Java 最小可运行AI Agent示例（调用搜索工具查天气）
这个示例基于LangChain4j实现，功能和之前Python版一致：接收“查XX城市天气”指令，Agent自主判断并调用搜索工具，返回总结结果。

#### 步骤1：配置密钥（.env文件）
在项目根目录新建`.env`文件：
```env
# OpenAI密钥
OPENAI_API_KEY=your-openai-api-key
# SerpAPI密钥（搜索工具，https://serpapi.com/）
SERPAPI_API_KEY=your-serpapi-key
```

#### 步骤2：完整代码实现
```java
import dev.langchain4j.agent.tool.Tool;
import dev.langchain4j.model.openai.OpenAiChatModel;
import dev.langchain4j.service.AiServices;
import io.github.cdimascio.dotenv.Dotenv;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

// 1. 定义Agent的能力接口（核心：声明Agent能做的事 + 绑定工具）
interface WeatherAgent {
    // Agent的核心方法：接收用户指令，返回结果
    String chat(String userInput);

    // 2. 定义工具方法：标注@Tool，Agent会自主判断是否调用
    @Tool("用于搜索实时天气信息，参数为城市名+日期，例如：北京2026年2月25日")
    default String searchWeather(String query) {
        // 调用SerpAPI实现天气搜索（工具核心逻辑）
        OkHttpClient client = new OkHttpClient();
        String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
        String url = String.format(
            "https://serpapi.com/search?q=%s&api_key=%s",
            encodedQuery,
            Dotenv.load().get("SERPAPI_API_KEY")
        );

        Request request = new Request.Builder().url(url).build();
        try (Response response = client.newCall(request).execute()) {
            if (response.isSuccessful() && response.body() != null) {
                return response.body().string(); // 返回搜索结果
            }
        } catch (IOException e) {
            return "搜索失败：" + e.getMessage();
        }
        return "未获取到天气信息";
    }
}

// 3. 运行Agent
public class JavaAiAgentDemo {
    public static void main(String[] args) {
        // 加载环境变量
        Dotenv dotenv = Dotenv.load();
        
        // 初始化LLM（Agent大脑）
        OpenAiChatModel llm = OpenAiChatModel.builder()
                .apiKey(dotenv.get("OPENAI_API_KEY"))
                .modelName("gpt-3.5-turbo") // 新手先用gpt-3.5-turbo
                .temperature(0) // 推理稳定，避免随机
                .build();

        // 4. 创建Agent实例（LangChain4j自动绑定工具+LLM）
        WeatherAgent agent = AiServices.create(WeatherAgent.class, llm);

        // 5. 运行Agent：查询北京2026年2月25日天气
        String result = agent.chat("查询北京2026年2月25日的天气情况，总结成100字以内");
        
        // 输出结果
        System.out.println("===== Agent 最终回答 =====");
        System.out.println(result);
    }
}
```

### 四、关键代码解释（Java Agent核心）
1. **Agent接口定义**：`WeatherAgent`接口是核心，`chat()`方法是Agent的入口，`searchWeather()`方法标注`@Tool`后，LangChain4j会自动让LLM识别并自主调用；
2. **LLM初始化**：`OpenAiChatModel`封装了OpenAI的API调用，只需配置密钥和模型名，无需手动写HTTP请求；
3. **AiServices.create()**：LangChain4j的核心魔法——自动将接口转换为可运行的Agent，无需手动处理“思考→调用工具→总结”的流程；
4. **工具方法**：`@Tool`注解的描述是关键（LLM靠这个判断是否调用），方法参数/返回值越清晰，Agent调用越准确。

### 五、Java Agent 进阶工具扩展
#### 1. 对接国产大模型（讯飞星火/百度文心）
LangChain4j支持国产模型，只需替换LLM实现，示例（讯飞星火）：
```java
// 需额外添加讯飞星火依赖
<dependency>
    <groupId>dev.langchain4j</groupId>
    <artifactId>langchain4j-xunfei</artifactId>
    <version>0.32.0</version>
</dependency>

// 初始化讯飞星火LLM
XunFeiSparkChatModel llm = XunFeiSparkChatModel.builder()
        .appId(dotenv.get("XUNFEI_APP_ID"))
        .apiKey(dotenv.get("XUNFEI_API_KEY"))
        .apiSecret(dotenv.get("XUNFEI_API_SECRET"))
        .build();
```

#### 2. 长期记忆管理（Redis）
```java
// 添加Redis依赖
<dependency>
    <groupId>dev.langchain4j</groupId>
    <artifactId>langchain4j-redis</artifactId>
    <version>0.32.0</version>
</dependency>

// 配置Redis记忆存储
RedisChatMemory memory = RedisChatMemory.builder()
        .redisHost("localhost")
        .redisPort(6379)
        .sessionId("user-123") // 每个用户一个session
        .build();

// 创建Agent时绑定记忆
WeatherAgent agent = AiServices.builder(WeatherAgent.class)
        .chatLanguageModel(llm)
        .chatMemory(memory)
        .build();
```

#### 3. 自定义本地工具（比如文件读写）
```java
@Tool("用于将文本保存到本地文件，参数1：文件路径，参数2：文本内容")
default String saveToFile(String filePath, String content) {
    try {
        java.nio.file.Files.write(
            java.nio.file.Paths.get(filePath),
            content.getBytes(StandardCharsets.UTF_8)
        );
        return "文件保存成功：" + filePath;
    } catch (IOException e) {
        return "保存失败：" + e.getMessage();
    }
}
```

### 六、常见问题解决
1. **依赖冲突**：优先用JDK 17，避免JDK 8兼容问题；
2. **工具调用失败**：检查`@Tool`注解的描述是否清晰，LLM靠描述判断是否调用；
3. **国产模型对接**：确保API密钥、AppId配置正确，讯飞/百度需先申请开发者账号；
4. **日志调试**：添加SLF4J日志依赖，开启DEBUG级别，查看Agent思考/调用过程。

### 总结
1. Java开发AI Agent的核心框架是**LangChain4j**，对标Python的LangChain，无需从零实现工具调用、记忆管理；
2. 关键是通过`@Tool`注解定义工具，`AiServices.create()`绑定LLM和工具，快速生成Agent；
3. 对接国产模型只需替换LLM实现类，工具扩展只需实现带`@Tool`的方法，新手易上手。

如果运行代码时遇到具体报错（比如密钥、依赖、工具调用问题），可以告诉我错误信息，我帮你定位解决。