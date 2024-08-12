
# sentinel 

-   [spring-cloud-alibaba](https://github.com/alibaba/spring-cloud-alibaba/wiki)
-   [Spring Cloud Alibaba Sentinel](https://github.com/alibaba/spring-cloud-alibaba/wiki/Sentinel)
-   [Sentinel官网](https://sentinelguard.io/zh-cn/index.html)
-   [Sentinel wiki](https://github.com/alibaba/Sentinel/wiki)

## core

``` xml
<dependency>
  <groupId>com.alibaba.csp</groupId>
  <artifactId>sentinel-core</artifactId>
  <version>1.8.1</version>
</dependency>
```

``` java
public class Test1 {
    public static void main(String[] args) throws InterruptedException {
        initFlowRules();
        while (true) {
            try (Entry entry = SphU.entry("HelloWorld")) {
                Thread.sleep(100);
                System.out.println("hello world");
            } catch (BlockException ex) {
                System.out.println("blocked!");
            }
        }
    }
    private static void initFlowRules(){
        List<FlowRule> rules = new ArrayList<>();
        FlowRule rule = new FlowRule();
        rule.setResource("HelloWorld");
        rule.setGrade(RuleConstant.FLOW_GRADE_QPS);
        rule.setCount(5);
        rules.add(rule);
        FlowRuleManager.loadRules(rules);
    }
}
```

## SentinelResource

``` xml
<dependency>
  <groupId>org.springframework</groupId>
  <artifactId>spring-context</artifactId>
  <version>5.2.8.RELEASE</version>
</dependency>
<dependency>
  <groupId>com.alibaba.csp</groupId>
  <artifactId>sentinel-core</artifactId>
  <version>1.8.1</version>
</dependency>
<dependency>
  <groupId>com.alibaba.csp</groupId>
  <artifactId>sentinel-annotation-aspectj</artifactId>
  <version>1.8.1</version>
</dependency>
```

``` java

@Configuration
@EnableAspectJAutoProxy
@ComponentScan("com.onekbase.demo.sentinel")
public class TestMain {
    public static void main(String[] args) throws InterruptedException {
        ApplicationContext context = new AnnotationConfigApplicationContext(TestMain.class);
        TestService ts= context.getBean(TestService.class);
        initFlowRules();
        while (true) {
            Thread.sleep(100);
            System.out.println(ts.hello(123));
        }
    }

    private static void initFlowRules(){
        List<FlowRule> rules = new ArrayList<>();
        FlowRule rule = new FlowRule();
        rule.setResource("hello");
        rule.setGrade(RuleConstant.FLOW_GRADE_QPS);
        // Set limit QPS to 20.
        rule.setCount(2);
        rules.add(rule);
        FlowRuleManager.loadRules(rules);
    }
}

@Configuration
public class AopConfiguration {
    @Bean
    public SentinelResourceAspect sentinelResourceAspect() {
        return new SentinelResourceAspect();
    }
}


@Service
public class TestService {

    @SentinelResource(value = "hello", fallback = "helloFallback")
    public String hello(long s) {
        if (s < 0) {
            throw new IllegalArgumentException("invalid arg");
        }
        return String.format("Hello at %d", s);
    }

    public String helloFallback(long s, Throwable ex) {
        ex.printStackTrace();
        return "Oops, error occurred at " + s;
    }
}

```

## FlowRule

![](assets/2023-11-02_22-53-41_screenshot.png)

可以通过调用 FlowRuleManager.loadRules() 方法来用硬编码的方式定义流量控制规则，比如：

``` java
private static void initFlowQpsRule() {
    List<FlowRule> rules = new ArrayList<>();
    FlowRule rule1 = new FlowRule();
    rule1.setResource(resource);
    // Set max qps to 20
    rule1.setCount(20);
    rule1.setGrade(RuleConstant.FLOW_GRADE_QPS);
    rule1.setLimitApp("default");
    rules.add(rule1);
    FlowRuleManager.loadRules(rules);
}
```

## 日志查看

位置：\~/logs/csp/

- sentinel-record.log** : 系统日志，包括规则加载更新等
- <appName>-metrics.log** : 明细
- sentinel-block.log: 拦截记录

metrics.log

其中 p 代表通过的请求, block 代表被阻止的请求, s 代表成功执行完成的请求个数, e 代表用户自定义的异常, rt 代表平均响应时长。
```
    |--timestamp-|------date time----|--resource-|p |block|s |e|rt
    1529998904000|2018-06-26 15:41:44|hello world|20|0    |20|0|0
    1529998905000|2018-06-26 15:41:45|hello world|20|5579 |20|0|728
    1529998906000|2018-06-26 15:41:46|hello world|20|15698|20|0|0
    1529998907000|2018-06-26 15:41:47|hello world|20|19262|20|0|0
    1529998908000|2018-06-26 15:41:48|hello world|20|19502|20|0|0
    1529998909000|2018-06-26 15:41:49|hello world|20|18386|20|0|0
```
sentinel-record.log

-   以\[FlowRuleManager\] Flow rules loaded: {} 开头
-   DynamicSentinelProperty
-   FlowRuleManager
-   AuthorityRuleManager
-   SystemRuleManager
-   DegradeRuleManager
-   SpiLoader

```
    2023-11-02 22:34:48.050 INFO [FlowRuleManager] Flow rules loaded: {}
    2023-11-02 22:34:48.054 INFO App name resolved from default: com.onekbase.demo.sentinel.Test1
    2023-11-02 22:34:48.054 INFO [SentinelConfig] Application type resolved: 0
    2023-11-02 22:34:48.069 INFO [MetricWriter] Creating new MetricWriter, singleFileSize=52428800, totalFileCount=6
    2023-11-02 22:34:48.073 INFO [DynamicSentinelProperty] Config will be updated to: [FlowRule{resource=HelloWorld, limitApp=default, grade=1, count=5.0, strategy=0, refResource=null, controlBehavior=0, warmUpPeriodSec=10, maxQueueingTimeMs=500, clusterMode=false, clusterConfig=null, controller=null}]
    2023-11-02 22:34:48.077 INFO [FlowRuleManager] Flow rules received: {HelloWorld=[FlowRule{resource=HelloWorld, limitApp=default, grade=1, count=5.0, strategy=0, refResource=null, controlBehavior=0, warmUpPeriodSec=10, maxQueueingTimeMs=500, clusterMode=false, clusterConfig=null, controller=com.alibaba.csp.sentinel.slots.block.flow.controller.DefaultController@b81eda8}]}
    2023-11-02 22:34:48.090 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.init.InitFunc, provider=com.alibaba.csp.sentinel.metric.extension.MetricCallbackInit, aliasName=com.alibaba.csp.sentinel.metric.extension.MetricCallbackInit, isSingleton=true, isDefault=false, order=0
    2023-11-02 22:34:48.092 INFO [InitExecutor] Found init func: com.alibaba.csp.sentinel.metric.extension.MetricCallbackInit
    2023-11-02 22:34:48.095 INFO [InitExecutor] Executing com.alibaba.csp.sentinel.metric.extension.MetricCallbackInit with order 2147483647
    2023-11-02 22:34:48.098 INFO Add child <sentinel_default_context> to node <machine-root>
    2023-11-02 22:34:48.113 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.SlotChainBuilder, provider=com.alibaba.csp.sentinel.slots.DefaultSlotChainBuilder, aliasName=com.alibaba.csp.sentinel.slots.DefaultSlotChainBuilder, isSingleton=true, isDefault=true, order=0
    2023-11-02 22:34:48.114 INFO [SlotChainProvider] Global slot chain builder resolved: com.alibaba.csp.sentinel.slots.DefaultSlotChainBuilder
    2023-11-02 22:34:48.116 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.nodeselector.NodeSelectorSlot, aliasName=com.alibaba.csp.sentinel.slots.nodeselector.NodeSelectorSlot, isSingleton=false, isDefault=false, order=-10000
    2023-11-02 22:34:48.116 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.clusterbuilder.ClusterBuilderSlot, aliasName=com.alibaba.csp.sentinel.slots.clusterbuilder.ClusterBuilderSlot, isSingleton=false, isDefault=false, order=-9000
    2023-11-02 22:34:48.117 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.logger.LogSlot, aliasName=com.alibaba.csp.sentinel.slots.logger.LogSlot, isSingleton=true, isDefault=false, order=-8000
    2023-11-02 22:34:48.120 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.statistic.StatisticSlot, aliasName=com.alibaba.csp.sentinel.slots.statistic.StatisticSlot, isSingleton=true, isDefault=false, order=-7000
    2023-11-02 22:34:48.120 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.block.authority.AuthoritySlot, aliasName=com.alibaba.csp.sentinel.slots.block.authority.AuthoritySlot, isSingleton=true, isDefault=false, order=-6000
    2023-11-02 22:34:48.121 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.system.SystemSlot, aliasName=com.alibaba.csp.sentinel.slots.system.SystemSlot, isSingleton=true, isDefault=false, order=-5000
    2023-11-02 22:34:48.122 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.block.flow.FlowSlot, aliasName=com.alibaba.csp.sentinel.slots.block.flow.FlowSlot, isSingleton=true, isDefault=false, order=-2000
    2023-11-02 22:34:48.123 INFO [SpiLoader] Found SPI implementation for SPI com.alibaba.csp.sentinel.slotchain.ProcessorSlot, provider=com.alibaba.csp.sentinel.slots.block.degrade.DegradeSlot, aliasName=com.alibaba.csp.sentinel.slots.block.degrade.DegradeSlot, isSingleton=true, isDefault=false, order=-1000
    2023-11-02 22:34:48.127 INFO Add child <HelloWorld> to node <sentinel_default_context>
    2023-11-02 22:34:48.129 INFO [AuthorityRuleManager] Load authority rules: {}
    2023-11-02 22:34:48.138 INFO [SystemRuleManager] Current system check status: false, highestSystemLoad: 1.797693e+308, highestCpuUsage: 1.797693e+308, maxRt: 9223372036854775807, maxThread: 9223372036854775807, maxQps: 1.797693e+308
    2023-11-02 22:34:48.141 INFO [DegradeRuleManager] Degrade rules loaded: {}
    2023-11-02 22:34:48.143 WARNING No SPI configuration file, filename=META-INF/services/com.alibaba.csp.sentinel.metric.extension.MetricExtension, classloader=sun.misc.Launcher$AppClassLoader@18b4aac2
    2023-11-02 22:34:48.143 INFO [MetricExtensionProvider] No existing MetricExtension found
    2023-11-02 22:34:49.077 INFO [MetricWriter] New metric file created: /Users/zihuidou/logs/csp/com-onekbase-demo-sentinel-Test1-metrics.log.2023-11-02
    2023-11-02 22:34:49.077 INFO [MetricWriter] New metric index file created: /Users/zihuidou/logs/csp/com-onekbase-demo-sentinel-Test1-metrics.log.2023-11-02.idx
```
## 启动配置

[启动配置项](https://sentinelguard.io/zh-cn/docs/startup-configuration.html)

默认 Sentinel 会尝试从 classpath:sentinel.properties 文件读取配置，读取编码默认为 UTF-8。

`csp.sentinel.log.output.type`可以在控制台输出日志，对于定位问题非常有用。

``` properties
project.name=appname
csp.sentinel.log.use.pid=true
csp.sentinel.log.output.type=console
csp.sentinel.dashboard.server=localhost:9090
csp.sentinel.api.port=8719
```

spring环境日志有效，纯java怎么加载需要确认

## springcloud接入

``` xml
<dependency>
    <groupId>com.alibaba.cloud</groupId>
    <artifactId>spring-cloud-starter-alibaba-sentinel</artifactId>
</dependency>
```

``` java
@SpringBootApplication
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(ServiceApplication.class, args);
    }
}

@Service
public class TestService {

    @SentinelResource(value = "sayHello")
    public String sayHello(String name) {
        return "Hello, " + name;
    }
}

@RestController
public class TestController {

    @Autowired
    private TestService service;

    @GetMapping(value = "/hello/{name}")
    public String apiHello(@PathVariable String name) {
        return service.sayHello(name);
    }
}
```

### Feign 支持

Sentinel 适配了 Feign 组件。如果想使用，除了引入 spring-cloud-starter-alibaba-sentinel 的依赖外还需要 2 个步骤：

配置文件打开 Sentinel 对 Feign 的支持：feign.sentinel.enabled=true

加入 spring-cloud-starter-openfeign 依赖使 Sentinel starter 中的自动化配置类生效

### springcloud 接入控制台
```yml
    spring:
      cloud:
        sentinel:
          transport:
            port: 8719 # 默认值，被占用自动加1
            dashboard: localhost:8080
```
这里的 spring.cloud.sentinel.transport.port 端口配置会在应用对应的机器上启动一个 Http Server，该 Server 会与 Sentinel 控制台做交互。比如 Sentinel 控制台添加了一个限流规则，会把规则数据 push 给这个 Http Server 接收，Http Server 再将规则注册到 Sentinel 中。

## HTTP API

引入`spring-cloud-starter-alibaba-sentinel`会关联引入`sentinel-transport-simple-http`,这会开启一个http服务，核心方法`HttpEventTask.run`。

通过命令行模式映射了31个命名，`SimpleHttpCommandCenter.handlerMap`保存映射

引入了 transport 模块后，可以通过以下的 HTTP API 来获取所有已加载的规则：

    http://localhost:8719/getRules?type=<XXXX>

其中，type=flow 以 JSON 格式返回现有的限流规则，degrade 返回现有生效的降级规则列表，system 则返回系统保护规则。

getRules最终调用地址：

``` java
@CommandMapping(
    name = "getRules",
    desc = "get all active rules by type, request param: type={ruleType}"
)
public class FetchActiveRuleCommandHandler implements CommandHandler<String> {
    public FetchActiveRuleCommandHandler() {
    }

    public CommandResponse<String> handle(CommandRequest request) {
        String type = request.getParam("type");
        if ("flow".equalsIgnoreCase(type)) {
            return CommandResponse.ofSuccess(JSON.toJSONString(FlowRuleManager.getRules()));
        } else if ("degrade".equalsIgnoreCase(type)) {
            return CommandResponse.ofSuccess(JSON.toJSONString(DegradeRuleManager.getRules()));
        } else if ("authority".equalsIgnoreCase(type)) {
            return CommandResponse.ofSuccess(JSON.toJSONString(AuthorityRuleManager.getRules()));
        } else {
            return "system".equalsIgnoreCase(type) ? CommandResponse.ofSuccess(JSON.toJSONString(SystemRuleManager.getRules())) : CommandResponse.ofFailure(new IllegalArgumentException("invalid type"));
        }
    }
}
```


## 源码分析

### FlowRule流控规则

-   FlowSlot 流控规则入口
    -   checkFlow
        -   FlowRuleChecker
            -   checkFlow
            -   canPassCheck
                -   passClusterCheck 集群规则check
                    -   applyTokenResult 处理clusterService返回结果
                        -   fallbackToLocalOrPass 返回失败处理方法
                -   passLocalCheck 本地规则check

FlowRuleChecker.checkFlow对所有rule做通过检查，有不通过的抛出FlowException

``` java
for (FlowRule rule : rules) {
    if (!canPassCheck(rule, context, node, count, prioritized)) {
        throw new FlowException(rule.getLimitApp(), rule);
    }
}
```

fallbackToLocalOrPass

``` java
if (rule.getClusterConfig().isFallbackToLocalWhenFail()) {
    return passLocalCheck(rule, context, node, acquireCount, prioritized);
} else {
    // The rule won't be activated, just pass.
    return true;
}
```

## Sentinel 控制台

下载jar包启动

```
java -Dserver.port=9090 -Dcsp.sentinel.dashboard.server=localhost:9090 -Dproject.name=sentinel-dashboard -jar sentinel-dashboard.jar
```

从 Sentinel 1.6.0 起，Sentinel 控制台引入基本的登录功能，默认用户名和密码都是 sentinel。可以参考 鉴权模块文档 配置用户名和密码。

### 服务端接入控制台

``` xml
<dependency>
    <groupId>com.alibaba.csp</groupId>
    <artifactId>sentinel-transport-simple-http</artifactId>
    <version>x.y.z</version>
</dependency>
```

启动时加入 JVM 参数`-Dcsp.sentinel.dashboard.server=consoleIp:port`指定控制台地址和端口。若启动多个应用，则需要通过`-Dcsp.sentinel.api.port=xxxx`指定服务端监控 API 的端口（默认是 8719）。

除了修改 JVM 参数，也可以通过配置文件取得同样的效果

## 生产使用问题

sentinel的规则都缓存在服务端，服务重启后规则会丢失。控制台在配置规则后会主动推送到服务端，本身只存在内存中，在控制台重启后规则会丢失。

控制台不会主动获取服务端的规则，所以显示和实际规则不一致。控制台不影响sentinel的功能，只是一个规则管理端，没有高可用需求，所以不用集群部署。

所以需要sentinel服务端在服务启动时从一个存储中读取规则，在存储规则变更时能同步更新到服务端。控制台对规则的增删改查都从存储中读取，要和服务端规则一致。

sentinel在开源版本中支持多种存储，但控制台需要较大改动，要重写规则的增删改查逻辑。

其中Nacos是一个比较主流的存储方式，对于集成spingcloud alibaba的项目非常友好，项目通过引入nacos数据源的依赖就可以从nacos实时更新规则。


### 服务端从Nacos读取数据

服务端读取的Nacos配置一般是控制台配置的，服务端的每类规则都要配置一个nacos数据源和配置的dataId，dataId命名规则要和nacos中的配置文件一致，控制台一般会以`${spring.application.name}-${ruleType}-rules`格式命名dataId。

改造后的sentinel控制台将不再给服务端推送规则变更，服务端从nacos监听规则变更。

服务端除了引入sentinel依赖外，还需要引入nacos数据源的依赖，再添加相关配置就可以从nacos中读取规则。

``` xml
<dependency>
    <groupId>com.alibaba.csp</groupId>
    <artifactId>sentinel-datasource-nacos</artifactId>
</dependency>
```

配置文件

```yaml
appconfig:
  nacos:
    server-addr: ${ENV_NACOS_ADDERSS:127.0.0.1:8848}
    namespace: xxx
    username: xxx
    password: xxx
  sentinel:
    nacosconfig:
      namespace: xxx
      groupId: xxx

spring:
  application:
    name: xxx
  cloud:
    nacos:
      config:
        file-extension: yml
        prefix: ${spring.application.name}
        server-addr: ${appconfig.nacos.server-addr}
        namespace: ${appconfig.nacos.namespace}
        username: ${appconfig.nacos.username}
        password: ${appconfig.nacos.password}
      discovery:
        server-addr: ${appconfig.nacos.server-addr}
        namespace: ${appconfig.nacos.namespace}
        username: ${appconfig.nacos.username}
        password: ${appconfig.nacos.password}
    sentinel:
      transport:
        # sentinel-dashboard 控制台地址
        dashboard: localhost:8060
        # clientIp: 127.0.0.1
        # 本应用（sentinel应用客户端）与 sentinel-dashboard 的交互端口，默认8719，如果被占用会自动加1
        port: 8719
      eager: true
      # 本应用监听nacos上的sentinel规则
      datasource:
        # 流控--dsl-flow允许自定义，没有固定要求，主要是里面的配置
        dsl-flow:
          nacos:
            server-addr: ${spring.cloud.nacos.config.server-addr}
            username: ${spring.cloud.nacos.config.username}
            password: ${spring.cloud.nacos.config.password}
            namespace: ${appconfig.sentinel.nacosconfig.namespace}
            groupId: ${appconfig.sentinel.nacosconfig.groupId}
            # 规则类型 flow-流控控制
            rule-type: flow
            # 持久化规则文件在nacos上的名称，必须是应用名-flow-rules
            dataId: ${spring.application.name}-flow-rules
        # 熔断降级
        dsl-degrade:
          nacos:
            server-addr: ${spring.cloud.nacos.config.server-addr}
            username: ${spring.cloud.nacos.config.username}
            password: ${spring.cloud.nacos.config.password}
            namespace: ${appconfig.sentinel.nacosconfig.namespace}
            groupId: ${appconfig.sentinel.nacosconfig.groupId}
            rule-type: degrade
            dataId: ${spring.application.name}-degrade-rules
        # 热点规则
        dsl-param-flow:
          nacos:
            server-addr: ${spring.cloud.nacos.config.server-addr}
            username: ${spring.cloud.nacos.config.username}
            password: ${spring.cloud.nacos.config.password}
            namespace: ${appconfig.sentinel.nacosconfig.namespace}
            groupId: ${appconfig.sentinel.nacosconfig.groupId}
            rule-type: param-flow
            dataId: ${spring.application.name}-param-flow-rules
        # 系统规则
        dsl-system:
          nacos:
            server-addr: ${spring.cloud.nacos.config.server-addr}
            username: ${spring.cloud.nacos.config.username}
            password: ${spring.cloud.nacos.config.password}
            namespace: ${appconfig.sentinel.nacosconfig.namespace}
            groupId: ${appconfig.sentinel.nacosconfig.groupId}
            rule-type: system
            dataId: ${spring.application.name}-system-rules
        # 授权规则
        dsl-authority:
          nacos:
            server-addr: ${spring.cloud.nacos.config.server-addr}
            username: ${spring.cloud.nacos.config.username}
            password: ${spring.cloud.nacos.config.password}
            namespace: ${appconfig.sentinel.nacosconfig.namespace}
            groupId: ${appconfig.sentinel.nacosconfig.groupId}
            rule-type: authority
            dataId: ${spring.application.name}-authority-rules
```
#### 原理

在项目启动时在starter的`SentinelAutoConfiguration`中会注册`SentinelDataSourceHandler`，`SentinelDataSourceHandler`实现了`SmartInitializingSingleton`会在`afterSingletonsInstantiated()`初始化数据源配置，配置使用装配到`SentinelProperties`中的datasource属性。

Nacos最终会装配`NacosDataSource`,这会创建监听器和初始化规则监听器配置。

在调试时，最好打开sentinel的日志，可以看到数据的加载和装配。

sentinel.properties

``` properties
csp.sentinel.log.use.pid=true
csp.sentinel.log.output.type=console
```

支持的配置，配置文件的rule-type对应`com.alibaba.cloud.sentinel.datasource.RuleType`枚举, 只支持：

``` java
public enum RuleType {

	/**
	 * flow.
	 */
	FLOW("flow", FlowRule.class),
	/**
	 * degrade.
	 */
	DEGRADE("degrade", DegradeRule.class),
	/**
	 * param flow.
	 */
	PARAM_FLOW("param-flow", ParamFlowRule.class),
	/**
	 * system.
	 */
	SYSTEM("system", SystemRule.class),
	/**
	 * authority.
	 */
	AUTHORITY("authority", AuthorityRule.class),
	/**
	 * gateway flow.
	 */
	GW_FLOW("gw-flow",
			"com.alibaba.csp.sentinel.adapter.gateway.common.rule.GatewayFlowRule"),
	/**
	 * api.
	 */
	GW_API_GROUP("gw-api-group",
			"com.alibaba.csp.sentinel.adapter.gateway.common.api.ApiDefinition");
```

监听的添加，在`SentinelDataSourceHandler.registerBean`最后会调用`AbstractDataSourceProperties.postRegister`，对不同类型规则添加监听。

```java
public class AbstractDataSourceProperties {

	@NotEmpty
	private String dataType = "json";

	@NotNull
	private RuleType ruleType;

	private String converterClass;

	@JsonIgnore
	private final String factoryBeanName;

	@JsonIgnore
	private Environment env;


	public void postRegister(AbstractDataSource dataSource) {
		switch (this.getRuleType()) {
		case FLOW:
			FlowRuleManager.register2Property(dataSource.getProperty());
			break;
		case DEGRADE:
			DegradeRuleManager.register2Property(dataSource.getProperty());
			break;
		case PARAM_FLOW:
			ParamFlowRuleManager.register2Property(dataSource.getProperty());
			break;
		case SYSTEM:
			SystemRuleManager.register2Property(dataSource.getProperty());
			break;
		case AUTHORITY:
			AuthorityRuleManager.register2Property(dataSource.getProperty());
			break;
		case GW_FLOW:
			GatewayRuleManager.register2Property(dataSource.getProperty());
			break;
		case GW_API_GROUP:
			GatewayApiDefinitionManager.register2Property(dataSource.getProperty());
			break;
		default:
			break;
		}
	}

}
```

### 控制台接入Nacos


首先从github上下载sentinel的源码，sentinel-dashboard为源码中的一个模块。

项目为maven项目，首先运行起项目来，从浏览器可以访问到sentinel控制台。

在项目的test目录中，rule/nacos部分为sentinel控制台的接入nacos的示例。

改造方案：
1. 配置naocs，创建ConfigService
2. 包装公共方法读写nacos配置文件
3. 改造sentinel控制台后端controller通过nacos读写配置

#### 配置nacos

创建包`com.alibaba.csp.sentinel.dashboard.rule.nacos`,

添加配置类`NacosConfig`

```java
@Configuration
public class NacosConfig {

    @Value("${sentinel.datasource.nacos.server-addr:localhost:8848}")
    private String serverAddr;

    @Value("${sentinel.datasource.nacos.namespace:public}")
    private String namespace;

    @Value("${sentinel.datasource.nacos.username:nacos}")
    private String username;

    @Value("${sentinel.datasource.nacos.password:nacos}")
    private String password;

    @Bean
    public ConfigService nacosConfigService() throws NacosException {
        Properties properties = new Properties();
        properties.put(PropertyKeyConst.SERVER_ADDR, serverAddr);
        properties.put(PropertyKeyConst.NAMESPACE, namespace);
        properties.put(PropertyKeyConst.USERNAME, username);
        properties.put(PropertyKeyConst.PASSWORD, password);
        return NacosFactory.createConfigService(properties);
    }
}
```

最关键的是用`ConfigService.getConfig()`和`ConfigService.publishConfig`存取nacos的配置，这里封装一下：

```java

public final class NacosConfigUtil {

    public static final String GROUP_ID = "SENTINEL_GROUP";
    public static final String FLOW_DATA_ID_POSTFIX = "-flow-rules";
    public static final String PARAM_FLOW_DATA_ID_POSTFIX = "-param-flow-rules";
    public static final String DEGRADE_DATA_ID_POSTFIX = "-degrade-rules";
    public static final String AUTHORITY_DATA_ID_POSTFIX = "-authority-rules";
    public static final String SYSTEM_DATA_ID_POSTFIX = "-system-rules";
    public static final String GETWAY_API_DATA_ID_POSTFIX = "-gateway-api-rules";
    public static final String GETWAY_FLOW_DATA_ID_POSTFIX = "-gateway-flow-rules";

    private NacosConfigUtil() {}

    public static <T> List<T> fromNacosRuleEntity(ConfigService configService, String appName, String postfix, Class<T> clazz) throws NacosException {
        String rules = fromNacosRuleString(configService, appName, postfix);
        if (StringUtil.isEmpty(rules)) {
            return new ArrayList<>();
        }
        return JSONUtils.parseObject(clazz, rules);
    }

    public static String fromNacosRuleString(ConfigService configService, String appName, String postfix) throws NacosException {
        AssertUtil.notEmpty(appName, "app name cannot be empty");
        String rules = configService.getConfig(
                genDataId(appName, postfix),
                NacosConfigUtil.GROUP_ID,
                3000
        );
        if (StringUtil.isEmpty(rules)) {
            rules = "";
        }
        return rules;
    }

    public static <T> void publishNacosRuleEntityConfig(ConfigService configService, String appName, String postfix, List<T> rules) throws NacosException{
        AssertUtil.notEmpty(appName, "app name cannot be empty");
        if (rules == null) {
            return;
        }
        publishNacosConfig(configService, appName, postfix, printPrettyJSON(rules));
    }

    public static void publishNacosConfig(ConfigService configService, String appName, String postfix, String rules) throws NacosException{
        AssertUtil.notEmpty(appName, "app name cannot be empty");
        if (StringUtil.isEmpty(rules)) {
            return;
        }
        String dataId = genDataId(appName, postfix);
        boolean publishConfig = configService.publishConfig(
                dataId,
                NacosConfigUtil.GROUP_ID,
                rules
        );
        if(!publishConfig){
            throw new RuntimeException("publish to nacos fail");
        }
    }

    public static String genDataId(String appName, String postfix) {
        return appName + postfix;
    }

    public static String printPrettyJSON(Object obj) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            return JSON.toJSONString(obj);
        }
    }
}
```
#### 封装读写

参考test的设计，每个规则封装一套Provider，Publisher。

注：源码中的FlowControllerV2有问题，整体都要重写，所以这里要不要这么设计不是重点，最终调用ConfigService就可以。

```java
@Component("flowRuleNacosProvider")
public class FlowRuleNacosProvider implements DynamicRuleProvider<List<FlowRuleEntity>> {

    @Autowired
    private ConfigService configService;

    @Override
    public List<FlowRuleEntity> getRules(String appName) throws Exception {
        AssertUtil.notEmpty(appName, "app name cannot be empty");
        return NacosConfigUtil.fromNacosRuleEntity(configService, appName, NacosConfigUtil.FLOW_DATA_ID_POSTFIX, FlowRuleEntity.class);
    }
}

@Component("flowRuleNacosPublisher")
public class FlowRuleNacosPublisher implements DynamicRulePublisher<List<FlowRuleEntity>> {

    @Autowired
    private ConfigService configService;

    @Override
    public void publish(String app, List<FlowRuleEntity> rules) throws Exception {
        AssertUtil.notEmpty(app, "app name cannot be empty");
        if (rules == null) {
            return;
        }
        NacosConfigUtil.publishNacosRuleEntityConfig(configService, app, NacosConfigUtil.FLOW_DATA_ID_POSTFIX, rules);
    }
}
```

封装规则的增删除改查：

```java

public abstract class InDataSourceRuleStore<T extends RuleEntity> {
    private final Logger logger = LoggerFactory.getLogger(InDataSourceRuleStore.class);

    /**
    * 格式化从外部数据源获取到的数据（RuleEntity下的部分数据字段填充等）
    **/
    protected abstract void format(T entity, String app);
    /**
    * 更新规则时部分数据的整合，字段维护
    **/
    protected abstract void merge(T entity, T oldEntity);

    /**
    * 根据当前id获取远程匹配的规则实体
     * 此处只对普通流控做了转换，会经过format进行，其余规则直接返回远程规则对象，后面根据具体情况自行转换改造
    **/
    protected T findById(DynamicRuleProvider<List<T>> ruleProvider, String app, Long id) {
        try {
            // 远程获取规则(当前种类下（如网关流控，普通流控，系统等）的所有规则数据)
            List<T> rules = ruleProvider.getRules(app);
            // 匹配符合当前查询的规则，格式化远端规则数据为sentinel服务端可使用格式（Entity形式）
            if (rules != null && !rules.isEmpty()) {
                Optional<T> entity = rules.stream().filter(rule -> (id.equals(rule.getId()))).findFirst();
                if (entity.isPresent()){
                    T t = entity.get();
                    this.format(t, app);
                    return t;
                }
            }
        } catch (Exception e) {
            logger.error("服务[{}]规则[{}]匹配远端规则异常：{}", app, id, e.getMessage());
        }
        return null;
    }

    /**
    * 获取对应模块下的所有规则，存在format的进行规则转换
    **/
    protected List<T> list(DynamicRuleProvider<List<T>> ruleProvider, String app) throws Exception {
        List<T> rules = ruleProvider.getRules(app);
        if (rules != null && !rules.isEmpty()) {
            for (T entity : rules) {
                this.format(entity, app);
            }
            rules.sort((p1,p2) -> (int) (p1.getId() - p2.getId()));
        } else {
            rules = new ArrayList<>();
        }
        return rules;
    }

    /**
    * 添加规则至远程数据源
     * 添加前先获取远程数据源，再加入本次新增，一起推送到远程数据源（否则存在覆盖的可能）
     * 修改nextId生成规则，原nextId生成由InMemoryRuleRepositoryAdapter类下nextId()方法实现，内部维护了一个AtomicLong实现自增，每次重启则重新从0开始
    **/
    protected void save(DynamicRulePublisher<List<T>> rulePublisher, DynamicRuleProvider<List<T>> ruleProvider, T entity) throws Exception {
        if (null == entity || ObjectUtils.isEmpty(entity.getApp())) {
            throw new InvalidParameterException("app is required");
        }
        if (null != entity.getId()) {
            throw new InvalidParameterException("id must be null");
        }
        List<T> rules = this.list(ruleProvider, entity.getApp());
        // 增规则添加至集合
        long nextId = 1;
        if (!rules.isEmpty()) {
            // 获取集合的最后一个元素，得到id，进行增1操作（集合在）list方法内进行过排序，以保证此处获取到的最后一个元素为当前集合内id最大的元素
            nextId = rules.get(rules.size() - 1).getId() + 1;
        }
        entity.setId(nextId);
        setClusterConfigId(entity);
        rules.add(entity);
        rulePublisher.publish(entity.getApp(), rules);
        sleep();
    }

    /**
     * 因为同步nacso为异步，界面在增删改后会重新获取数据，可能会导致没有获取到变化后的数据，这里等一下
     */
    private void sleep()  {
        try {
            Thread.sleep(300);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    private void setClusterConfigId(Object entity){
        if(entity instanceof FlowRuleEntity){
            FlowRuleEntity flow = (FlowRuleEntity) entity;
            if(flow.getClusterConfig() != null&&flow.getClusterConfig().getFlowId() == null){
                flow.getClusterConfig().setFlowId(flow.getId());
            }
        }
    }

    protected Result<T> update(DynamicRulePublisher<List<T>> rulePublisher, DynamicRuleProvider<List<T>> ruleProvider, T entity) throws Exception {
        if (null == entity || null == entity.getId() || ObjectUtils.isEmpty(entity.getApp())) {
            return Result.ofFail(-1, "id is required");
        }
        // 获取远程规则数据
        List<T> rules = this.list(ruleProvider, entity.getApp());
        if (null == rules || rules.isEmpty()) {
            return Result.ofFail(-1, "Failed to save authority rule, no matching authority rule");
        }
        // 远程规则集合与当前规则匹配项，当前规则填充旧的集合中对应规则数据
        for (int i = 0; i < rules.size(); i++) {
            T oldEntity = rules.get(i);
            if (oldEntity.getId().equals(entity.getId())) {
                // 新旧值替换填充，字段检查
                this.merge(entity, oldEntity);
                setClusterConfigId(entity);
                // 写回规则集合
                rules.set(i, entity);
                break;
            }
        }
        rulePublisher.publish(entity.getApp(), rules);
        sleep();
        return Result.ofSuccess(entity);
    }


    protected Result<Long> delete(DynamicRulePublisher<List<T>> rulePublisher, DynamicRuleProvider<List<T>> ruleProvider, long id, String app) throws Exception {
        List<T> rules = this.list(ruleProvider, app);
        if (null == rules || rules.isEmpty()) {
            return Result.ofSuccess(null);
        }
        boolean removeIf = rules.removeIf(flowRuleEntity -> flowRuleEntity.getId().equals(id));
        if (!removeIf){
            return Result.ofSuccess(null);
        }
        rulePublisher.publish(app, rules);
        sleep();
        return Result.ofSuccess(id);
    }
}
```
#### 修改controller

这里直接修改前端界面对应的后端controller，没有使用FlowControllerV2，因为V2只实现了flow这一类规则其他没实现，而且实现的还有问题。

这里需要注意，可能需要修改前端的代码，前端打包后的dest/js/app.js为最终打包后的js文件，需要前端debug打到代码位置修改。

注意：前端为angular项目，需要会打包的可以直接修改源码重新打包。

```java
@RestController
@RequestMapping(value = "/v1/flow")
public class FlowControllerV1 extends InDataSourceRuleStore<FlowRuleEntity>{

    private final Logger logger = LoggerFactory.getLogger(FlowControllerV2.class);

    @Autowired
    @Qualifier("flowRuleNacosProvider")
    private DynamicRuleProvider<List<FlowRuleEntity>> ruleProvider;
    @Autowired
    @Qualifier("flowRuleNacosPublisher")
    private DynamicRulePublisher<List<FlowRuleEntity>> rulePublisher;

    @GetMapping("/rules")
    @AuthAction(PrivilegeType.READ_RULE)
    public Result<List<FlowRuleEntity>> apiQueryMachineRules(@RequestParam String app) {

        if (StringUtil.isEmpty(app)) {
            return Result.ofFail(-1, "app can't be null or empty");
        }
        try {
            List<FlowRuleEntity> rules = this.list(ruleProvider, app);
            return Result.ofSuccess(rules);
        } catch (Throwable throwable) {
            logger.error("Error when querying flow rules", throwable);
            return Result.ofThrowable(-1, throwable);
        }
    }

    private <R> Result<R> checkEntityInternal(FlowRuleEntity entity) {
        if (entity == null) {
            return Result.ofFail(-1, "invalid body");
        }
        if (StringUtil.isBlank(entity.getApp())) {
            return Result.ofFail(-1, "app can't be null or empty");
        }
        if (StringUtil.isBlank(entity.getLimitApp())) {
            return Result.ofFail(-1, "limitApp can't be null or empty");
        }
        ......
 
        return null;
    }

    @PostMapping("/rule")
    @AuthAction(value = PrivilegeType.WRITE_RULE)
    public Result<FlowRuleEntity> apiAddFlowRule(@RequestBody FlowRuleEntity entity) {

        Result<FlowRuleEntity> checkResult = checkEntityInternal(entity);
        if (checkResult != null) {
            return checkResult;
        }
        try {
            Date date = new Date();
            entity.setGmtCreate(date);
            entity.setGmtModified(date);
            this.save(rulePublisher, ruleProvider, entity);
        } catch (Throwable throwable) {
            logger.error("Failed to add flow rule", throwable);
            return Result.ofThrowable(-1, throwable);
        }
        return Result.ofSuccess(entity);
    }

    @PutMapping("/save.json")
    @AuthAction(PrivilegeType.WRITE_RULE)
    public Result<FlowRuleEntity> apiUpdateFlowRule(@RequestBody FlowRuleEntity entity) {
        if (entity == null) {
            return Result.ofFail(-1, "invalid body");
        }
        Long id =  entity.getId();
        if (id == null || id <= 0) {
            return Result.ofFail(-1, "Invalid id");
        }
        FlowRuleEntity oldEntity = this.findById(ruleProvider, entity.getApp(), id);
        if (oldEntity == null) {
            return Result.ofFail(-1, "id " + id + " does not exist");
        }
        entity.setId(id);

        Result<FlowRuleEntity> checkResult = checkEntityInternal(entity);
        if (checkResult != null) {
            return checkResult;
        }
        try {
            return this.update(rulePublisher, ruleProvider, entity);
        } catch (Throwable throwable) {
            logger.error("Failed to update flow rule", throwable);
            return Result.ofThrowable(-1, throwable);
        }
    }

    @DeleteMapping("/delete.json")
    @AuthAction(PrivilegeType.DELETE_RULE)
    public Result<Long> apiDeleteRule(@RequestParam("id") Long id, @RequestParam("app") String app) {
        if (id == null || id <= 0) {
            return Result.ofFail(-1, "Invalid id");
        }
        if (StringUtils.isEmpty(app)) {
            return Result.ofFail(-1, "Invalid app");
        }
        try {
            return this.delete(rulePublisher, ruleProvider, id, app);
        } catch (Exception e) {
            return Result.ofFail(-1, e.getMessage());
        }
    }

    @Override
    protected void format(FlowRuleEntity entity, String app) {
        entity.setApp(app);
        if (entity.getClusterConfig() != null && entity.getClusterConfig().getFlowId() != null) {
            entity.setId(entity.getClusterConfig().getFlowId());
        }
//        Date date = new Date();
//        entity.setGmtCreate(date);
//        entity.setGmtModified(date);
        entity.setLimitApp(entity.getLimitApp().trim());
        entity.setResource(entity.getResource().trim());
    }

    @Override
    protected void merge(FlowRuleEntity entity, FlowRuleEntity oldEntity) {
        entity.setApp(oldEntity.getApp());
        entity.setIp(oldEntity.getIp());
        entity.setPort(oldEntity.getPort());
        Date date = new Date();
        entity.setGmtCreate(oldEntity.getGmtCreate());
        entity.setGmtModified(date);
    }
}
```
