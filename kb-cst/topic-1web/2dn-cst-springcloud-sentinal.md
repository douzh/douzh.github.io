
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

服务端读取的Nacos配置一般是控制台配置的，服务端的每类规则都要配置一个nacos数据源和配置的dataId，dataId命名规则要和nacos中的配置文件一致，控制台一般会以`${spring.application.name}-${ruleType}-flow`格式命名dataId。

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

### 控制台接入Nacos


首先从github上下载sentinel的源码，sentinel-dashboard为源码中的一个模块。

项目为maven项目，首先运行起项目来，从浏览器可以访问到sentinel控制台。

在项目的test目录中，rule/nacos部分为sentinel控制台的接入nacos的示例。

改造方案：
1. 配置naocs，创建ConfigService
2. 包装公共方法读写nacos配置文件
3. 改造sentinel控制台后端controller通过nacos读写配置

#### 配置nacos


