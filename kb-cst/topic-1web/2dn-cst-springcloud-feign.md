# Feign 

# Feign调用 


## FeignClient 

应该由服务提供方打包好给消费方，虽然消费方自己写也可以

1.  Name 一般为应用名称
2.  Path应用路径
3.  contextId解决bean冲突问题

``` java
@Api(value = "示例接口",tags = "测试接口")
@RequestMapping("/demo")
@FeignClient(name = "${edu.cloud.center.register-name}", path = "${edu.cloud.center.context-path}",contextId = "IHelloApi")
public interface IHelloApi {

    @GetMapping("hello")
    public CommResult<String> hello();
}
```

## EnableFeignClients [enablefeignclients]

必须指定扫的包，否则可能有bean冲突

在配制文件中配制服务方的name和path

``` java
@ComponentScan({"com.edu"})
@EnableFeignClients(basePackages = {"com.edu.center.api"})
@SpringBootApplication
public class SpringbootHdlApplication {

    public static void main(String[] args) {
        SpringApplication.run(SpringbootHdlApplication.class,args);
    }
}
```
``` yaml
hdl:
  cloud:
    center:
      register-name: hdl-cloud-center
      context-path: /cloud/center
```

超时配制

```yaml
    ribbon:
      ConnectTimeout: 3000 # 连接超时时间，默认为1秒，该值会被FeignClient配置connectTimeout覆盖
      ReadTimeout: 5000  # 读超时时间，默认为1秒，该值会被FeignClient配置readTimeout覆盖
      MaxAutoRetries: 1  # 最大重试次数
```
## 使用HttpClient和OkHttp 

在Feign中，Client是一个非常重要的组件，Feign最终发送Request请求以及接收Response响应都是由Client组件来完成的。Client在Feign源码中是一个接口，在默认情况下，Client的实现类是Client.Default。Client.Default是由HttpURLConnection来实现网络请求的。另外,Client还支持HttpClient和OkHttp来进行网络请求。

首先查看FeignRibbonClient的自动配置类FeignRibbonClientAutoConfiguration，该类在程序启动的时候注入一些Bean，其中注入了一个BeanName为feignClient的Client类型的Bean。在省缺配置BeanName为FeignClient的Bean的情况下，会自动注入Client.Default这个对象，跟踪Client.Default源码,Client.Default使用的网络请求框架是HttpURLConnection.

那么如何在Feign中使用HttpClient的框架呢？我们查看FeignAutoConfiguration.

HttpClientFeignConfiguration的源码：

``` java
@Configuration
@ConditionalOnClass({ApacheHttpClient.class})
@ConditionalOnMissingClass({"com.netflix.loadbalancer.ILoadBalancer"})
@ConditionalOnMissingBean({CloseableHttpClient.class})
@ConditionalOnProperty(
    value = {"feign.httpclient.enabled"},
    matchIfMissing = true
)
protected static class HttpClientFeignConfiguration {
```

从代码@ConditionalOnClass({ApacheHttpClient.class})注解可知，只需要在pom文件上加上HttpClient依赖即可。另外需要在配置文件中配置feign.httpclient.enabled为true，从@ConditionalOnProperty注解可知，这个配置可以不写，因为在默认情况下就为true:

``` xml
<dependency>
    <groupId>io.github.openfeign</groupId>
    <artifactId>feign-httpclient</artifactId>
    <version>9.4.0</version>
</dependency>

<dependency>
  <groupId>io.github.openfeign</groupId>
  <artifactId>feign-okhttp</artifactId>
  <version>10.2.0</version>
</dependency>
```

## Feign域名调用 [feign域名调用]

FeignClient接口是否走负载，取决于是否设置了url属性，当有url属性时，直接取出delegate的client实现做handler的调用实现，不再走中间LoadBalancerFeignClient负载流程。

其中name必须配制，值随意，因为不用走负载替换成IP，url为服务地址，一般为域名加端口值，不以http开关会默认添加 “http://” 前缀。

``` java
@FeignClient(
        name = "${edu.cloud.center.regist-name}",
        url = "${edu.cloud.center.host}",
        path = "${edu.cloud.center.path}",
        contextId = "IHttpApi"
)
public interface IHttpApi {
```

# Feign原理

feign 假装的意思，在功能上是给FeignClient注解的接口创建代理，代理的主要功能是将接口调用转成http调用并将结果封装返回。所以Feign原理上主要有几个点要搞清楚：

1.  如何为FeignClient注解接口创建的代理
2.  Feign接口调用和代理处理流程
3.  HTTP调用请求的处理和返回结果的处理

代理注册流程

1.  EnableFeignClients注解中import FeignCientsRegistrar
2.  FeignClientsRegistrar.registerFeignClient
    使用FeignClientFactoryBean注册Bean
3.  FeignClientFactoryBean.getTarget 创建代理bean
4.  Targeter.target最终调用Feign.Builder.target创建bean
5.  最终ReflectiveFeign.newInstance创建bean
6.  最终的代码类为ReflectiveFeign.FeignInvocationHandler，最终的代理方法为SynchronousMethodHandler

调用流程：

1.  代理调用都指向ReflectiveFeign.FeignInvocationHandler.invoke
2.  方法都映射到SynchronousMethodHandler.invoke实现
3.  Feign远程调用都通过LoadBalancerFeignClient实现

Client装配

1.  在注册流程中，
    FeignClientFactoryBean.getTarget会创建Feign.Builder，默认Client是Client.Default
2.  最后loadBalance会通过FeignContext获取Client设置到建Feign.Builder

Client注册

1.  Feign最终获取的是LoadBalancerFeignClient实例
2.  LoadBalancerFeignClient是在启动时条件装配的

## springboot加载

springboot启动类在调用SpringApplication.run时会调用AbstractApplicationContext的refresh方案，在这个过程中会扫注解，注册BEAN

``` java
@Override
public void refresh() throws BeansException, IllegalStateException {
        // Prepare this context for refreshing.
        prepareRefresh();
        // Tell the subclass to refresh the internal bean factory.
        ConfigurableListableBeanFactory beanFactory = obtainFreshBeanFactory();
        // Prepare the bean factory for use in this context.
        prepareBeanFactory(beanFactory);
        // Allows post-processing of the bean factory in context subclasses.
        postProcessBeanFactory(beanFactory);
        // Invoke factory processors registered as beans in the context.
        invokeBeanFactoryPostProcessors(beanFactory);
        // Register bean processors that intercept bean creation.
        registerBeanPostProcessors(beanFactory);
        // Initialize message source for this context.
        initMessageSource();
        // Initialize event multicaster for this context.
        initApplicationEventMulticaster();
        // Initialize other special beans in specific context subclasses.
        onRefresh();
        // Check for listener beans and register them.
        registerListeners();
        // Instantiate all remaining (non-lazy-init) singletons.
        finishBeanFactoryInitialization(beanFactory);
        // Last step: publish corresponding event.
        finishRefresh();
        ......
}
```

## 创建远程接口的本地代理实例

要使用Feign就必须在启动类上加上注解@EnableFeignCleints，这个注解就相当于Feign组件的一个入口，当使用@EnableFeignCleints后，程序启动后，会进行包扫描，扫描所有被@FeignCleint注解修饰的接口，通过JDK底层的动态代理来为远程接口创建代理实例，并且注册到IOC容器中。我们先来看看@EnableFeignCleints这个注解。

``` java
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
@Import(FeignClientsRegistrar.class)
public @interface EnableFeignClients {
    //一系列属性
    ......
}

```

@EnableFeignCleints注解引入了FeignClientsRegistrar类，这个类会在启动时调用registerBeanDefinitions()方法，而这个方法里面只有两行调用其他方法的代码，分别是用来加载Feign的各项配置的方法registerDefaultConfiguration(metadata, registry)和注册实例化对象的方法registerFeignClients(metadata, registry)。我们重点来看一下registerFeignClients()方法，方法的主要的逻辑就是先扫描基础包路径然后，然后找出被@FeignClient注解修饰的接口，放入Set中，接着遍历集合为每个接口逐一创建实例化对象，主要代码如下：

``` java
  //获取扫描的包路径
  Set<String> basePackages;
  basePackages.add(ClassUtils.getPackageName(clazz));
  clientClasses.add(clazz.getCanonicalName());
  ......
  //遍历包路径搜索目标接口
  for (String basePackage : basePackages) {
      Set<BeanDefinition> candidateComponents = scanner.findCandidateComponents(basePackage);
      for (BeanDefinition candidateComponent : candidateComponents) {
      Map<String, Object> attributes = annotationMetadata.getAnnotationAttributes(
                          FeignClient.class.getCanonicalName());
      registerClientConfiguration(registry, name,attributes.get("configuration"));
      //为接口创建代理对象的方法
      registerFeignClient(registry, annotationMetadata, attributes);
      }
  }

private void registerFeignClient(BeanDefinitionRegistry registry,
                AnnotationMetadata annotationMetadata, Map<String, Object> attributes) {
        String className = annotationMetadata.getClassName();
        BeanDefinitionBuilder definition = BeanDefinitionBuilder
                        .genericBeanDefinition(FeignClientFactoryBean.class);
        ......
        String alias = contextId + "FeignClient";

        AbstractBeanDefinition beanDefinition = definition.getBeanDefinition();
        ......
        BeanDefinitionHolder holder = new BeanDefinitionHolder(beanDefinition, className,
                        new String[] { alias });
        BeanDefinitionReaderUtils.registerBeanDefinition(holder, registry);
}
```

可以看到创建对象是交给registerFeignClient()方法来处理的，该方法会在创建对象前进行一系列的准备工作，比如IOC中对象对应的key值统一使用@FeignClient中的value值+”FeignClient”，本例中对应为productFeignClient等。

FeignClientFactoryBean getObject()用于生成所有的bean实例，

``` java
<T> T getTarget() {
        FeignContext context = applicationContext.getBean(FeignContext.class);
        Feign.Builder builder = feign(context);

        if (!StringUtils.hasText(url)) {
                if (!name.startsWith("http")) {
                        url = "http://" + name;
                }
                else {
                        url = name;
                }
                url += cleanPath();
                return (T) loadBalance(builder, context,
                                new HardCodedTarget<>(type, name, url));
        }
        if (StringUtils.hasText(url) && !url.startsWith("http")) {
                url = "http://" + url;
        }
        String url = this.url + cleanPath();
        Client client = getOptional(context, Client.class);
        if (client != null) {
                if (client instanceof LoadBalancerFeignClient) {
                        // not load balancing because we have a url,
                        // but ribbon is on the classpath, so unwrap
                        client = ((LoadBalancerFeignClient) client).getDelegate();
                }
                if (client instanceof FeignBlockingLoadBalancerClient) {
                        // not load balancing because we have a url,
                        // but Spring Cloud LoadBalancer is on the classpath, so unwrap
                        client = ((FeignBlockingLoadBalancerClient) client).getDelegate();
                }
                builder.client(client);
        }
        Targeter targeter = get(context, Targeter.class);
        return (T) targeter.target(this, builder, context,
                        new HardCodedTarget<>(type, name, url));
    }
```

最终的调用是Targeter.target，内部最终调用 Feign.Builder.target(Target<T> target)，创建ReflectiveFeign调用newInstance方法。

``` java
  public <T> T target(Target<T> target) {
    return build().newInstance(target);
  }

  public Feign build() {
    Client client = Capability.enrich(this.client, capabilities);
    Retryer retryer = Capability.enrich(this.retryer, capabilities);
    List<RequestInterceptor> requestInterceptors = this.requestInterceptors.stream()
        .map(ri -> Capability.enrich(ri, capabilities))
        .collect(Collectors.toList());
    Logger logger = Capability.enrich(this.logger, capabilities);
    Contract contract = Capability.enrich(this.contract, capabilities);
    Options options = Capability.enrich(this.options, capabilities);
    Encoder encoder = Capability.enrich(this.encoder, capabilities);
    Decoder decoder = Capability.enrich(this.decoder, capabilities);
    InvocationHandlerFactory invocationHandlerFactory =
        Capability.enrich(this.invocationHandlerFactory, capabilities);
    QueryMapEncoder queryMapEncoder = Capability.enrich(this.queryMapEncoder, capabilities);

    SynchronousMethodHandler.Factory synchronousMethodHandlerFactory =
        new SynchronousMethodHandler.Factory(client, retryer, requestInterceptors, logger,
            logLevel, decode404, closeAfterDecode, propagationPolicy, forceDecoding);
    ParseHandlersByName handlersByName =
        new ParseHandlersByName(contract, options, encoder, decoder, queryMapEncoder,
            errorDecoder, synchronousMethodHandlerFactory);
    return new ReflectiveFeign(handlersByName, invocationHandlerFactory, queryMapEncoder);
  }
}
```

而对于创建对象的所调用的方法一层层深入进去会发现，最终实际创建对象的核心代码是feign.ReflectiveFeign类中的newInstance()方法，其代码如下：

``` java
public <T> T newInstance(Target<T> target) {
  Map<String, MethodHandler> nameToHandler = targetToHandlersByName.apply(target);
  Map<Method, MethodHandler> methodToHandler = new LinkedHashMap<Method, MethodHandler>();
  List<DefaultMethodHandler> defaultMethodHandlers = new LinkedList<DefaultMethodHandler>();

  for (Method method : target.type().getMethods()) {
    if (method.getDeclaringClass() == Object.class) {
      continue;
    } else if (Util.isDefault(method)) {
      DefaultMethodHandler handler = new DefaultMethodHandler(method);
      defaultMethodHandlers.add(handler);
      methodToHandler.put(method, handler);
    } else {
      methodToHandler.put(method, nameToHandler.get(Feign.configKey(target.type(), method)));
    }
  }
  InvocationHandler handler = factory.create(target, methodToHandler);
  T proxy = (T) Proxy.newProxyInstance(target.type().getClassLoader(),
      new Class<?>[] {target.type()}, handler);

  for (DefaultMethodHandler defaultMethodHandler : defaultMethodHandlers) {
    defaultMethodHandler.bindTo(proxy);
  }
  return proxy;
}

```

``` java
static class FeignInvocationHandler implements InvocationHandler {

   private final Target target;
   private final Map<Method, MethodHandler> dispatch;

   FeignInvocationHandler(Target target, Map<Method, MethodHandler> dispatch) {
     this.target = checkNotNull(target, "target");
     this.dispatch = checkNotNull(dispatch, "dispatch for %s", target);
   }

   @Override
   public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
     ......
     return dispatch.get(method).invoke(args);
   }
```

``` java
// classloader 代理的类 对应的处理器
T proxy = (T) Proxy.newProxyInstance(target.type().getClassLoader(),
        new Class<?>[] {target.type()}, handler);
```

看到这段代码相信大家都很熟悉了吧，这不正是JDK底层典型的动态代理嘛，可见Feign也是基于代理模式来创建接口的实例化对象的。

而代理工具类InvocationHandler，Feign也默认提供了两种实现，稍后会提到。

## 封装Request对象并进行编码

底层通过JDK动态代理交由Feign的代理类FeignInvocationHandler进行处理，（Feign底层实现了两个调用处理器分别为FeignInvocationHandler和HystrixInvocationHandler，默认的调用处理器为FeignInvocationHandler，当和Hystrix结合使用时才会使用HystrixInvocationHandler，本篇基于默认的FeignInvocationHandler进行分析）根据方法的对象在映射集合中找到对应的MethodHandler方法处理器。

``` java
static class FeignInvocationHandler implements InvocationHandler {
    private final Map<Method, MethodHandler> dispatch;
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    ......
      //找到对应的方法处理器
      return dispatch.get(method).invoke(args);
    }
}
```

Feign中为MethodHandler接口提供了默认的实现类SynchronousMethodHandler，其中中核心的逻辑就是根据具体的配置先创建RequestTemplate请求模板对象，然后调用Target接口中的apply(RequestTemplate input)方法根据刚刚获取到的请求模板对象创建远程服务请求的Request对象。部分核心代码如下：

``` java
final class SynchronousMethodHandler implements MethodHandler {
    ......
    public Object invoke(Object[] argv) throws Throwable {
        //创建请求模板对象，同时使用Encode接口进行编码操作
        RequestTemplate template = buildTemplateFromArgs.create(argv);
        Retryer retryer = this.retryer.clone();
    //Feign默认提供的重试机制
        while (true) {
              try {
        //发送请求和获取响应结果的核心方法
            return executeAndDecode(template);
              } catch (RetryableException e) {
            retryer.continueOrPropagate(e);
            if (logLevel != Logger.Level.NONE) {
                  logger.logRetry(metadata.configKey(), logLevel);
            }
            continue;
              }
       }
    }

    Object executeAndDecode(RequestTemplate template) throws Throwable {
    //多个方法层层调用这里简单贴出主要逻辑
    for (RequestInterceptor interceptor : requestInterceptors) {
              interceptor.apply(template);
    }
    Request request = target.apply(new RequestTemplate(template));
    if (logLevel != Logger.Level.NONE) {
             logger.logRequest(metadata.configKey(), logLevel, request);
        }
    ......
    }
}
```

经过这一系列的操作后，Request对象就封装好了，我们来简单看一下这个对象的具体结构：

``` java
//请求方法，本例中对应为GET
private final String method;
//要访问的接口地址，本例中对应为/product/getProductInfo/22
private final String url;
//请求头部信息，本例对应的键值对为token=xingren
private final Map<String, Collection<String>> headers;
//请求体，一般POST请求下将复杂对象放入请求体，会被转换成字节数组进行传输
private final byte[] body;
//字符集
private final Charset charset;

```

## feign.Client注册

Client装配

1.  在注册流程中， FeignClientFactoryBean.getTarget会创建Feign.Builder，默认Client是Client.Default
2.  最后loadBalance会通过FeignContext获取Client设置到建Feign.Builder

Client注册

1.  Feign最终获取的是LoadBalancerFeignClient实例
2.  LoadBalancerFeignClient是在启动时条件装配的

在openfeign的spring.factories文件里，第一个FeignRibbonClientAutoConfiguration就是LoadBalancerFeignClient的创建配制

```
    org.springframework.boot.autoconfigure.EnableAutoConfiguration=\
    org.springframework.cloud.openfeign.ribbon.FeignRibbonClientAutoConfiguration,\
    org.springframework.cloud.openfeign.hateoas.FeignHalAutoConfiguration,\
    org.springframework.cloud.openfeign.FeignAutoConfiguration,\
    org.springframework.cloud.openfeign.encoding.FeignAcceptGzipEncodingAutoConfiguration,\
    org.springframework.cloud.openfeign.encoding.FeignContentGzipEncodingAutoConfiguration,\
    org.springframework.cloud.openfeign.loadbalancer.FeignLoadBalancerAutoConfiguration
```

如果httpclient和okhttp都满足装配装配哪个？只会装配一个，另一个因为ConditionalOnMissingBean不满足就不装配了

``` java
  @Configuration(proxyBeanMethods = false)
  class DefaultFeignLoadBalancedConfiguration {
          @Bean
          @ConditionalOnMissingBean
          public Client feignClient(CachingSpringLoadBalancerFactory cachingFactory,
                          SpringClientFactory clientFactory) {
                  return new LoadBalancerFeignClient(new Client.Default(null, null), cachingFactory,
                                  clientFactory);
          }
  }

  @Configuration(proxyBeanMethods = false)
  @ConditionalOnClass(ApacheHttpClient.class)
  @ConditionalOnProperty(value = "feign.httpclient.enabled", matchIfMissing = true)
  @Import(HttpClientFeignConfiguration.class)
  class HttpClientFeignLoadBalancedConfiguration {
          @Bean
          @ConditionalOnMissingBean(Client.class)
          public Client feignClient(CachingSpringLoadBalancerFactory cachingFactory,
                          SpringClientFactory clientFactory, HttpClient httpClient) {
                  ApacheHttpClient delegate = new ApacheHttpClient(httpClient);
                  return new LoadBalancerFeignClient(delegate, cachingFactory, clientFactory);
          }
  }

  @Configuration(proxyBeanMethods = false)
  @ConditionalOnClass(OkHttpClient.class)
  @ConditionalOnProperty("feign.okhttp.enabled")
  @Import(OkHttpFeignConfiguration.class)
  class OkHttpFeignLoadBalancedConfiguration {

          @Bean
          @ConditionalOnMissingBean(Client.class)
          public Client feignClient(CachingSpringLoadBalancerFactory cachingFactory,
                          SpringClientFactory clientFactory, okhttp3.OkHttpClient okHttpClient) {
                  OkHttpClient delegate = new OkHttpClient(okHttpClient);
                  return new LoadBalancerFeignClient(delegate, cachingFactory, clientFactory);
          }

  }
```
## httpclient注册

在client注册时，HttpClient已经注册好了，这个注册流程如下：

1. 在openfeign的jar包里spring.factories有配制FeignLoadBalancerAutoConfiguration配制类
2. FeignLoadBalancerAutoConfiguration配制类import了HttpClientFeignLoadBalancerConfiguration
3. HttpClientFeignLoadBalancerConfiguration类import了HttpClientFeignConfiguration
4. HttpClientFeignConfiguration初始化了CloseableHttpClient实例

``` java
  @Configuration(proxyBeanMethods = false)
  @ConditionalOnMissingBean(CloseableHttpClient.class)
  public class HttpClientFeignConfiguration {

          private final Timer connectionManagerTimer = new Timer(
                          "FeignApacheHttpClientConfiguration.connectionManagerTimer", true);

          private CloseableHttpClient httpClient;

          @Autowired(required = false)
          private RegistryBuilder registryBuilder;

          @Bean
          @ConditionalOnMissingBean(HttpClientConnectionManager.class)
          public HttpClientConnectionManager connectionManager(
                          ApacheHttpClientConnectionManagerFactory connectionManagerFactory,
                          FeignHttpClientProperties httpClientProperties) {
                  final HttpClientConnectionManager connectionManager = connectionManagerFactory
                                  .newConnectionManager(httpClientProperties.isDisableSslValidation(),
                                                  httpClientProperties.getMaxConnections(),
                                                  httpClientProperties.getMaxConnectionsPerRoute(),
                                                  httpClientProperties.getTimeToLive(),
                                                  httpClientProperties.getTimeToLiveUnit(), this.registryBuilder);
                  this.connectionManagerTimer.schedule(new TimerTask() {
                          @Override
                          public void run() {
                                  connectionManager.closeExpiredConnections();
                          }
                  }, 30000, httpClientProperties.getConnectionTimerRepeat());
                  return connectionManager;
          }

          @Bean
          @ConditionalOnProperty(value = "feign.compression.response.enabled",
                          havingValue = "true")
          public CloseableHttpClient customHttpClient(
                          HttpClientConnectionManager httpClientConnectionManager,
                          FeignHttpClientProperties httpClientProperties) {
                  HttpClientBuilder builder = HttpClientBuilder.create().disableCookieManagement()
                                  .useSystemProperties();
                  this.httpClient = createClient(builder, httpClientConnectionManager,
                                  httpClientProperties);
                  return this.httpClient;
          }
    ......

  }

```

## feign.Client返回结果解码

这一步骤Feign的代码量比较多，而且用了大量的try-catch和if-else，整段拿出来展示的话，可读性比较低，我在这里将源码分段进行分析。首先是调用feign.Client接口中的excute()方法执行请求并获取响应结果。对于Client接口，Feign提供了几个不同的实现：

1. 默认的实现Default()类，内部使用JDK中的HttpURLConnnection完成URL请求处理，没有使用连接池，http连接的服用能力弱，性能低，一般不会采用这种方式。
2. 当Feign配合Ribbon使用时提供的默认实现类LoadBalancerFeignClient，内部使用 Ribben 负载均衡技术完成URL请求处理的feign.Client客户端实现类。

除此之外，我们也可以使用其他的http客户端来替代Feign底层默认的实现类。比如ApacheHttpClient，相比传统JDK自带的URLConnection，增加了易用性和灵活性，它不仅使客户端发送Http请求变得容易，而且也方便开发人员测试接口。既提高了开发的效率，也方便提高代码的健壮性。使用起来也很简单，只需引入依赖，然后增加配置feign.httpclient.enabled=true即可。这一步骤，源码简化之后如下：

``` java
Response response;
try { 
    response = client.execute(request, options);
    response.toBuilder().request(request).build();
} catch (IOException e) {
    //一系列的日志处理
}

```

其次是对响应对象的null值处理、响应种类的判断以及响应对象消息体的处理，最后会将body转化成字节数组：

``` java
//判断返回的响应对象的类型是否正确
if (Response.class == metadata.returnType()) {
    //消息体的非空判断
    if (response.body() == null) {
        return response;
    }
    if (response.body().length() == null || response.body().length() > MAX_RESPONSE_BUFFER_SIZE) {
        shouldClose = false;
        return response;
    }
    //转换成字节数组
    byte[] bodyData = Util.toByteArray(response.body().asInputStream());
    return response.toBuilder().body(bodyData).build();
}

```

最后一步是针对不同的http状态码来做不同的处理，如果http状态码为成功，再判断是否有返回值，如果有返回值就对响应对象进行解码操作，无返回值直接返回null；如果是404调用解码方法，最后Decoder底层会执行emptyValueOf()方法返回空的Response对象；如果状态码为其他类型，则返回异常。

``` java
if (response.status() >= 200 && response.status() < 300) {
    if (void.class == metadata.returnType()) {
        return null;
    } else {
        return decode(response);
    }
} else if (decode404 && response.status() == 404 && void.class != metadata.returnType()) {
    return decode(response);
} else {
    throw errorDecoder.decode(metadata.configKey(), response);
}

```

## feign client参数配制

1. 在不做任何配制情况下，Feign的超时时间为readTimeout 60s connectTimeout 10s, 这是在FeignClientFactoryBean注册代理过程中设置的Feign.Builder
2. 在不做任何配制情况下，到Client的超时时间为readTimeout 1s connectTimeout 1s，这是在SynchronousMethodHandler调用到具体Client过程中，LoadBalancerFeignClient做了设置
3. 在设置了feign.client超时时间后，超时时间为设置的值

**默认超时时间**

``` java

class FeignClientFactoryBean
                implements FactoryBean, InitializingBean, ApplicationContextAware {
        private int readTimeoutMillis = new Request.Options().readTimeoutMillis();

        private int connectTimeoutMillis = new Request.Options().connectTimeoutMillis();
}
public Options() {
      this(10, TimeUnit.SECONDS, 60, TimeUnit.SECONDS, true);
}

```

**设置超时时间**

FeignClientFactoryBean

``` java
protected void configureUsingProperties(
                FeignClientProperties.FeignClientConfiguration config,
                Feign.Builder builder) {
        ......
        connectTimeoutMillis = config.getConnectTimeout() != null ? config.getConnectTimeout() : connectTimeoutMillis;
        readTimeoutMillis = config.getReadTimeout() != null ? config.getReadTimeout() : readTimeoutMillis;

        builder.options(new Request.Options(connectTimeoutMillis, TimeUnit.MILLISECONDS, readTimeoutMillis, TimeUnit.MILLISECONDS, true));
        ......
```

默认Client设置项：

1.  connectTimeout
2.  readTimeout
3.  followRedirects

ApacheHttpClient设置项：

1.  connectTimeout
2.  readTimeout

httpClient设置项：

1.  disableSslValidation:false
2.  maxConnections: 200 连接池连接最大连接数
3.  maxConnectionsPerRoute:50 一个服务每次能并行接收的请求数量
4.  timeToLive:900 连接最大闲置时间
5.  timeToliveUnit: 秒
6.  connectionTimerRepeat:3000
7.  connectionTimeout:2000
8.  followRedirects:true

``` java
@Bean
@ConditionalOnMissingBean(HttpClientConnectionManager.class)
public HttpClientConnectionManager connectionManager(
                ApacheHttpClientConnectionManagerFactory connectionManagerFactory,
                FeignHttpClientProperties httpClientProperties) {
        final HttpClientConnectionManager connectionManager = connectionManagerFactory
                        .newConnectionManager(httpClientProperties.isDisableSslValidation(),
                                        httpClientProperties.getMaxConnections(),
                                        httpClientProperties.getMaxConnectionsPerRoute(),
                                        httpClientProperties.getTimeToLive(),
                                        httpClientProperties.getTimeToLiveUnit(), this.registryBuilder);
        this.connectionManagerTimer.schedule(new TimerTask() {
                @Override
                public void run() {
                        connectionManager.closeExpiredConnections();
                }
        }, 30000, httpClientProperties.getConnectionTimerRepeat());
        return connectionManager;
}

private CloseableHttpClient createClient(HttpClientBuilder builder,
                HttpClientConnectionManager httpClientConnectionManager,
                FeignHttpClientProperties httpClientProperties) {
        RequestConfig defaultRequestConfig = RequestConfig.custom()
                        .setConnectTimeout(httpClientProperties.getConnectionTimeout())
                        .setRedirectsEnabled(httpClientProperties.isFollowRedirects()).build();
        CloseableHttpClient httpClient = builder
                        .setDefaultRequestConfig(defaultRequestConfig)
                        .setConnectionManager(httpClientConnectionManager).build();
        return httpClient;
}

```

Feign的client属性类是FeignClientProperties，其中可以根据不同接口的contextId单独设置超时时间等参数，存储在config的map里。

每个接口的配制存储在FeignClientConfiguration配制类里。

``` java
@ConfigurationProperties("feign.client")
public class FeignClientProperties {

    private boolean defaultToProperties = true;

    private String defaultConfig = "default";

    private Map<String, FeignClientConfiguration> config = new HashMap<>();


```
```yaml
feign:
  client:
    config:
      default: # 服务名，填写 default 为所有服务，或者指定某服务，取的接口的contextId
        connectTimeout: 1000
        readTimeout: 2000
  httpclient:
    enabled: true # 关闭 ApacheHttpClient
    max-connections: 50 # 连接池连接最大连接数（缺省值 200）
    time-to-live: 600 # 连接最大闲置时间，单位为秒，600秒==10分钟（缺省值为 900秒==15分钟）
  okhttp:
    enabled: false # 开启 okhttp
```

``` java
public static class FeignClientConfiguration {

    private Logger.Level loggerLevel;

    private Integer connectTimeout;

    private Integer readTimeout;

    private Class<Retryer> retryer;

    private Class<ErrorDecoder> errorDecoder;

    private List<Class<RequestInterceptor>> requestInterceptors;

    private Map<String, Collection<String>> defaultRequestHeaders;

    private Map<String, Collection<String>> defaultQueryParameters;

    private Boolean decode404;

    private Class<Decoder> decoder;

    private Class<Encoder> encoder;

    private Class<Contract> contract;

    private ExceptionPropagationPolicy exceptionPropagationPolicy;
```

httpclient属性配制为FeignHttpClientProperties，默认参数在常量方式定义在类中。

``` java
@ConfigurationProperties(prefix = "feign.httpclient")
public class FeignHttpClientProperties {

    /**
     * Default value for disabling SSL validation.
     */
    public static final boolean DEFAULT_DISABLE_SSL_VALIDATION = false;

    /**
     * Default value for max number od connections.
     */
    public static final int DEFAULT_MAX_CONNECTIONS = 200;

    /**
     * Default value for max number od connections per route.
     */
    public static final int DEFAULT_MAX_CONNECTIONS_PER_ROUTE = 50;

    /**
     * Default value for time to live.
     */
    public static final long DEFAULT_TIME_TO_LIVE = 900L;

    /**
     * Default time to live unit.
     */
    public static final TimeUnit DEFAULT_TIME_TO_LIVE_UNIT = TimeUnit.SECONDS;

    /**
     * Default value for following redirects.
     */
    public static final boolean DEFAULT_FOLLOW_REDIRECTS = true;

    /**
     * Default value for connection timeout.
     */
    public static final int DEFAULT_CONNECTION_TIMEOUT = 2000;

    /**
     * Default value for connection timer repeat.
     */
    public static final int DEFAULT_CONNECTION_TIMER_REPEAT = 3000;
```

## feign 域名调用

FeignClient接口是否走负载，取决于是否设置了url属性，当有url属性时，直接取出delegate的client实现做handler的调用实现，不再走中间LoadBalancerFeignClient负载流程。

其中name必须配制，值随意，因为不用走负载替换成IP，url为服务地址，一般为域名加端口值，不以http开关会默认添加 `http://` 前缀。

``` java
@FeignClient(
        name = "${edu.cloud.center.regist-name}",
        url = "${edu.cloud.center.host}",
        path = "${edu.cloud.center.path}",
        contextId = "IHttpApi"
)
public interface IHttpApi {
```

FeignClientFactoryBean

``` java
<T> T getTarget() {
    FeignContext context = applicationContext.getBean(FeignContext.class);
    Feign.Builder builder = feign(context);

    if (!StringUtils.hasText(url)) {
        if (!name.startsWith("http")) {
            url = "http://" + name;
        }
        else {
            url = name;
        }
        url += cleanPath();
        return (T) loadBalance(builder, context,
                new HardCodedTarget<>(type, name, url));
    }
    if (StringUtils.hasText(url) && !url.startsWith("http")) {
        url = "http://" + url;
    }
    String url = this.url + cleanPath();
    Client client = getOptional(context, Client.class);
    if (client != null) {
        if (client instanceof LoadBalancerFeignClient) {
            // not load balancing because we have a url,
            // but ribbon is on the classpath, so unwrap
            client = ((LoadBalancerFeignClient) client).getDelegate();
        }
        if (client instanceof FeignBlockingLoadBalancerClient) {
            // not load balancing because we have a url,
            // but Spring Cloud LoadBalancer is on the classpath, so unwrap
            client = ((FeignBlockingLoadBalancerClient) client).getDelegate();
        }
        builder.client(client);
    }
    Targeter targeter = get(context, Targeter.class);
    return (T) targeter.target(this, builder, context,
            new HardCodedTarget<>(type, name, url));
}

```

# Feign调优

## 总结

1.  正常情况下使用默认URL配制无问题，200ms和500ms接口都可以达到最佳QPS且无错误
2.  如可想提升服务线程数修改max-threads，使用默认URL在高并发下会报错，需要换成httpclient
3.  httpclient除了 max-connections, per-route也需要配合修改

压测流程：

1.  center服务调优到最优性能
2.  facade服务压测hello接口在无延时、200ms、500ms条件下url配制、httpclient配制的最佳性能

参数调整方法：

1.  响应时间长需要增加服务器线程数
2.  响应时间小增加jmeter线程数
3.  CPU未满增加服务线程数
4.  最佳参数CPU90%+，响应时间未超过接口响应时间一倍，jmeter线程数为用户端最大并发数

httpclient参数调整：

1.  tomcat的max-threads限制服务处理请求的总线程数
2.  httpclient的max-connections限制http调用生成的连接池大小，正常应该不大于max-threads，除非请求中有异常多线程调用Feign接口的场景
3.  httpclient的per-route限制一个服务地址（ip:port或域名:port）连接最大数量，是从max-connections连接池总数中划分出来的
4.  httpclient实际情况是每个route有自己的子连接池，所有route的总连接数不能超过max-connections

配制可以参考2C4G单节点机器性能

## tomcat参数配制

[[2dn-cst-web-tomcat]]

server.tomcat.accept-count=100

server.tomcat.max-threads=200

server.tomcat.min-spare-threads=10

server.tomcat.max-connections=8192

``` java
 // Maximum number of connections that the server accepts and processes at any given time. Once the limit has been reached, the operating system may still accept connections based on the "acceptCount" property.
private int maxConnections = 8192;
// Maximum queue length for incoming connection requests when all possible request processing threads are in use.
private int acceptCount = 100;

// Maximum amount of worker threads.
private int max = 200;
```

## center单服务压测

**简单hello接口**

tomcat默认配制可达2w+tps,
加大参数也不会更好，主要是CPU已经100%，资源都用在线程切换上了，增加线程数已经无意义

理论上一个线程TPS200，默认200个TPS 4w

| jemter线程数 | max-threads | max-connections | accept-count | 90% | 95% | 99% | 吞吐量 | 错误率 | jvm threads | cpu  | 备注   |
|--------------|-------------|-----------------|--------------|-----|-----|-----|--------|--------|-------------|------|--------|
| 100          | 200默认     | 8192默认        | 100默认      | 6   | 6   | 9   | 2.2w+  | 0%     | 150+        | 100% |        |
| 100          | 500         |                 |              | 5   | 6   | 9   | 2.3w+  | 0%     | 150+        | 100% | 无优化 |
|              |             |                 |              |     |     |     |        |        |             |      |        |

**hello接口sleep200ms**

理论上一个线程TPS 5，默认200个TPS 1000，理论最大TPS mat-threads\*5

最大值以max-threads能将CPU打满为止

| jmeter线程数 | max-threads | 90% | 95%  | 99%  | 吞吐量 | 错误率 | jvm threads | cpu  | 备注                               |
|--------------|-------------|-----|------|------|--------|--------|-------------|------|------------------------------------|
| 100          | 200默认     | 206 | 208  | 212  | 400    | 0%     | 150         | 30%  | CPU无压力，TPS增长缓慢             |
| 300          |             | 335 | 344  | 354  | 900    | 0%     | 240         | 30%  | CPU无压力，TPS增长快，响应时间变长 |
| 500          |             | 528 | 535  | 551  | 900    | 0%     | 240         | 30%  | CPU无压力，TPS增长快，响应时间变长 |
| 500          | 400         | 299 | 312  | 337  | 1800   | 0%     | 440         | 40%  | CPU无压力，TPS增长缓慢             |
| 2000         |             | 1s+ | 1s+  | 1s+  | 1800   | 0%     | 440         | 40%  | CPU无压力，TPS增长快，响应时间变长 |
| 2000         | 1000        | 420 | 440  | 490  | 4500   | 0%     | 1040        | 60%  | CPU无压力，TPS增长快               |
| 4000         | 1500        | 1s  | 1.3s | 2.2s | 4400   | 8%     | 1539        | 95%+ | CPU到100%，出现报错                |
| 3500         |             | 600 | 730  | 1s   | 6100+  | 0%     | 1542        | 90%  | CPU90%左右，稳定                   |
| 3000         |             | 460 | 500  | 630  | 6500+  | 0%     | 1540        | 80%  | CPU80%左右，稳定                   |
| 2500         |             | 380 | 390  | 470  | 6700+  | 0%     | 1542        | 70%  | CPU70%左右，稳定                   |
| 2500         | 2000        | 345 | 390  | 540  | 8100+  | 0%     | 2042        | 85%  | CPU85%左右，稳定                   |
| 3000         |             | 500 | 620  | 910  | 7300+  | 0%     | 2042        | 95%+ | CPU95%+，服务CPU不稳定             |
| 3000         | 3000        | 700 | 900  | 2s   | 5400+  | 0%     | 3040        | 100% | 机器卡了，性能不足                 |

**hello接口sleep500ms**

理论上一个线程TPS 2，默认200个TPS 400，理论最大TPS max-threads\*2

个人电脑jdk8，线程数最大3060，受哪限制不清楚

最优max-threads 3000，调用线程最优3000

| jemter线程数 | max-threads | 90%  | 95%  | 99%  | 吞吐量 | 错误率 | jvm threads | cpu  | 备注                                    |
|--------------|-------------|------|------|------|--------|--------|-------------|------|-----------------------------------------|
| 300          | 200默认     | 780  | 780  | 780  | 350+   | 0%     | 242         | 30%  | CPU无压力，TPS增长缓慢，100时特别慢     |
| 3000         | 2000        | 870  | 900  | 960  | 3600+  | 0%     | 2042        | 50%  |                                         |
| 3000         | 4000        | 710  | 810  | 1100 | 4500++ | 0%     | 3100        | 85%  | 线程数上不去                            |
| 4000         | 3000        | 1.2s | 1.5s | 2.8s | 3200+  | 10%    | 3039        | 95%+ |                                         |
| 3000         | 3000        | 630  | 750  | 1160 | 4500+  | 0%     | 3042        | 80%  | 最佳配制                                |
| 3000         | 4000        | 700  | 790  | 1100 | 4500+  | 0%     | 3059        | 90%  | jvm线程到3060就上不去了，应该被哪限制了 |
|              |             |      |      |      |        |        |             |      |                                         |

## facade hello 无延时压测

参考center压测结果，max-threads、max-connections、accept-count三个参数在不加sleep时间情况下无需要调整即可压到瓶颈，所以去除

**默认URL实现**

| jemter线程数 | connectTimeout | readTimeout | 90% | 95% | 99% | 吞吐量 | 错误率 | jvm threads | cpu  | 备注                                         |
|--------------|----------------|-------------|-----|-----|-----|--------|--------|-------------|------|----------------------------------------------|
| 50           | 1s默认         | 1s默认      | 6   | 6   | 17  | 2200   | 0.1%   | 92+         | 90%+ | CPU周期性100%，facade有connect timed out异常 |
|              |                |             |     |     |     |        |        |             |      | 吞吐量先升到5000后不断下降                   |
| 50           | 5s             | 10s         | 5   | 7   | 12  | 3000   | 0.09   | 90          | 90%+ | 运行情况没有变化                             |
|              |                |             |     |     |     |        |        |             |      |                                              |

**httpClient实现**

因为facade和center两个服务同时运行，CPU性能被分配，center单服务的2.2w在两个服务feign调用下性能吞吐量不到8000.

|              |                 |            |                |             |     |     |     |        |        |             |      |                                       |
|--------------|-----------------|------------|----------------|-------------|-----|-----|-----|--------|--------|-------------|------|---------------------------------------|
|              | httpclient      | httpclient |                |             |     |     |     |        |        |             |      |                                       |
| jemter线程数 | max-connections | per-route  | connectTimeout | readTimeout | 90% | 95% | 99% | 吞吐量 | 错误率 | jvm threads | cpu  | 备注                                  |
| 50           | 200默认         | 50默认     | 1s默认         | 1s默认      | 10  | 13  | 23  | 6700+  | 0%     | 117         | 99%+ | CPU平稳在100%，服务稳定               |
|              |                 |            |                |             |     |     |     |        |        |             |      |                                       |
| 50           | 1000            |            |                |             | 10  | 13  | 24  | 7600+  | 0%     | 108         | 99%  | CPU平稳在100%，服务稳定，性能小有提升 |
| 50           | 1000            | 200        |                |             |     |     |     |        |        |             |      | 结果同上无变化                        |

## facade hello 200ms延时压测

理论TPS=服务最大线程\*5

**默认URL实现**

在修改默认线程数200时，调到300就会有异常，参考2C4G配制，CPU未到100%。

|        |             |         |         |     |      |      |        |        |             |      |                          |
|--------|-------------|---------|---------|-----|------|------|--------|--------|-------------|------|--------------------------|
| jemter | tomcat      | connect | read    |     |      |      |        |        |             |      |                          |
| 线程数 | max-threads | Timeout | Timeout | 90% | 95%  | 99%  | 吞吐量 | 错误率 | jvm threads | cpu  | 备注                     |
| 500    | 200默认     | 1s默认  | 1s默认  | 600 | 600  | 630  | 900+   | 0%     | 245         | 40%  | 需要提升服务线程数       |
| 500    | 500         |         |         | 830 | 1500 | 2800 | 700+   | 5%     | 531         | 75%  | 大量Connection reset错误 |
| 300    | 500         |         |         | 350 | 600  | 1.3s | 800+   | 2%     | 346         | 50%+ |                          |
| 500    |             | 5s      | 10s     | 750 | 1s   | 3s   | 970    | 8%     | 538         | 70%  |                          |
| 300    |             |         |         | 450 | 676  | 1.2s | 860    | 3%     | 340         | 50%  |                          |

**httpclient实现**

|        |                 |            |             |         |         |     |      |      |        |        |         |      |                                                 |
|--------|-----------------|------------|-------------|---------|---------|-----|------|------|--------|--------|---------|------|-------------------------------------------------|
| jemter | httpclient      | httpclient | tomcat      | connect | read    |     |      |      |        |        | jvm     |      |                                                 |
| 线程数 | max-connections | per-route  | max-threads | Timeout | Timeout | 90% | 95%  | 99%  | 吞吐量 | 错误率 | threads | cpu  | 备注                                            |
| 500    | 200默认         | 50默认     | 200默认     | 1s默认  | 1s默认  | 3s  | 3.5s | 4.7s | 240    | 0%     | 247     | 30%  |                                                 |
| 500    |                 | 200        |             |         |         | 564 | 570  | 600  | 900+   | 0%     | 247     | 40%  |                                                 |
| 500    | 400             | 400        | 400         |         |         | 310 | 330  | 360  | 1600+  | 0%     | 442     | 45%  |                                                 |
| 1500   | 1000            | 1000       | 1000        |         |         | 450 | 500  | 570  | 3900+  | 0%     | 1047    | 70%  | 响应时间长说明并发量有点高，CPU不满线程还可以提 |
| 3000   | 2000            | 2000       | 2000        |         |         |     |      |      | 3300+  |        | 2045    | 95%+ | 到3000报异常 Too many open files                |
| 2000   | 1500            | 1500       | 1500        |         |         | 520 | 580  | 710  | 4500   | 0%     | 1547    | 90%  | jemter还有点大，响应时间长，线程数也就这样了    |

## facade hello 500ms延时压测

理论TPS=服务最大线程\*2

**默认URL实现**

|        |             |         |         |      |      |      |        |        |         |     |                                        |
|--------|-------------|---------|---------|------|------|------|--------|--------|---------|-----|----------------------------------------|
| jemter | tomcat      | connect | read    |      |      |      |        |        | jvm     |     |                                        |
| 线程数 | max-threads | Timeout | Timeout | 90%  | 95%  | 99%  | 吞吐量 | 错误率 | threads | cpu | 备注                                   |
| 500    | 200         | 1s默认  | 1s默认  | 1.4s | 1.4s | 1.5s | 360+   | 0%     | 243     | 40% |                                        |
| 500    | 300         |         |         | 1s   | 1s   | 1s   | 560+   | 0%     | 346     | 50% |                                        |
| 500    | 500         |         |         | 860  | 960  | 1.8s | 660    | 1.5%   | 546     | 70% | 稳定后的结果，大量Connection reset错误 |
|        |             |         |         |      |      |      |        |        |         |     |                                        |

**httpclient实现**

|        |                 |            |             |         |         |      |      |      |        |        |         |     |                                                           |
|--------|-----------------|------------|-------------|---------|---------|------|------|------|--------|--------|---------|-----|-----------------------------------------------------------|
| jemter | httpclient      | httpclient | tomcat      | connect | read    |      |      |      |        |        | jvm     |     |                                                           |
| 线程数 | max-connections | per-route  | max-threads | Timeout | Timeout | 90%  | 95%  | 99%  | 吞吐量 | 错误率 | threads | cpu | 备注                                                      |
| 500    | 200             | 50         | 200         | 1s      | 1s默认  | 5s   | 6s   | 9s   | 100    | 0%     | 248     | 30% | 响应时间长，需要增加服务器处理能力                        |
| 500    |                 | 200        |             |         |         | 1.3s | 1.3s | 1.3s | 350+   | 0%     | 243     | 30% | cpu未满，需要增加服务线程数                               |
| 500    | 400             | 400        | 400         |         |         | 700  | 700  | 750  | 700+   | 0%     | 447     | 30% | 响应时间短，需要增加调用线程数，cpu未满，服务器增加线程数 |
| 2500   | 1500            | 1500       | 1500        |         |         | 1s   | 1s   | 1.2s | 2600+  | 0.01%  | 1547    | 60% | Bad file descriptor (Write failed),应该是操作系统瓶颈     |
| 2000   | 1300            | 1300       | 1300        |         |         | 1s   | 1s   | 1s   | 2250+  | 0%     | 1347    | 50% | 受限于操作系统，CPU未满                                   |

# Feign项目自查指南 [feign项目自查指南]

## MAVEN引用

在父POM统一引入springcloud和springcloud alibaba依赖，版本关系参考官方说明：https://github.com/alibaba/spring-cloud-alibaba/wiki

``` xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>org.springframework.cloud</groupId>
            <artifactId>spring-cloud-dependencies</artifactId>
            <version>Hoxton.SR9</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
        <dependency>
            <groupId>com.alibaba.cloud</groupId>
            <artifactId>spring-cloud-alibaba-dependencies</artifactId>
            <version>2.2.6.RELEASE</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>
```

Feign接口配置

1.  注册中心应用名一定要配置在配置文件，方便服务方和引用方统一修改
2.  如果有contextPath配置，一定要配置在配置文件，方便服务方和引用方统一修改
3.  contextId一定要写，不要使用spring的bean覆盖配置，因为可以根据contextId单独设置超时时间等参数

``` java
@FeignClient(name = "${edu.cloud.center.regist-name}",path = "${edu.cloud.center.path}",contextId = "IEchoApi")
public interface IEchoApi {
  }
```

## 服务参数

### 默认URL连接方式

如果使用默认URL连接方式，建议配置超时时间，并禁止调大tomcat线程数量，具体参数根据服务实际情况配置：

``` yaml
feign:
  client:
    config:
      default: # 服务名，填写 default 为所有服务，或者指定某服务，取的接口的contextId
        connectTimeout: 5000
        readTimeout: 5000
      IEchoApi:
        connectTimeout: 5000
        readTimeout: 10000
```

### Httpclient连接方式

maven引用，版本未被springcloud管理，需要自己指定：

``` xml
<dependency>
    <groupId>io.github.openfeign</groupId>
    <artifactId>feign-httpclient</artifactId>
    <version>10.1.0</version>
</dependency>
```

tomcat线程数，需要实际压测调大，默认太小了：
```
server:
  port: 8088
  tomcat:
    max-threads: 1300 # Maximum number of worker threads.
```
httpclient配制，需要实际压测，并根据Feign调用关系综合配置，默认太小了。per-route要根据调用服务的性能参考配置。

``` yaml
feign:
  client:
    config:
      default: # 服务名，填写 default 为所有服务，或者指定某服务，取的接口的contextId
        connectTimeout: 5000
        readTimeout: 5000
      IEchoApi:
        connectTimeout: 5000
        readTimeout: 10000
  httpclient:
    enabled: true # 关闭 ApacheHttpClient
    max-connections: 1300 # 连接池连接最大连接数（缺省值 200）
    max-connections-per-route: 1300 # 缺省值 50
  #    time-to-live: 600 # 连接最大闲置时间，单位为秒，600秒==10分钟（缺省值为 900秒==15分钟）
```


[//begin]: # "Autogenerated link references for markdown compatibility"
[2dn-cst-java-web-tomcat]: 2dn-cst-java-web-tomcat.md "tomcat tomcat"
[//end]: # "Autogenerated link references"