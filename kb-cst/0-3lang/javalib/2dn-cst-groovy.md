# groovy动态编译


## 动态编译

### 编译与执行

**脚本的编译**

groovy提供了动态加载脚本编译的支持，通过GroovyScriptEngine实现：

```java
    @PostConstruct
    public void init() {
        try {
            // 使用文件系统路径创建GroovyScriptEngine
            scriptEngine = new GroovyScriptEngine(scriptPath);
            log.info("groovey script path:"+scriptPath);

            // 生产环境：按修改时间检查重新编译
            scriptEngine.getGroovyClassLoader().setShouldRecompile(true);

            // 开发环境：总是重新编译
            // scriptEngine.setRecompileMode(GroovyScriptEngine.RECOMPILE_ALWAYS);

        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize GroovyScriptEngine", e);
        }
    }
```

GroovyScriptEngine支持缓存编译结果，当脚本修改后才会再次编译。

**脚本的执行**

```java
    public static Object executeScript(String scriptName, Map<String, Object> parameters) {
        try {
            Binding binding = new Binding();

            // 绑定参数
            if (parameters != null) {
                parameters.forEach(binding::setVariable);
            }

            // 执行脚本
            Object rs =  scriptEngine.run(scriptName, binding);
            return rs;

        } catch (Exception e) {
            throw new RuntimeException("Failed to execute script: " + scriptName, e);
        }
    }
```

scriptName为脚本在配置的脚本目录下的全路径，如`com/onekbase/groovy/scripts/demo/test.groovy`

### 动态编译说明

所有的脚本文件都会编译成以脚本文件名为名的类。

**纯脚本文件**

任何 .groovy 文件，如果包含顶层语句（脚本代码），都会生成 Script 子类

``` groovy
def result =  "Parameters: ${binding.variables}"
if (binding.variables.containsKey('name')) {
    result += " with name: ${name}"
}
println "Hello from Groovy test script!"
println result
return result
```

编译后会生成一个继承自 `groovy.lang.Script` 的类。根据你提供的脚本，编译后的类结构如下：

- **类名**：`Demo1Script`（与脚本文件同名）
- **父类**：`groovy.lang.Script`
- `Demo1Script()` - 无参构造函数
- `Demo1Script(Binding binding)` - 带Binding参数的构造函数
- **`run()` 方法** - 重写自Script类，包含脚本的主要逻辑
  - 实现了脚本中所有的语句和表达式
  - 处理变量绑定和字符串插值逻辑
- **`main(String[] args)` 静态方法** - 使脚本可以独立运行
- 脚本中的 `binding.variables` 访问会被编译为对绑定对象的适当方法调用
- 字符串插值 `${binding.variables}` 和 `${name}` 会被转换为字符串连接操作

**纯类文件**

``` groovy
// User.groovy - 仅包含类定义，无顶层脚本代码
package com.onekbase.groovy.scripts.demo

class User {
    String name
    String email
    int age
    // ...
}
```

Groovy 会将其视为普通类文件，不生成 Script 子类。

**混合文件**

类生成类文件，顶层代码会被编译成 Script 子类。

### 重新自动编译

- GroovyScriptEngine
  - Class loadScriptByName(String scriptName)
    - isSourceNewer(entry)
    - clazz = groovyLoader.parseClass(content, path);
  - Script script = InvokerHelper.createScript(clazz, binding);
  - script.run()

重新编译是通过`loadScriptByName`方法实现的，`loadScriptByName`每次调用都会检测脚本文件的修改时间，如果脚本文件被修改，则重新编译脚本。

脚本关联的类如果修改，脚本也会重新编译，这是通过`isSourceNewer`方法实现的，每个编译的脚本实体都存储了依赖的脚本，`isSourceNewer`会检测依赖的脚本是否有修改，如果依赖的脚本有修改，本脚本也会重新编译。

如果多个类有引用关系，如 `Script->Service->Dao`，当Dao脚本进行修改时不会触发重新编译，当Service脚本进行修改时不仅会触发Script重新编译还会重新编译修改后的Dao。

### spring混合使用问题

如果使用静态编译groovy，spring使用和java差异。

动态编译的问题在于需要手工定制bean的注册与销毁，而且存在历史引用失效、依赖管理等问题

解决方案：
1. 接口层提供统一接口，通过脚本ID调用脚本
2. 服务层脚本类不注册spring bean，代码中直接new 对象
3. spingboot项目提供工具类可以getBean，以获取环境常用bean
4. 数据层使用动态配置查询管理方式实现，提供统一的工具类由脚本调用
5. mybatis、redis的各种Template可以用工具getBean方式直接获取

### spring ioc

1. ioc主要为了解决**面向接口编程**中实现类需要经常替换的问题。

在动态脚本中此类需求可以将功能实现为**通过脚本ID获取脚本Class再创建对象**，通过配置脚本的ID切换实现类。

公共资源bean还是通过spring管理，如数据库连接、redis连接等。

2. ioc的第二个优点为方便创建单例bean，减少new操作产生的资源占用和回收问题。

这可以通过包装**通过脚本ID获取脚本Class再创建对象**操作，加实例缓存实现类似单例管理的功能。

用枚举封装类似ioc容器的对象，调用示例

```groovy
IUserService userServiceNew = SidService.IUserService.singleton()

enum SidService {
    IUserService(UserService),

    private final Class<?> clazz

    // 添加构造函数
    SidService(Class<?> clazz) {
        this.clazz = clazz
    }

    <T> T singleton() {
        return GE.singleton(clazz)
    }

    <T> T prototype() {
        return GE.prototype(clazz)
    }

}


class GE {
    static <T> T singleton(String sid) {
        return MetaGroovyEngine.singleton(sid)
    }

    static <T> T prototype(String sid) {
        return MetaGroovyEngine.prototype(sid)
    }

    static <T> T newObject(String sid) {
        return MetaGroovyEngine.newObject(sid)
    }

    static <T> T singleton(Class<T> clazz) {
        return MetaGroovyEngine.singleton(getScriptIdByClassName(clazz.getName()))
    }

    static <T> T prototype(Class<T> clazz) {
        return MetaGroovyEngine.prototype(getScriptIdByClassName(clazz.getName()))
    }

    static <T> T newObject(Class<T> clazz) {
        return MetaGroovyEngine.newObject(getScriptIdByClassName(clazz.getName()))
    }

    static <T> T getBean(Class<T> clazz) {
        return MetaGroovyEngine.getBean(clazz);
    }


    static <T> T getBean(String beanName,Class<T> clazz) {
        return MetaGroovyEngine.getBean(beanName,clazz)
    }

    static String getScriptIdByClass(Class<?> clazz) {
        return getScriptIdByClassName(clazz.getName())
    }

    static String getScriptIdByClassName(String className) {
        return className.replace('.', '/')+".groovy";
    }
}
```

MetaGroovyEngine

```groovy
    private static Map<String, Singleton> singletonCache = new HashMap<>();

    public static Object executeScript(String scriptId, Map<String, Object> parameters) {
        return executeScript(scriptId,parameters,properties.getScriptCache());
    }

    public static Object executeScript(String scriptId, Map<String, Object> parameters, boolean cacheScript) {
        List<GroovyScriptAspect> matchedAspects = null;
        Long startTime = System.currentTimeMillis();
        try {
            Binding binding = new Binding();
            // 绑定参数
            if (parameters != null) {
                parameters.forEach(binding::setVariable);
            }
            Script script = (Script)newObject(scriptId, cacheScript, true,binding);
            if(script==null){
                throw new RuntimeException("script create fail, script name: " + scriptId);
            }
            // 如果脚本缓存了，Binding也会缓存到脚本对象中，所以要重新设置一下
            if(cacheScript) {
                script.setBinding(binding);
            }
            matchedAspects = scriptAspects.stream().map(aspectId -> (GroovyScriptAspect) newObject(aspectId, properties.getAspectCache(), false,null)).filter(aspect -> aspect.matches(scriptId)).collect(Collectors.toList());
            // 1. 前置通知：所有切面执行before
            matchedAspects.forEach(aspect -> aspect.before(scriptId, binding));
            // 执行脚本
            Object rs =  script.run();
            // 3. 后置返回通知：所有切面执行afterReturning
            matchedAspects.forEach(aspect -> aspect.afterReturning(scriptId, rs));
            return rs;
        } catch (Exception e) {
            log.error("Failed to execute script: {}" , scriptId, e);
            // 4. 异常通知：所有切面执行afterThrowing
            matchedAspects.forEach(aspect -> aspect.afterThrowing(scriptId, e));
            throw new RuntimeException("Failed to execute script: " + scriptId, e);
        }finally {
            // 5. 最终通知：所有切面执行after（无论是否异常）
            matchedAspects.forEach(aspect -> aspect.after(scriptId));
            if(properties.getIsDebug()) {
                long cost = System.currentTimeMillis() - startTime;
                log.info("Performance Monitoring scriptId:{} cost: {} ms", scriptId, cost);
            }
        }
    }


    static class Singleton{

        protected String scriptId;
        protected Class<?> clazz;
        protected Object singleton;

        public Singleton(Class<?> clazz, Object singleton) {
            this.clazz = clazz;
            this.singleton = singleton;
        }
        public Singleton(String scriptId) {
            this.scriptId = scriptId;
        }
        public Singleton(String scriptId,Class<?> clazz, Object singleton) {
            this.scriptId = scriptId;
            this.clazz = clazz;
            this.singleton = singleton;
        }
    }

    public static Object singleton(String scriptId) {
        return newObject(scriptId,true,false,null);
    }

    public static Object prototype(String scriptId) {
        return newObject(scriptId,false,false,null);
    }

    public static Object newObject(String scriptId) {
        return newObject(scriptId,properties.getNewCache(),false,null);
    }


    private static Object newObject(String scriptId,boolean cache,boolean isScript,Binding binding) {
        try {
            Singleton cacheSingleton = singletonCache.get(scriptId);
            // 缓存 不检查变更 且有实例，直接返回
            if(cache && !properties.getCheckModify() && cacheSingleton!=null){
                return cacheSingleton.singleton;
            }
            // 下面的的逻辑都要用到类
            Class<?> clazz = getClass(scriptId) ;
            if(clazz==null) return null ;
            // 不缓存每次新建
            if(!cache){
                singletonCache.remove(scriptId);
                return isScript? InvokerHelper.createScript(clazz, binding):clazz.newInstance();
            }
            // 需要缓存，但没有缓存过，直接生成并缓存，不用考虑线程安全，并发创建多个缓存一个就行
            if(cacheSingleton == null){
                Object o = isScript? InvokerHelper.createScript(clazz, binding):clazz.newInstance();
                singletonCache.put(scriptId,new Singleton(scriptId,clazz,o));
                log.info("new instance:{}",scriptId);
                return o;
            }
            // 缓存过对比class是否有变化，scriptEngine没有重新编译脚本且缓存过对象，直接返回缓存对象
            if(clazz==cacheSingleton.clazz){
                return cacheSingleton.singleton;
            }
            // 缓存过对象，但scriptEngine重新编译了脚本，重新生成对象并缓存
            // 不用考虑线程安全，并发创建多个缓存一个就行
            singletonCache.remove(scriptId);
            Object o = isScript? InvokerHelper.createScript(clazz, binding):clazz.newInstance();
            singletonCache.put(scriptId,new Singleton(scriptId,clazz,o));
            log.info("renew instance:{}",scriptId);
            return o;
        } catch (Exception e) {
            log.error("new instance error:{}",scriptId,e);
            throw new RuntimeException(e);
        }
    }

    private static Class<?> getClass(String scriptId) {
        try {
            if(properties.getCheckModify()){
                // 都用loadScriptByName获取类，每次都会检测脚本是否变更，有变更会重新编译类
                return scriptEngine.loadScriptByName(scriptId);
            }
            Singleton cacheSingleton = singletonCache.get(scriptId);
            if(cacheSingleton==null){
                return scriptEngine.loadScriptByName(scriptId);
            }
            // isRecompile=true时执行脚本关联的类也会重新编译, 所以还要对比是否重新生成
            // return scriptEngine.getGroovyClassLoader().loadClass(cacheSingleton.getClass().getName());
            return cacheSingleton.clazz;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
```



### spring aop

aop是为了面向切面编程，在动态脚本中此类需求可以将功能实现为包装脚本的run接口口，在run方法前后执行切面逻辑。

只能对执行的脚本做切面，不能对脚本内调用的类方法做切面，所以脚本内调用的其他功能需要切面需要将被调用方法做成脚本并用执行脚本接口调用。


切面接口

```java
public interface GroovyScriptAspect {

    default boolean matches(String scriptId) {
        return true;
    }

    default void before(String scriptId, Binding binding) {}

    default void afterReturning(String scriptId, Object result) {}

    default void afterThrowing(String scriptId, Throwable throwable) {}

    default void after(String scriptId) {}
}
```

切面示例

```groovy
class LogGroovyAspect implements GroovyScriptAspect {

    private static final Logger log = LoggerFactory.getLogger(LogGroovyAspect.class);

    @Override
    void before(String scriptId, Binding binding) {
        log.info("【Groovy脚本执行前置】scriptId={}, 参数={}", scriptId, binding);
    }

    @Override
    void afterReturning(String scriptId, Object result) {
        log.info("【Groovy脚本执行成功】scriptId={}, 结果={}", scriptId, result);
    }

    @Override
    void afterThrowing(String scriptId, Throwable throwable) {
        log.error("【Groovy脚本执行异常】scriptId={}", scriptId, throwable);
    }

    @Override
    void after(String scriptId) {
        log.info("【Groovy脚本执行最终】scriptId={} 执行完成", scriptId);
    }
}


class PerformanceGroovyAspect implements GroovyScriptAspect {

    private static final Logger log = LoggerFactory.getLogger(PerformanceGroovyAspect.class);

    // 存储脚本执行开始时间（线程安全）
    private final ConcurrentHashMap<String, Long> startTimeMap = new ConcurrentHashMap<>();

    @Override
    void before(String scriptId, Binding binding) {
        // 记录开始时间
        startTimeMap.put(scriptId, System.currentTimeMillis());
    }

    @Override
    void after(String scriptId) {
        // 计算耗时并清理
        Long startTime = startTimeMap.remove(scriptId);
        if (startTime != null) {
            long cost = System.currentTimeMillis() - startTime;
            log.info("【性能监控】scriptId=" + scriptId + ", 执行耗时=" + cost + "ms");
            // 可扩展：将耗时存入监控系统（如Prometheus、ELK）
        }
    }
}
```

```java
    private static List<Singleton> scriptAspects = new ArrayList<>();

    public static void registerAspect(String scriptId){
    try {
            Class<?> clazz = scriptEngine.loadScriptByName(scriptId);
            if(clazz==null) {
                log.warn("registerAspect fail, scriptId:{}",scriptId);
                return;
            }
            scriptAspects.add(new Singleton(scriptId,clazz.getName()));
            log.info("registerAspect scriptId:{} className:{}",scriptId,clazz.getName());
        } catch (ResourceException e) {
            throw new RuntimeException(e);
        } catch (ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    public static Object executeScript(String scriptId, Map<String, Object> parameters, boolean cacheScript) {
        List<GroovyScriptAspect> matchedAspects = null;
        try {
            Binding binding = new Binding();
            // 绑定参数
            if (parameters != null) {
                parameters.forEach(binding::setVariable);
            }
            Script script = createScript(scriptId, binding, cacheScript);
            if(script==null){
                throw new RuntimeException("script create fail, script name: " + scriptId);
            }
            if(properties.getIsDebug()) {
                // 只有通过loadScriptByName获取类才会检测脚本变更
                matchedAspects = scriptAspects.stream().map(aspect -> (GroovyScriptAspect) newObjectByScriptId(aspect.scriptId, properties.getIsCache())).filter(aspect -> aspect.matches(scriptId)).collect(Collectors.toList());
            }else{
                // 使用newObject不会检测脚本变更
                matchedAspects = scriptAspects.stream().map(aspect -> (GroovyScriptAspect) newObject(aspect.className, properties.getIsCache())).filter(aspect -> aspect.matches(scriptId)).collect(Collectors.toList());
            }
            // 1. 前置通知：所有切面执行before
            matchedAspects.forEach(aspect -> aspect.before(scriptId, binding));
            // 执行脚本
            Object rs =  script.run();
            // 3. 后置返回通知：所有切面执行afterReturning
            matchedAspects.forEach(aspect -> aspect.afterReturning(scriptId, rs));
            return rs;
        } catch (Exception e) {
            // 4. 异常通知：所有切面执行afterThrowing
            matchedAspects.forEach(aspect -> aspect.afterThrowing(scriptId, e));
            throw new RuntimeException("Failed to execute script: " + scriptId, e);
        }finally {
            // 5. 最终通知：所有切面执行after（无论是否异常）
            matchedAspects.forEach(aspect -> aspect.after(scriptId));
        }
    }
```


### feign

代替feign的方法是使用restTemplate，添加`@LoadBalanced`注解后restTemplate有服务发现能力。

一体化架构（可以微服务部署也可以单体部署）可以做一个调用接口传入脚本ID，当微服务时用restTemplate调用，一体化部署时调用本地运行接口。

用枚举封装类似Api接口的对象，调用示例

```groovy
DemoApi.Demo1Script.call(binding.variables)


enum DemoApi {
    Demo1Script("test-server1","com/demo/product1/module1/controller/Demo1Script")

    private String serverName;
    private String sid;

    DemoApi(String serverName, String sid) {
        this.serverName = serverName;
        this.sid = sid;
    }

    public <T> T call(Map<String, Object> params){
        return ApiCall.call(serverName,sid,params);

    }
}


```

``` java
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
public class RestTemplateConfig {

    @Bean
    @LoadBalanced
    public RestTemplate loadBalancedRestTemplate() {
        return new RestTemplate();
    }

    @Bean
    public RestTemplate restTemplate() {
        return new RestTemplate();
    }
}


class ApiCall {

    static def call(String serverName,String sid, Map<String, Object> params) {
        // serverName的根据配置确认是走本地还是远程调用
        Environment env = GE.getBean(Environment.class)
        boolean enable = env.getProperty( "meta.groovy.cloud.server.${serverName}.enabled", Boolean.class, false);
        if(!enable){
            // 非微服务模式直接按本地脚本调用
            return MetaGroovyEngine.executeScript(sid+".groovy",params)
        }
        // 需要用添加@LoadBalanced注解的RestTemplate发起调用
        boolean loadBalanced = env.getProperty( "meta.groovy.cloud.server.${serverName}.loadBalanced", Boolean.class, false);
        RestTemplate restTemplate = GE.getBean(loadBalanced?"loadBalancedRestTemplate":"restTemplate",RestTemplate.class);
        String serverUrl = env.getProperty("meta.groovy.cloud.server.${serverName}.url", String.class);
        String url = "${serverUrl}/${sid}"
        // 这里做下post 请求
        return restTemplate.postForObject(url, params, Map.class)
    }
}

meta:
  groovy:
    cloud:
      server:
        test-server1:
          enabled: true
          loadBalanced: false
          url: http://127.0.0.1:9998/run
```

### mybatis mapper

mybatis相关代码不支持动态更新，添加mapper时需要添加GroovyClassLoader解释类文件。


使用方式

```groovy
BusConfigMapper mapper = GroovyMapperUtils.getMapper(BusConfigMapper.class)
BusConfig bean = mapper.selectById(binding.variables.id)
return  bean


class GroovyMapperUtils {

    static <T> T getMapper(Class<T> clazz) {
        SqlSessionTemplate ss = GE.getBean(SqlSessionTemplate)
        if(ss.getConfiguration().hasMapper(clazz)) {
            return ss.getMapper(clazz)
        }
        Resources.setDefaultClassLoader(GE.getGroovyClassLoader())
        ss.getConfiguration().addMapper(clazz)
        return ss.getMapper(clazz)
    }
}
```

```groovy
@Mapper
public interface BusConfigMapper {

    BusConfig selectById(@Param("id") String id);
}


package com.onekbase.groovy.scripts.entity;

class BusConfig {

     String ckey;
     String value;

}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE mapper PUBLIC "-//mybatis.org//DTD Mapper 3.0//EN"
        "http://mybatis.org/dtd/mybatis-3-mapper.dtd">

<mapper namespace="com.demo.product1.module1.dao.mapper.BusConfigMapper">

    <!-- 结果映射 -->
    <resultMap id="BusConfigResultMap" type="com.demo.product1.module1.dao.entity.BusConfig">
        <id property="ckey" column="ckey"/>
        <result property="value" column="value"/>
    </resultMap>

    <!-- 查询方法 -->
    <select id="selectById" parameterType="string" resultMap="BusConfigResultMap">
        SELECT ckey, value
        FROM bus_config
        WHERE ckey = #{id}
    </select>

</mapper>

```

### 动态mapper

用枚举封装类似mapper的对象以实现修改后能动态更新，调用示例

```groovy
List<BusConfig> beanList = DemoMapper.QueryBusConfig.select(binding.variables)


enum DemoMapper {
    QueryBusConfig(Test1GSql)

    private final Class<?> clazz

    // 添加构造函数
    DemoMapper(Class<?> clazz) {
        this.clazz = clazz
    }

    <T> T select(Map<String, Object> params) {
        return ((GSql)GE.newObject(clazz)).select(params)
    }

    <T> T execute(Map<String, Object> params) {
        return ((GSql)GE.newObject(clazz)).execute(params)
    }

}

class Test1GSql implements GSql {

    private static final Logger log = LoggerFactory.getLogger(Test1GSql.class);

    @Override
    String sql(Map<String, Object> params) {
        return "SELECT ckey, value FROM bus_config WHERE ckey = '${params.id}'"
    }

    @Override
    void afterReturning(String sqlStr, Map<String, Object> params, Object gsr) {
        log.info("Test1GSql 执行结果：{} 在获取数据后处理国际化等逻辑",gsr)
    }
}
```

GSql

```groovy
public interface GSql extends GSqlAspect {

     default <T> List<T> select(Map<String, Object> params){
        return GSqlExecutor.select(this,params)
    }

    default int execute(String sql, Map<String, Object> params){
        return GSqlExecutor.select(this,params)
    }

    public abstract String sql(Map<String, Object> params);
}


public interface GSqlAspect {

    default boolean matches(String scriptId) {
        return true;
    }

    default void before(Map<String, Object> params) {}

    default void afterReturning(String sqlStr, Map<String, Object> params, Object gsr) {}

    default void afterThrowing(String scriptId, Throwable throwable) {}

    default void after(String scriptId) {}
}
```

GSqlExecutor

```groovy
public class GSqlExecutor {

    private static final Logger log = LoggerFactory.getLogger(GSqlExecutor.class);

    static final List<String> scriptAspects = new ArrayList<>();

    public static <T> List<T> select(GSql gsql,Map<String, Object> params){
        return execute(gsql,params, true);
    }

    public static int executeSql(GSql gsql,Map<String, Object> params){
        return execute(gsql,params, false);
    }
    private static <T> T execute(GSql gsql,Map<String, Object> params,boolean isSelect){
        String className = gsql.getClass().getName();
        List<GSqlAspect> matchedAspects = null;
        try {
            matchedAspects = scriptAspects.stream().map(aspectId -> (GSqlAspect) GE.newObject(aspectId)).filter(aspect -> aspect.matches(className)).collect(Collectors.toList());
            matchedAspects.forEach(aspect -> aspect.before(params));
            gsql.before(params);
            String sqlStr = gsql.sql(params);
            T gsr = (T)(isSelect?GMapperUtils.select(sqlStr,params):GMapperUtils.executeSql(sqlStr,params));
            gsql.afterReturning(sqlStr,params, gsr);
            matchedAspects.forEach(aspect -> aspect.afterReturning(sqlStr,params, gsr));
            return gsr;
        } catch (Exception e) {
            log.error("Failed to execute sql: {}" , className, e);
            gsql.afterThrowing(className, e);
            matchedAspects.forEach(aspect -> aspect.afterThrowing(className, e));
            throw new RuntimeException("Failed to execute sql: " + className, e);
        }finally {
            gsql.after(className);
            matchedAspects.forEach(aspect -> aspect.after(className));
        }
    }

}

```

```groovy
class GMapperUtils {

    static <T> T getMapper(Class<T> clazz) {
        SqlSessionTemplate ss = MetaGroovyEngine.getBean(SqlSessionTemplate)
        if(ss.getConfiguration().hasMapper(clazz)) {
            return ss.getMapper(clazz)
        }
        ss.getConfiguration().addMapper(clazz)
        return ss.getMapper(clazz)
    }

    static List<Map<String, Object>> select(String sql,Map<String, Object> params){
        CommonSqlMapper mapper = getMapper(CommonSqlMapper.class)
        return mapper.executeSelect(sql,params)
    }

    static int executeSql(String sql, Map<String, Object> params){
        CommonSqlMapper mapper = getMapper(CommonSqlMapper.class)
        return mapper.executeSql(sql,params)
    }
}

@Mapper
interface CommonSqlMapper {

    List<Map<String, Object>> executeSelect(@Param("sql") String sql, @Param("params") Map<String, Object> params)

    int executeSql(@Param("sql") String sql, @Param("params") Map<String, Object> params)
}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE mapper PUBLIC "-//mybatis.org//DTD Mapper 3.0//EN"
        "http://mybatis.org/dtd/mybatis-3-mapper.dtd">

<mapper namespace="com.onekbase.groovy.scripts.core.sql.CommonSqlMapper">

    <!-- 通用查询方法，返回 Map 列表 -->
    <select id="executeSelect" parameterType="map" resultType="map">
        ${sql}
    </select>

    <!-- 通用方法 -->
    <update id="executeSql" parameterType="map">
        ${sql}
    </update>

</mapper>
```







## idea 开发环境配置

IDEA 默认对独立的 Groovy 脚本缺少 SpringBoot 相关类的提示，需要通过以下配置解决：

步骤 1：将 scripts 目录标记为 "Sources Root"

右键项目根目录下的scripts目录 → Mark Directory as → Sources Root

这样 IDEA 会将该目录视为源码目录，参与类路径扫描

步骤 2：配置 Groovy SDK

打开File → Project Structure → SDKs

点击+号 → 选择Groovy SDK → 选择与项目依赖版本一致的 Groovy SDK（如 3.0.17）

应用并保存

步骤 3：添加项目依赖到 Groovy 脚本的类路径

打开File → Project Structure → Modules

选择当前 SpringBoot 模块 → 切换到Dependencies标签

点击+号 → Module Dependency → 选择当前模块的main源码目录

确保Scope设置为Compile，这样 Groovy 脚本就能访问 SpringBoot 项目中的所有类