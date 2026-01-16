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

```java
    private static Map<String, Singleton> singletonCache = new HashMap<>();

    public static Object executeScript(String scriptName, Map<String, Object> parameters, boolean cacheScript) {
        try {
            Binding binding = new Binding();
            // 绑定参数
            if (parameters != null) {
                parameters.forEach(binding::setVariable);
            }
            Script script = createScript(scriptName, binding, cacheScript);
            if(script==null){
                throw new RuntimeException("script create fail, script name: " + scriptName);
            }
            // 执行脚本
            Object rs =  script.run();
            return rs;
        } catch (Exception e) {
            throw new RuntimeException("Failed to execute script: " + scriptName, e);
        }
    }

    public static Script createScript(String scriptId,Binding binding,boolean cache) {
        try {
            Class<?> clazz = scriptEngine.loadScriptByName(scriptId);
            if(clazz==null) return null ;
            String className = clazz.getName();
            if(!cache){
                singletonCache.remove(className);
                return InvokerHelper.createScript(clazz, binding);
            }
            // 没有缓存过，直接生成并缓存，不用考虑线程安全，并发创建多个缓存一个就行
            if(singletonCache.get(className)==null){
                Script o = InvokerHelper.createScript(clazz, binding);
                singletonCache.put(className,new Singleton(scriptId,className,clazz,o));
                log.info("create script :"+className);
                return o;
            }
            // scriptEngine没有重新编译脚本且缓存过对象，直接返回缓存对象
            if(clazz==singletonCache.get(className).clazz){
                return (Script)singletonCache.get(className).singleton;
            }
            // 缓存过对象，但scriptEngine重新编译了脚本，重新生成对象并缓存
            // 不用考虑线程安全，并发创建多个缓存一个就行
            singletonCache.remove(className);
            Script o = InvokerHelper.createScript(clazz, binding);
            singletonCache.put(className,new Singleton(scriptId,className,clazz,o));
            log.info("recreate script :"+className);
            return o;
        } catch (ScriptException e) {
            throw new RuntimeException(e);
        } catch (ResourceException e) {
            throw new RuntimeException(e);
        }
    }

    public static Object newObject(String className) {
        return newObject(className,properties.getIsCache());
    }
    public static Object newObjectByScriptId(String scriptId,boolean cache) {
        try {
            Class<?> clazz =  scriptEngine.loadScriptByName(scriptId);
            return newObjectByClass(clazz,cache);
        } catch (ResourceException | ScriptException e) {
            throw new RuntimeException(e);
        }
    }

    public static Object newObject(String className,boolean cache) {
        try {
            Class<?> clazz = scriptEngine.getGroovyClassLoader().loadClass(className);
            return newObjectByClass(clazz,cache);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static Object newObjectByClass(Class<?> clazz,boolean cache) {
        try {
            if(clazz==null) return null ;
            String className = clazz.getName();
            if(!cache){
                singletonCache.remove(className);
                return clazz.newInstance();
            }
            // 没有缓存过，直接生成并缓存，不用考虑线程安全，并发创建多个缓存一个就行
            if(singletonCache.get(className)==null){
                Object o = clazz.newInstance();
                singletonCache.put(className,new Singleton(clazz,o));
                log.info("new instance :"+className);
                return o;
            }
            // scriptEngine没有重新编译脚本且缓存过对象，直接返回缓存对象
            Class<?> cacheClass =singletonCache.get(className).clazz;
            if(clazz==cacheClass){
                return singletonCache.get(className).singleton;
            }
            // 缓存过对象，但scriptEngine重新编译了脚本，重新生成对象并缓存
            // 不用考虑线程安全，并发创建多个缓存一个就行
            singletonCache.remove(className);
            Object o = clazz.newInstance();
            singletonCache.put(className,new Singleton(clazz,o));
            log.info("renew instance :"+className);
            return o;
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }
```

调用示例

```groovy
package com.onekbase.groovy.scripts.demo

import com.onekbase.framework.groovy.engine.MetaGroovyEngine
import com.onekbase.groovy.scripts.demo.User
import com.onekbase.groovy.scripts.demo.UserDao

class UserService {
    List<User> users = []

    UserDao userDao = MetaGroovyEngine.newObject("com.onekbase.groovy.scripts.demo.UserDao")
    UserDao userDaoNew = new UserDao()

    void addUser(User user) {
        users.add(user)
        println "Added user: $user"
        userDao.addUser(user)
        userDaoNew.addUser(user)
    }

    List<User> getAllUsers() {
        return users
    }

    User findUserByName(String name) {
        return users.find { it.name == name }
    }
}
```

### spring aop

aop是为了面向切面编程，在动态脚本中此类需求可以将功能实现为包装脚本的run接口口，在run方法前后执行切面逻辑。

只能对执行的脚本做切面，不能对脚本内调用的类方法做切面，所以脚本内调用的其他功能需要切面需要将被调用方法做成脚本并用执行脚本接口调用。

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

切面接口

```java
package com.onekbase.framework.groovy.engine;

import groovy.lang.Binding;


/**
 * Groovy脚本执行的AOP切面接口
 */
public interface GroovyScriptAspect {
    /**
     * 判断当前切面是否适用于指定脚本
     * @param scriptId 脚本唯一标识（如PAY_001、ORDER_CALC、TEST_002）
     * @return true=切面生效，false=切面跳过
     */
    default boolean matches(String scriptId) {
        // 默认实现：所有脚本都生效（兼容原有逻辑）
        return true;
    }
    /**
     * 前置通知：脚本执行前调用
     * @param scriptId 脚本唯一标识
     * @param binding 脚本执行入参
     */
    default void before(String scriptId, Binding binding) {}

    /**
     * 后置返回通知：脚本正常执行后调用
     * @param scriptId 脚本唯一标识
     * @param result 脚本执行结果
     */
    default void afterReturning(String scriptId, Object result) {}

    /**
     * 异常通知：脚本执行异常时调用
     * @param scriptId 脚本唯一标识
     * @param throwable 异常信息
     */
    default void afterThrowing(String scriptId, Throwable throwable) {}

    /**
     * 最终通知：无论是否异常，脚本执行完成后调用（类似finally）
     * @param scriptId 脚本唯一标识
     */
    default void after(String scriptId) {}
}
```

切面示例

```groovy
package com.onekbase.groovy.scripts.aspect;

import com.onekbase.framework.groovy.engine.GroovyScriptAspect;
import groovy.lang.Binding;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * 脚本执行日志切面
 */
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


package com.onekbase.groovy.scripts.aspect;

import com.onekbase.framework.groovy.engine.GroovyScriptAspect;
import groovy.lang.Binding
import org.slf4j.Logger
import org.slf4j.LoggerFactory;

import java.util.concurrent.ConcurrentHashMap;

/**
 * 脚本执行性能监控切面（Order=2，在日志切面之后执行）
 */
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

### feign

代替feign的方法是使用restTemplate，添加`@LoadBalanced`注解后restTemplate有服务发现能力。

一体化架构（可以微服务部署也可以单体部署）可以做一个调用接口传入脚本ID，当微服务时用restTemplate调用，一体化部署时调用本地运行接口。

``` java
import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
public class RestTemplateConfig {

    /**
     * 配置支持 Nacos 服务发现的 RestTemplate
     * @LoadBalanced 注解是核心：开启负载均衡 + 服务名解析
     */
    @Bean
    @LoadBalanced // 必须添加这个注解
    public RestTemplate restTemplate() {
        return new RestTemplate();
    }
}

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
public class DemoController {

    @Autowired
    private RestTemplate restTemplate;

    /**
     * 调用 Nacos 中的 user-service 服务
     * 注意：URL 中使用「服务名」而非 IP:端口
     */
    @GetMapping("/call/user/{id}")
    public String callUserService(@PathVariable Long id) {
        // 核心：URL 中的 user-service 是 Nacos 中注册的服务名，而非具体 IP
        String url = "http://user-service/user/" + id;
        // RestTemplate 会自动通过 Nacos 解析 user-service 为具体的实例地址（如 192.168.1.100:8080）
        return restTemplate.getForObject(url, String.class);
    }
}
```

### mybatis

mybatis相关代码不支持动态更新，xml文件加载时相关entity类发现不了（类加载器问题），可以简单使用mapper类添加注解的方式。

```groovy
package com.onekbase.groovy.scripts.mapper

import com.onekbase.groovy.scripts.entity.BusConfig
import org.apache.ibatis.annotations.Delete
import org.apache.ibatis.annotations.Insert;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param
import org.apache.ibatis.annotations.Select
import org.apache.ibatis.annotations.Update;

@Mapper
public interface BusConfigMapper {
    @Select("SELECT ckey, value FROM bus_config2 WHERE ckey = #{id}")
    BusConfig selectById(@Param("id") String id);

    @Insert("INSERT INTO bus_config (ckey, value) VALUES (#{ckey}, #{value})")
    int insert(BusConfig config);

    @Update("UPDATE bus_config SET value = #{value} WHERE ckey = #{ckey}")
    int update(BusConfig config);

    @Delete("DELETE FROM bus_config WHERE ckey = #{ckey}")
    int delete(@Param("ckey") String ckey);
}


package com.onekbase.groovy.scripts.entity;

class BusConfig {

     String ckey;
     String value;

}
```

使用方式

```groovy

package com.onekbase.groovy.scripts.core

import com.onekbase.framework.groovy.engine.MetaGroovyEngine
import org.mybatis.spring.SqlSessionTemplate

class GroovyMapperUtils {

    static <T> T getMapper(Class<T> clazz) {
        SqlSessionTemplate ss = MetaGroovyEngine.getBean(SqlSessionTemplate)
        if(ss.getConfiguration().hasMapper(clazz)) {
            return ss.getMapper(clazz)
        }
        ss.getConfiguration().addMapper(clazz)
        return ss.getMapper(clazz)
    }
}


package com.onekbase.groovy.scripts.demo

import com.onekbase.groovy.scripts.core.GroovyMapperUtils
import com.onekbase.groovy.scripts.entity.BusConfig
import com.onekbase.groovy.scripts.mapper.BusConfigMapper

BusConfigMapper mapper = GroovyMapperUtils.getMapper(BusConfigMapper.class)
BusConfig bean = mapper.selectById(binding.variables.id)
return  bean
```

### 数据层

```groovy
package com.onekbase.groovy.scripts.core.sql

import com.onekbase.framework.groovy.engine.MetaGroovyEngine
import org.mybatis.spring.SqlSessionTemplate

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
        CommonSqlMapper mapper = GMapperUtils.getMapper(CommonSqlMapper.class)
        return mapper.executeSelect(sql,params)
    }

    static int executeSql(String sql, Map<String, Object> params){
        CommonSqlMapper mapper = GMapperUtils.getMapper(CommonSqlMapper.class)
        return mapper.executeSql(sql,params)
    }
}

package com.onekbase.groovy.scripts.core.sql

import org.apache.ibatis.annotations.Mapper
import org.apache.ibatis.annotations.Param

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

```groovy
package com.onekbase.groovy.scripts.core.sql;

import com.onekbase.framework.groovy.engine.MetaGroovyEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
            matchedAspects = scriptAspects.stream().map(aspectId -> (GSqlAspect) MetaGroovyEngine.newObject(aspectId)).filter(aspect -> aspect.matches(className)).collect(Collectors.toList());
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


package com.onekbase.groovy.scripts.core.sql;


import java.util.Map;

public interface GSql extends GSqlAspect {

     default <T> List<T> select(Map<String, Object> params){
        return GSqlExecutor.select(this,params)
    }

    default int execute(String sql, Map<String, Object> params){
        return GSqlExecutor.select(this,params)
    }

    public abstract String sql(Map<String, Object> params);
}

package com.onekbase.groovy.scripts.core.sql;

import java.util.Map;


/**
 * Groovy脚本执行的AOP切面接口
 */
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

```groovy
package com.onekbase.groovy.scripts.dao

import com.onekbase.groovy.scripts.core.sql.GSql
import org.slf4j.Logger
import org.slf4j.LoggerFactory

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

```groovy
package com.onekbase.groovy.scripts.demo

import com.onekbase.groovy.scripts.dao.Test1GSql
import com.onekbase.groovy.scripts.entity.BusConfig

List<BusConfig> beanList = new Test1GSql().select(binding.variables)
return  beanList
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