# groovy

## 和Java语法兼容性

Groovy **并非完全兼容所有Java语法**，但对绝大多数常规Java代码具备高度兼容性；直接将Java文件改后缀为`.groovy`，**大部分场景下可行**，但存在少数边界情况会报错或行为不一致。以下分维度详细说明：

**核心结论**

| 场景                | 是否可行？ | 关键说明                                                                 |
|---------------------|------------|--------------------------------------------------------------------------|
| 常规Java代码（POJO、Service、工具类） | ✅ 可行    | 无特殊语法的Java代码，改后缀后可直接运行                                 |
| 包含Java 8+特性（Lambda、Stream） | ✅ 可行    | Groovy完全支持Java 8+的核心语法特性                                     |
| 包含极端语法（如匿名内部类特殊写法、泛型边界） | ⚠️ 部分可行 | 少数语法需微调                                                           |
| 依赖Java编译器严格校验的场景（如注解处理器、泛型擦除细节） | ❌ 不可行 | Groovy编译器规则与javac有差异，可能触发兼容问题                         |

**完全兼容的核心语法**

Groovy是JVM语言，**基于Java语法做了扩展**，以下Java代码改后缀后可直接运行：
- 类/方法/变量定义（`public/private/protected`、`static/final`等）；
- 控制流（`if/else`、`for/while`、`switch`、`try/catch`）；
- 面向对象（继承`extends`、实现`implements`、构造器、重载）；
- Java 8+特性（Lambda表达式、Stream API、方法引用、默认方法）；
- 注解（`@Autowired`、`@Service`等Spring注解完全兼容）。

**示例：Java代码直接改Groovy**
```java
// UserService.java → 改后缀为UserService.groovy，完全可运行
public class UserService {
    private String name;
    
    public UserService(String name) {
        this.name = name;
    }
    
    public String getName() {
        return name;
    }
    
    public static void main(String[] args) {
        UserService service = new UserService("test");
        System.out.println(service.getName());
    }
}
```

**存在兼容问题的边界场景**

以下Java语法直接改Groovy会报错或行为异常，需微调：

| 问题类型                | Java示例                          | Groovy兼容问题                          | 修复方案                                  |
|-------------------------|-----------------------------------|-----------------------------------------|-------------------------------------------|
| 分号省略的歧义          | `int a = 1; int b = 2;`           | Groovy允许省略分号，但多行合并可能报错   | 保留分号（或按Groovy规范分行）            |
| 字符字面量的处理        | `char c = 'a';`                   | Groovy中`'a'`默认是String，非char       | 显式声明：`char c = 'a' as char`          |
| 空指针判断的语法冲突    | `if (obj == null)`                | Groovy中`==`等价于Java的`equals`，`null`判断需用`is` | `if (obj.is(null))` 或 `if (obj == null)`（Groovy 3+已兼容） |
| 匿名内部类的写法        | `new Runnable() { public void run() {} }` | Groovy对匿名内部类的语法解析更严格      | 改用Lambda（`() -> {}`）或调整括号格式    |
| 泛型擦除的细节差异      | `List<String> list = new ArrayList<>();` | Groovy泛型校验更宽松，可能隐藏类型问题  | 显式指定泛型：`List<String> list = new ArrayList<String>()` |
| 静态导入的冲突          | `import static java.lang.Math.*;` | Groovy静态导入优先级与Java略有差异      | 显式指定类名（如`Math.abs()`）            |
| 关键字冲突              | 变量名用`def`/`in`等Groovy关键字  | Java中`def`不是关键字，Groovy中是        | 重命名变量（如`def`→`defVal`）            |

**Groovy的语法扩展（不影响Java兼容）**

Groovy在Java基础上增加了便捷语法（这些语法Java不支持，但Java代码不会触发）：
- 省略`return`（方法最后一行自动返回）；
- 省略分号；
- 动态类型（`def`关键字）；
- 集合字面量（`def list = [1,2,3]`）；
- 闭包（`{ param -> ... }`）；
- 运算符重载（`a + b`可自定义）。

**“改后缀”的实操注意事项**

1. 编译/运行环境
- Groovy文件需用`groovyc`编译器（或Spring Boot的`groovy-maven-plugin`），而非`javac`；
- Spring Boot中引入Groovy依赖后，可自动识别`.groovy`文件：
  ```xml
  <!-- pom.xml引入Groovy依赖 -->
  <dependency>
      <groupId>org.codehaus.groovy</groupId>
      <artifactId>groovy-all</artifactId>
      <version>3.0.17</version>
      <type>pom</type>
  </dependency>
  ```

1. IDE兼容
- IntelliJ IDEA/Eclipse对Groovy的语法提示、断点调试完全支持，改后缀后无需额外配置；
- 若代码中使用了Java的注解处理器（如Lombok），需确保Groovy编译器兼容（Lombok对Groovy支持有限，建议慎用）。

1. 性能与字节码
- Groovy编译器生成的字节码与Java类似，常规场景下性能无显著差异；
- Groovy的动态特性（如元编程）会带来少量性能开销，但纯Java语法的Groovy代码无此问题。

**总结**

1. **常规Java代码**：直接改后缀为`.groovy`完全可行，无需修改；
2. **特殊语法场景**：需微调（如char声明、null判断、匿名内部类），但改动量极小；
3. **核心建议**：
   - 若只是将Java代码迁移为Groovy（无动态脚本需求），改后缀即可；
   - 若要利用Groovy的动态特性，可在兼容Java语法的基础上逐步扩展；
   - 若依赖Java编译器的严格校验（如泛型、注解处理器），建议保留`.java`后缀，Groovy与Java混合编译（Spring Boot支持混合编译）。

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
