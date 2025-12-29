# groovy语言


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

## 闭包

### 一、 先明确：Closure的核心定义
Groovy中的Closure是**一段可传递、可复用的代码块**，它可以独立存在，也可以被赋值给变量、作为方法参数传递、作为返回值返回，同时还能捕获和持有其定义上下文的变量（即闭包具有“词法作用域”），本质是JVM上的一种特殊对象（实现了`groovy.lang.Closure`接口）。

简单示例（直观感受Closure的形态）：
```groovy
// 定义一个闭包：计算两个数的和
def sumClosure = { a, b -> a + b }
// 调用闭包
def result = sumClosure(3, 5)
println result // 输出 8
```

### 二、 Closure的核心作用（核心功能）

**1.  作为“可传递的代码块”：实现回调与行为参数化**

这是Closure最基础也最常用的作用，它允许将一段业务逻辑（行为）作为参数传递给方法，让方法的逻辑更灵活、可定制，无需重复编写相似代码（即“行为参数化”），这也是相比Java早期版本（无Lambda）的核心优势。

- 解决的问题：避免为不同的小逻辑编写多个重载方法或实现类（如Java中的`Runnable`、`Comparator`等匿名内部类）；
- 典型场景：集合遍历、排序、过滤等。

示例1：集合遍历（行为作为参数传递）
```groovy
def list = [1, 2, 3, 4, 5]

// 将“打印元素*2”的逻辑作为闭包传递给each方法
list.each { it -> println it * 2 } 
// 简化写法（默认参数it，无需显式声明）
list.each { println it * 3 }

// 对比Java（Java 8+ Lambda本质是类似思想，但Groovy闭包更灵活）
// list.forEach(item -> System.out.println(item * 2));
```

示例2：自定义方法接收闭包参数（实现回调逻辑）
```groovy
// 自定义方法：执行业务操作，并在操作前后调用回调闭包
def doBusiness(String businessName, Closure before, Closure after) {
    before.call(businessName) // 执行前置回调
    println "正在执行【${businessName}】业务逻辑..."
    after.call(businessName)  // 执行后置回调
}

// 传递闭包作为回调，灵活定制前置/后置行为
doBusiness("用户下单", 
    { name -> println "前置校验：${name}业务参数合法性" }, // 前置闭包
    { name -> println "后置处理：${name}业务日志记录" }  // 后置闭包
)
```

**2.  实现代码复用与逻辑封装**

Closure可以将一段通用逻辑封装成独立的代码块，通过赋值给变量或提取为公共方法返回值，实现多处复用，简化代码冗余。

- 优势：相比普通方法，Closure更轻便（无需定义类结构）、更灵活（可按需调整上下文）；
- 典型场景：通用计算逻辑、数据格式化逻辑等。

示例：封装通用格式化逻辑
```groovy
// 封装日期格式化闭包（复用逻辑）
def dateFormatClosure = { Date date, String pattern ->
    new SimpleDateFormat(pattern).format(date)
}

// 多处复用该闭包
def now = new Date()
def ymdFormat = dateFormatClosure(now, "yyyy-MM-dd")
def hmsFormat = dateFormatClosure(now, "HH:mm:ss")

println "年月日格式：${ymdFormat}"
println "时分秒格式：${hmsFormat}"
```

**3.  支撑层级化结构：构建DSL的块级语义基础**

Closure是Groovy DSL实现层级化、嵌套式结构的核心支撑，没有Closure就无法实现DSL的块级语法（如Gradle的`dependencies { ... }`、`repositories { ... }`）。

- 核心原理：将Closure作为方法的参数，通过嵌套传递Closure，实现DSL的层级关系（贴合业务领域的层级结构，如“订单-商品-规格”）；
- 关键价值：让DSL代码结构清晰、接近自然语言，非技术人员也能理解。

示例（DSL层级结构实现）：
```groovy
// 顶层DSL方法：接收闭包构建应用配置
def appConfig(Closure closure) {
    println "开始构建应用配置..."
    closure.call() // 执行顶层闭包
}

// 二级DSL方法：接收闭包构建数据源配置
def dataSource(Closure closure) {
    println "  开始构建数据源配置..."
    closure.call() // 执行二级闭包
}

// 使用DSL（嵌套闭包实现层级结构，完全贴合业务逻辑）
appConfig {
    println "  应用名称：user-service"
    println "  端口：8080"
    // 嵌套闭包，实现二级结构
    dataSource {
        println "    数据库地址：localhost:3306"
        println "    用户名：root"
    }
}
```

**4.  词法作用域与变量捕获：持有上下文状态**

Closure具有**词法作用域**特性，即它可以捕获并持有其定义时所在上下文的变量（即使脱离了该上下文，闭包仍能访问和修改这些变量），这是Closure实现状态保持的关键。

- 核心特性：
  1.  闭包可以访问定义它的外部作用域的变量；
  2.  闭包可以修改外部作用域的变量（无需额外声明，直接操作）；
  3.  即使外部作用域执行完毕，闭包仍能持有这些变量的引用。

示例：变量捕获与状态保持
```groovy
def outerVar = 10 // 外部作用域变量

// 定义闭包，捕获outerVar
def closure = {
    outerVar++ // 修改外部变量
    println "闭包内：outerVar = ${outerVar}"
}

// 调用闭包
closure() // 输出：闭包内：outerVar = 11
closure() // 输出：闭包内：outerVar = 12

// 外部变量已被闭包修改
println "闭包外：outerVar = ${outerVar}" // 输出：闭包外：outerVar = 12
```

### 三、 Closure的关键特性

上述作用的实现，依赖于Closure的几个独特特性，这些特性也是它区别于Java Lambda的关键：

**1.  闭包委托（Delegate）：DSL的灵魂**

这是Groovy Closure最核心的特性，也是实现DSL语义分发、上下文隔离的关键（前文DSL示例的核心支撑）。
- 作用：可以将闭包内的方法调用、属性访问，转发到指定的`delegate`对象上，而非闭包的所有者或包含者；
- 配置方式：通过`closure.delegate = 委托对象`绑定委托，通过`closure.resolveStrategy`设置解析策略（如`Closure.DELEGATE_FIRST`：优先使用委托对象的方法）；
- 价值：实现DSL的“语法与逻辑分离”，避免方法冲突，让DSL更灵活、可扩展。

**2.  默认参数（it）：简化代码**

当Closure没有显式声明参数时，Groovy会自动提供一个默认参数`it`，代表传入的唯一参数，大幅简化单参数闭包的写法。

示例：
```groovy
def list = [1, 2, 3]
// 显式声明参数
list.each { num -> println num * 2 }
// 使用默认参数it，简化写法
list.each { println it * 2 }
```

**3.  灵活的调用方式**

Closure支持多种调用方式，适配不同场景：
```groovy
def closure = { a, b -> a + b }
// 标准调用方式
def result1 = closure(3, 5)
// 使用call方法调用（适合闭包可能为null的场景，可配合安全导航操作符：closure?.call(3,5)）
def result2 = closure.call(3, 5)
println result1 == result2 // 输出 true
```


### 总结

Closure的典型实际应用

1.  **DSL构建**：这是Closure最核心的应用场景（Gradle构建脚本、Spock测试框架、自定义业务DSL等），通过闭包的嵌套和委托机制，实现简洁可读的领域特定语言；
2.  **集合操作**：Groovy集合的`each`、`find`、`filter`、`sum`、`sort`等方法，均接收闭包作为参数，实现灵活的数据处理；
3.  **回调函数**：在异步编程、事件处理中，用闭包作为回调逻辑，简化代码（如文件读写的回调、网络请求的回调等）；
4.  **代码延迟执行**：将闭包作为参数传递，在需要时才调用`call()`方法执行，实现逻辑的延迟执行（如缓存逻辑、懒加载逻辑等）。
   
Closure的核心作用可概括为4点：

1.  **行为传递**：作为可传递的代码块，实现回调与行为参数化，让方法逻辑更灵活；
2.  **逻辑复用**：封装通用逻辑，实现多处复用，简化代码冗余；
3.  **DSL支撑**：通过嵌套结构和委托机制，构建层级化、可读的DSL，是Groovy DSL的核心基石；
4.  **状态持有**：通过词法作用域捕获外部变量，实现上下文状态的保持与修改。

## 闭包代理

### 一、 闭包委托的核心定义

Groovy闭包（Closure）作为一种特殊的代码块对象，内部存在三个关键的作用域对象（决定闭包内方法/属性的查找顺序），而**委托（Delegate）就是其中最灵活、最核心的一个对象**：
1.  **Owner（所有者）**：闭包的定义者（通常是定义闭包的类或另一个闭包），不可手动修改；
2.  **This（当前对象）**：闭包所在的最外层类对象（对应Java中的`this`），不可手动修改；
3.  **Delegate（委托）**：可手动指定的任意对象，是闭包委托的核心，支持动态修改，也是实现“方法转发”的关键。

闭包委托的核心作用：**将闭包内未在自身作用域（Owner/This）找到的方法调用或属性访问，转发（委托）到指定的Delegate对象上**，从而实现“语法与逻辑分离”，让闭包的行为可灵活定制。

### 二、 闭包委托的工作原理：方法/属性的查找规则

闭包内调用一个方法或访问一个属性时，默认会按照固定顺序查找对应的定义，而委托的核心价值在于通过配置改变这一查找顺序，实现灵活的方法分发。

**1. 默认查找顺序（未修改解析策略时）**

`This → Owner → Delegate`
- 第一步：在`This`对象（最外层类）中查找方法/属性；
- 第二步：若未找到，在`Owner`对象（闭包所有者）中查找；
- 第三步：若仍未找到，最后在`Delegate`对象中查找；
- 若三步均未找到，抛出`MissingMethodException`（方法未找到）或`MissingPropertyException`（属性未找到）。

**2. 可配置的解析策略（核心：修改查找优先级）**

Groovy提供了`Closure.resolveStrategy`属性，用于修改上述查找顺序，其中两个策略最常用（尤其在DSL开发中）：
| 解析策略常量                | 查找顺序                  | 核心用途                                  |
|-----------------------------|---------------------------|-------------------------------------------|
| `Closure.DELEGATE_FIRST`     | Delegate → Owner → This    | 优先使用委托对象的方法/属性（DSL开发首选） |
| `Closure.DELEGATE_ONLY`      | 仅在Delegate中查找        | 完全隔离闭包自身作用域，仅依赖委托对象    |
| `Closure.OWNER_FIRST`（默认）| Owner → Delegate → This    | 优先使用所有者对象，兜底使用委托对象      |
| `Closure.THIS_FIRST`（默认） | This → Owner → Delegate    | 优先使用最外层类对象，兜底使用委托对象    |

其中，`Closure.DELEGATE_FIRST`是DSL开发的核心配置，它确保闭包内的方法调用优先匹配委托对象的方法，让DSL语法更纯净，避免与闭包自身作用域的方法冲突。

### 三、 闭包委托的关键配置步骤

使用闭包委托的核心流程只有3步，这也是Groovy DSL实现的标准配置：
1.  **创建委托对象**：封装闭包需要转发的方法/属性（核心：承载业务逻辑）；
2.  **绑定委托对象**：通过`closure.delegate = 委托对象`，将闭包与委托对象关联；
3.  **设置解析策略**：通过`closure.resolveStrategy = 解析策略`（推荐`Closure.DELEGATE_FIRST`），提升委托对象的查找优先级；
4.  **执行闭包**：调用`closure.call()`或`closure()`，触发闭包内的方法转发。

核心代码模板：
```groovy
// 1. 定义委托类（承载业务逻辑）
class MyDelegate {
    def hello(String name) {
        println "Delegate：Hello, ${name}"
    }
}

// 2. 定义闭包
def myClosure = {
    hello("Groovy") // 闭包内未定义hello方法，将转发到Delegate
}

// 3. 配置委托
def delegateObj = new MyDelegate()
myClosure.delegate = delegateObj // 绑定委托对象
myClosure.resolveStrategy = Closure.DELEGATE_FIRST // 优先查找委托对象

// 4. 执行闭包
myClosure.call() // 输出：Delegate：Hello, Groovy
```

### 四、 闭包委托的核心使用场景

**1.  DSL构建（最核心场景）**

这是闭包委托的首要用途，通过委托机制实现DSL的“语法简洁化”和“逻辑封装化”：
- DSL使用者：只需编写接近自然语言的简洁语法（无需关心底层实现）；
- DSL开发者：在委托类中封装所有业务逻辑，实现语法与逻辑的分离，便于维护和扩展。
典型案例：Gradle构建脚本、Spock测试框架的`given-when-then`语法。

**2.  方法分发与上下文隔离**

当多个闭包需要复用不同的业务逻辑时，可通过绑定不同的委托对象，实现方法的动态分发，同时隔离不同闭包的上下文：
- 不同闭包绑定不同委托对象，执行相同的语法会触发不同的业务逻辑；
- 委托对象各自持有独立状态，避免闭包之间的状态污染。

**3.  简化对象配置**

通过委托机制，可以用简洁的块级语法为对象配置属性，替代繁琐的`setter`方法调用，让配置代码更可读。

### 五、 完整可运行示例

#### 示例1：基础委托使用

```groovy
// 1. 定义委托类（封装核心方法）
class GreetingDelegate {
    // 委托对象的核心方法：打招呼
    def sayHello(String name) {
        println "【委托对象】Hello, ${name}！"
    }

    // 委托对象的属性
    String tip = "这是委托对象的提示信息"
}

// 2. 定义外层类（验证This/Owner作用域）
class ClosureDelegateDemo {
    // This对象的方法（与委托对象方法同名，用于验证查找顺序）
    def sayHello(String name) {
        println "【This对象】Hello, ${name}！"
    }

    def demo() {
        // 定义闭包（Owner是ClosureDelegateDemo类）
        def myClosure = {
            sayHello("Groovy") // 调用同名方法
            println "访问属性：${tip}" // 访问委托对象的属性
        }

        // 3. 配置闭包委托
        def greetingDelegate = new GreetingDelegate()
        myClosure.delegate = greetingDelegate // 绑定委托对象

        // 场景A：使用默认解析策略（OWNER_FIRST）
        println "=== 默认解析策略（OWNER_FIRST） ==="
        myClosure.call() // 优先调用This/Owner的sayHello方法

        // 场景B：使用DELEGATE_FIRST解析策略
        println "\n=== 解析策略（DELEGATE_FIRST） ==="
        myClosure.resolveStrategy = Closure.DELEGATE_FIRST
        myClosure.call() // 优先调用委托对象的sayHello方法
    }
}

// 执行示例
new ClosureDelegateDemo().demo()
```

**运行结果**：
```
=== 默认解析策略（OWNER_FIRST） ===
【This对象】Hello, Groovy！
访问属性：这是委托对象的提示信息

=== 解析策略（DELEGATE_FIRST） ===
【委托对象】Hello, Groovy！
访问属性：这是委托对象的提示信息
```

#### 示例2：DSL构建

该示例复现了订单DSL的委托实现，完整展示委托在DSL中的核心作用：

```groovy
// 1. 定义业务模型类（对应订单领域的核心数据结构）
class Goods {
    String name        // 商品名称
    BigDecimal price   // 商品价格
    int quantity       // 购买数量

    // 计算商品总价
    BigDecimal getTotalPrice() {
        return price * quantity
    }
}

class Order {
    String orderNo     // 订单编号
    String buyer       // 买家姓名
    String payType     // 支付方式（微信/支付宝/银行卡）
    List<Goods> goodsList = []  // 订单包含的商品列表

    // 计算订单总金额
    BigDecimal getOrderTotalAmount() {
        return goodsList.sum { it.totalPrice } ?: BigDecimal.ZERO
    }

    // 打印订单详情（辅助展示）
    void printOrderInfo() {
        println "==================== 订单详情 ===================="
        println "订单编号：${orderNo}"
        println "买家姓名：${buyer}"
        println "支付方式：${payType}"
        println "商品列表："
        goodsList.eachWithIndex { goods, index ->
            println "  ${index+1}. 商品：${goods.name}，单价：${goods.price}，数量：${goods.quantity}，小计：${goods.totalPrice}"
        }
        println "订单总金额：${orderTotalAmount}"
        println "=================================================="
    }
}

// 2. 定义DSL委托类（核心：封装DSL的语法方法，处理闭包内的逻辑）
class OrderDslDelegate {
    // 持有订单对象，用于存储DSL配置的数据
    Order currentOrder = new Order()

    // DSL语法1：设置订单编号（对应 createOrder "订单号" { ... } 中的订单号）
    void orderNo(String no) {
        currentOrder.orderNo = no
    }

    // DSL语法2：设置买家姓名（DSL内可直接写 buyer "张三"）
    void buyer(String buyerName) {
        currentOrder.buyer = buyerName
    }

    // DSL语法3：设置支付方式（DSL内可直接写 payType "微信支付"）
    void payType(String type) {
        // 简单校验支付方式
        def validTypes = ["微信支付", "支付宝", "银行卡"]
        if (!validTypes.contains(type)) {
            throw new IllegalArgumentException("无效支付方式：${type}，仅支持${validTypes}")
        }
        currentOrder.payType = type
    }

    // DSL语法4：添加商品（支持嵌套/参数映射，DSL内可写 goods "手机", price: 5999, quantity: 1）
    void goods(Map<String, Object> attrs, String goodsName) {
        def goods = new Goods()
        goods.name = goodsName
        // 从参数映射中获取价格和数量，并做类型转换
        goods.price = new BigDecimal(attrs.price?.toString() ?: "0")
        goods.quantity = attrs.quantity ? (attrs.quantity as int) : 1

        currentOrder.goodsList.add(goods)
    }

    // 获取构建完成的订单对象
    Order getOrder() {
        return currentOrder
    }
}

// 3. 定义DSL入口方法（对外暴露简洁的DSL入口，封装闭包委托逻辑）
/**
 * 订单创建DSL入口方法
 * @param closure 闭包，内部是DSL语法内容
 * @return 构建完成的Order对象
 */
def createOrder(Closure closure) {
    // 创建委托对象
    def orderDelegate = new OrderDslDelegate()
    // 绑定闭包的委托对象为OrderDslDelegate
    closure.delegate = orderDelegate
    // 设置闭包解析策略：优先使用委托对象的方法/属性（DSL核心配置）
    closure.resolveStrategy = Closure.DELEGATE_FIRST
    // 执行闭包，解析DSL语法
    closure.call()

    // 返回构建完成的订单
    return orderDelegate.order
}

// 4. 使用DSL创建订单（核心：简洁、可读、接近自然语言）
def myOrder = createOrder {
    orderNo "202512290001"
    buyer "张三"
    payType "微信支付"
    // 批量添加商品
    goods "旗舰手机", price: 5999.00, quantity: 1
    goods "手机壳", price: 29.90, quantity: 2
    goods "无线充电器", price: 199.00, quantity: 1
}

// 5. 展示订单结果
myOrder.printOrderInfo()

// 扩展：再创建一个订单，验证DSL的灵活性
def anotherOrder = createOrder {
    orderNo "202512290002"
    buyer "李四"
    payType "支付宝"
    goods "笔记本电脑", price: 8999.00, quantity: 1
    goods "无线鼠标", price: 129.00, quantity: 1
}

println "\n" // 换行分隔
anotherOrder.printOrderInfo()
```

**运行结果**：
```
==================== 订单详情 ====================
订单编号：202512290001
买家姓名：张三
支付方式：微信支付
商品列表：
  1. 商品：旗舰手机，单价：5999.00，数量：1，小计：5999.00
  2. 商品：手机壳，单价：29.90，数量：2，小计：59.80
  3. 商品：无线充电器，单价：199.00，数量：1，小计：199.00
订单总金额：6257.80
==================================================


==================== 订单详情 ====================
订单编号：202512290002
买家姓名：李四
支付方式：支付宝
商品列表：
  1. 商品：笔记本电脑，单价：8999.00，数量：1，小计：8999.00
  2. 商品：无线鼠标，单价：129.00，数量：1，小计：129.00
订单总金额：9128.00
==================================================
```

#### 示例3：营销规则

```groovy
// 1. 定义业务模型类：承载营销规则和商品订单数据
// 商品类
class Product {
    String sku        // 商品唯一标识
    String name       // 商品名称
    BigDecimal price  // 商品单价
    int quantity      // 购买数量

    // 商品小计
    BigDecimal getSubtotal() {
        return price * quantity
    }
}

// 营销规则类
class MarketingRule {
    String ruleName                // 规则名称
    String type                    // 规则类型：FULL_DISCOUNT(满减)、DISCOUNT(折扣)、SPECIFIC_PRODUCT(指定商品优惠)
    BigDecimal threshold = BigDecimal.ZERO // 满减阈值
    BigDecimal discountAmount = BigDecimal.ZERO // 满减金额/折扣率（折扣场景下为0-1的小数，如0.8代表8折）
    List<String> targetSkuList = [] // 指定商品优惠的SKU列表
    BigDecimal maxDiscount = new BigDecimal("999999") // 最大优惠金额（默认无上限）

    // 打印规则详情
    void printRuleInfo() {
        println "==================== 营销规则详情 ===================="
        println "规则名称：${ruleName}"
        println "规则类型：${type}"
        switch (type) {
            case "FULL_DISCOUNT":
                println "满${threshold}减${discountAmount}，最大优惠${maxDiscount}"
                break
            case "DISCOUNT":
                println "订单享${discountAmount * 10}折优惠，最大优惠${maxDiscount}"
                break
            case "SPECIFIC_PRODUCT":
                println "指定商品SKU：${targetSkuList}"
                println "指定商品享${discountAmount * 10}折优惠，最大优惠${maxDiscount}"
                break
        }
        println "======================================================"
    }
}

// 2. 定义DSL委托类：封装营销规则的构建逻辑（核心）
class MarketingRuleDslDelegate {
    MarketingRule currentRule = new MarketingRule() // 当前构建的营销规则

    // -------------- DSL基础语法：设置规则基础信息 --------------
    // 设置规则名称
    void ruleName(String name) {
        currentRule.ruleName = name
    }

    // -------------- DSL核心语法：定义不同类型的营销规则 --------------
    // 1. 满减规则：fullDiscount { ... }
    void fullDiscount(Closure closure) {
        currentRule.type = "FULL_DISCOUNT"
        // 嵌套闭包：将满减规则的配置委托给内部逻辑
        def fullDiscountDelegate = new FullDiscountDelegate(currentRule)
        configureClosure(closure, fullDiscountDelegate)
    }

    // 2. 整体折扣规则：discount { ... }
    void discount(Closure closure) {
        currentRule.type = "DISCOUNT"
        def discountDelegate = new DiscountDelegate(currentRule)
        configureClosure(closure, discountDelegate)
    }

    // 3. 指定商品折扣规则：specificProduct(Closure closure)
    void specificProduct(Closure closure) {
        currentRule.type = "SPECIFIC_PRODUCT"
        def specificProductDelegate = new SpecificProductDelegate(currentRule)
        configureClosure(closure, specificProductDelegate)
    }

    // 通用闭包配置方法（抽取公共逻辑，避免冗余）
    private void configureClosure(Closure closure, def delegateObj) {
        closure.delegate = delegateObj
        closure.resolveStrategy = Closure.DELEGATE_FIRST
        closure.call()
    }

    // 获取构建完成的营销规则
    MarketingRule getRule() {
        return currentRule
    }
}

// 满减规则委托类（封装满减专属配置）
class FullDiscountDelegate {
    MarketingRule rule

    FullDiscountDelegate(MarketingRule rule) {
        this.rule = rule
    }

    // DSL语法：threshold 300 （设置满减阈值）
    void threshold(BigDecimal amount) {
        rule.threshold = amount
    }

    // DSL语法：reduce 50 （设置减免金额）
    void reduce(BigDecimal amount) {
        rule.discountAmount = amount
    }

    // DSL语法：maxDiscount 100 （设置最大优惠金额）
    void maxDiscount(BigDecimal amount) {
        rule.maxDiscount = amount
    }
}

// 折扣规则委托类（封装折扣专属配置）
class DiscountDelegate {
    MarketingRule rule

    DiscountDelegate(MarketingRule rule) {
        this.rule = rule
    }

    // DSL语法：rate 0.8 （设置折扣率，0.8=8折）
    void rate(BigDecimal rate) {
        if (rate <= 0 || rate > 1) {
            throw new IllegalArgumentException("折扣率必须在0-1之间，当前值：${rate}")
        }
        rule.discountAmount = rate
    }

    // DSL语法：maxDiscount 200 （设置最大优惠金额）
    void maxDiscount(BigDecimal amount) {
        rule.maxDiscount = amount
    }
}

// 指定商品规则委托类（封装指定商品专属配置）
class SpecificProductDelegate {
    MarketingRule rule

    SpecificProductDelegate(MarketingRule rule) {
        this.rule = rule
    }

    // DSL语法：sku "SKU001", "SKU002" （设置目标商品SKU）
    void sku(String... skus) {
        rule.targetSkuList.addAll(skus)
    }

    // DSL语法：discountRate 0.7 （设置指定商品折扣率）
    void discountRate(BigDecimal rate) {
        if (rate <= 0 || rate > 1) {
            throw new IllegalArgumentException("商品折扣率必须在0-1之间，当前值：${rate}")
        }
        rule.discountAmount = rate
    }

    // DSL语法：maxDiscount 50 （设置单商品最大优惠金额）
    void maxDiscount(BigDecimal amount) {
        rule.maxDiscount = amount
    }
}

// 3. 定义DSL入口方法：对外暴露简洁的营销规则构建入口
def defineMarketingRule(Closure closure) {
    // 创建顶层委托对象
    def marketingDelegate = new MarketingRuleDslDelegate()
    // 配置闭包委托和解析策略（DSL核心配置）
    closure.delegate = marketingDelegate
    closure.resolveStrategy = Closure.DELEGATE_FIRST
    closure.call()
    // 返回构建完成的营销规则
    return marketingDelegate.rule
}

// 4. 使用DSL定义各类营销规则（核心：简洁、可读、接近自然语言）
// 示例1：定义满减规则（满300减50，最大优惠50）
def fullDiscountRule = defineMarketingRule {
    ruleName "618全场满减活动"
    fullDiscount {
        threshold 300
        reduce 50
        maxDiscount 50
    }
}

// 示例2：定义整体折扣规则（全店8折，最大优惠200）
def discountRule = defineMarketingRule {
    ruleName "双十一全店折扣活动"
    discount {
        rate 0.8
        maxDiscount 200
    }
}

// 示例3：定义指定商品优惠规则（指定2个SKU商品7折，最大优惠100）
def specificProductRule = defineMarketingRule {
    ruleName "爆款手机专属优惠"
    specificProduct {
        sku "SKU001", "SKU002" // 手机SKU
        discountRate 0.7
        maxDiscount 100
    }
}

// 5. 打印所有营销规则详情，验证DSL构建效果
println "=== 构建的营销规则列表 ==="
fullDiscountRule.printRuleInfo()
println "\n"
discountRule.printRuleInfo()
println "\n"
specificProductRule.printRuleInfo()
```

```
=== 构建的营销规则列表 ===
==================== 营销规则详情 ====================
规则名称：618全场满减活动
规则类型：FULL_DISCOUNT
满300减50，最大优惠50
======================================================


==================== 营销规则详情 ====================
规则名称：双十一全店折扣活动
规则类型：DISCOUNT
订单享8.0折优惠，最大优惠200
======================================================


==================== 营销规则详情 ====================
规则名称：爆款手机专属优惠
规则类型：SPECIFIC_PRODUCT
指定商品SKU：[SKU001, SKU002]
指定商品享7.0折优惠，最大优惠100
======================================================
```

### 六、 关键总结
1.  **核心定位**：闭包委托（Delegate）是Groovy闭包的灵活扩展点，可手动指定对象接收闭包的方法/属性调用；
2.  **核心配置**：3步走（创建委托对象 → 绑定`closure.delegate` → 设置`resolveStrategy`（推荐`DELEGATE_FIRST`））；
3.  **查找规则**：默认`This→Owner→Delegate`，可通过解析策略修改优先级，`DELEGATE_FIRST`是DSL开发核心；
4.  **核心价值**：实现“语法与逻辑分离”，支撑DSL构建、方法分发、对象配置简化，是Groovy的核心竞争力之一；
5.  **典型场景**：DSL构建（Gradle/Spock）、对象配置、上下文隔离的业务逻辑封装。