# java javascrip

本文为了解所有关于 Nashorn JavaScript 引擎易于理解的代码例子。 NashornJavaScript 引擎是Java SE 8的一部分，它与其它像Google V8 (它是GoogleChrome 和Node.js的引擎)的独立引擎相互竞争。 Nashorn扩展了Java在JVM上运行动态JavaScript脚本的能力。在接下来的大约15分钟里，您将学习如何在JVM 上动态运行 JavaScript。 通过一些简短的代码示例演示最近 Nashorn的语言特性。 学习 Java 与 JavaScript 的相互调用。最后包括如何在日常的Java 业务中整合动态脚本。

## 使用Nashorn

Nashorn javascript引擎要么在java程序中以编程的方式使用要么在命令行工具jjs使用，jjs在目录$JAVA_HOME/bin中。如果你准备建立一个jjs的符号链接，如下：

```
$ cd /usr/bin
$ ln -s $JAVA_HOME/bin/jjs jjs
$ jjs
jjs> print('Hello World');
```

本教程关注的是在java代码中使用 nashorn，所以我们现在跳过jjs。用java代码来一个简单的 HelloWorld示例，如下：
 
``` java
ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
engine.eval("print('Hello World!');");
```

为了在java中执行JavaScript代码，首先使用原先Rhino(旧版Java中来自Mozilla的引擎)中的包javax.script来创建一个nashorn脚本引擎。.既可以向上面那样把JavaScript代码作为一个字符串来直接执行，也可放入一个js脚本文件中，如：

``` java
ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
engine.eval(new FileReader("script.js"));
```

Nashorn javascript是基于 ECMAScript 5.1 ，但nashorn后续版本将支持ECMAScript 6:当前Nashorn的策略是遵循ECMAScript规范。 当我们发布JDK8时，我们将实现ECMAScript 5.1标准。后续的 Nashorn的版本将实现 ECMAScriptEdition 6标准。

Nashorn定义了很多语言和扩展了 ECMAScript标准的API。接下来我们看看java与JavaScript的通信。

## Java调用Javascript 函数

Nashorn支持java代码直接调用定义在脚本文件中JavaScript函数。你可以把java对象作为函数的参数且在调用函数的java方法中接收返回的数据。

如下的JavaScript代码将会在java端调用：

``` java
var fun1 = function(name) {
    print('Hi there from Javascript, ' + name);
    return "greetings from javascript";
};
 
var fun2 = function (object) {
    print("JS Class Definition: " + Object.prototype.toString.call(object));
};
```

为了调用函数，你首先得把脚本引擎转换为 Invocable。NashornScriptEngine实现了 Invocable 接口且定义一个调用JavaScript函数的方法 invokeFunction，传入函数名即可。

``` java
ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
engine.eval(new FileReader("script.js"));
 
Invocable invocable = (Invocable) engine;
 
Object result = invocable.invokeFunction("fun1", "Peter Parker");
System.out.println(result);
System.out.println(result.getClass());
 
// Hi there from Javascript, Peter Parker
// greetings from javascript
// class java.lang.String
```

上述代码的执行将在控制台打印三行信息。调用 print函数将输出内容通过管道送到 System.out 控制台，因此我们首先看到的是JavaScript打印的信息。 现在我们通过传递任意的 Java对象去调用第二个函数：

``` java
invocable.invokeFunction("fun2", new Date());
// [object java.util.Date]
 
invocable.invokeFunction("fun2", LocalDateTime.now());
// [object java.time.LocalDateTime]
 
invocable.invokeFunction("fun2", new Person());
// [object com.winterbe.java8.Person]
```

你可以传递任意 Java 对象而不会在 JavaScript这边丢失类型信息。因为脚本本身是在 JVM 虚拟机中执行的，我们可以完全利用nashorn 引擎的 Java API 和外部库的强大功能。 在 JavaScript 端调用 Java方法

在 JavaScript 中调用 Java 方法很简单。首先我们定义一个静态的 Java 方法：

``` java
static String fun1(String name) {
    System.out.format("Hi there from Java, %s", name);
    return "greetings from java";
}
```

JavaScript 可通过 Java.type API 来引用 Java 类。这跟在 Java类中引入其他类是类似的。当定义了 Java 类型后我们可直接调用其静态方法fun1() 并打印结果到 sout。因为方法是静态的，所以我们无需创建类实例。

``` java
var MyJavaClass = Java.type('my.package.MyJavaClass');
 
var result = MyJavaClass.fun1('John Doe');
print(result);
 
// Hi there from Java, John Doe
// greetings from java
```

当调用java方法时，Nashorn怎样处理原生JavaScript类型与java类型转换？让我们用一个简单的例子来发现。下面的java方法简单打印实际的类方法参数的类型:

``` java
static void fun2(Object object) {
    System.out.println(object.getClass());
}
```

为了解引擎如何处理类型转换，我使用不同JavaScript类型来调用java方法：

``` java
MyJavaClass.fun2(123);
// class java.lang.Integer
 
MyJavaClass.fun2(49.99);
// class java.lang.Double
 
MyJavaClass.fun2(true);
// class java.lang.Boolean
 
MyJavaClass.fun2("hi there")
// class java.lang.String
 
MyJavaClass.fun2(new Number(23));
// class jdk.nashorn.internal.objects.NativeNumber
 
MyJavaClass.fun2(new Date());
// class jdk.nashorn.internal.objects.NativeDate
 
MyJavaClass.fun2(new RegExp());
// class jdk.nashorn.internal.objects.NativeRegExp
 
MyJavaClass.fun2({foo: 'bar'});
// class jdk.nashorn.internal.scripts.JO4
```

原始的javascript 类型被转换为适当的 java包装器类。而不是本地javascript对象内部适配器类。请记住，这些类来自于jdk.nashorn.internal，所以你不应该在客户端使用这些类:Anythingmarked internal will likely change out from underneath you.ScriptObjectMirror

当使用ScriptObjectMirror把本地JavaScript对象传入时，实际上是有一个java对象表示JavaScript对象。 ScriptObjectMirror实现了接口与jdk.nashorn.api内部的映射。这个包下的类目的就是用于客户端代码使用。

下一个示例更改参数类型Object为ScriptObjectMirror，因此我们能获取到传入JavaScript中对象的一些信息：

``` java
static void fun3(ScriptObjectMirror mirror) {
    System.out.println(mirror.getClassName() + ": " +
        Arrays.toString(mirror.getOwnKeys(true)));
}
```

当我们把传递对象hash到方法中，在Java端就能访问这些属性：

``` java
MyJavaClass.fun3({
    foo: 'bar',
    bar: 'foo'
});
 
// Object: [foo, bar]
```

我们也可以在Java端调用JavaScript对象中的函数。我们首先定义一个JavaScript类型Person，包含属性 firstName 、lastName 和函数getFullName。

``` java
function Person(firstName, lastName) {
    this.firstName = firstName;
    this.lastName = lastName;
    this.getFullName = function() {
        return this.firstName + " " + this.lastName;
    }
}
```

javascript 函数getFullName 能被 ScriptObjectMirror 的callMember()调用。

``` java
static void fun4(ScriptObjectMirror person) {
    System.out.println("Full Name is: " + person.callMember("getFullName"));
}
```

当我们传入一个新的person给java 方法时，我们能在控制台看到预期结果：

``` java
var person1 = new Person("Peter", "Parker");
MyJavaClass.fun4(person1);
 
// Full Name is: Peter Parker
```

## 语言扩展

Nashorn 定义一系列的语言和扩展了 ECMAScript 标准的API。让我们直接进入最新的功能：

### 类型数组

原始javascript 数组时无类型的。 Nashorn运行你在JavaScript中使用java数组：

``` java
var IntArray = Java.type("int[]");
 
var array = new IntArray(5);
array[0] = 5;
array[1] = 4;
array[2] = 3;
array[3] = 2;
array[4] = 1;
 
try {
    array[5] = 23;
} catch (e) {
    print(e.message);  // Array index out of range: 5
}
 
array[0] = "17";
print(array[0]);  // 17
 
array[0] = "wrong type";
print(array[0]);  // 0
 
array[0] = "17.3";
print(array[0]);  // 17
```

int[] 数组的行为像一个真正的 java int 数组。但当我们试图添加非整数的值的数组时，Nashorn 会执行隐式类型转换。字符串会自动转换为int，这相当方便。

### 集合与For Each

我们可以使用java的集合来代替数组。首先定义使用Java.type定义一个java类型，而后根据需要创建一个实例。

``` java
var ArrayList = Java.type('java.util.ArrayList');
var list = new ArrayList();
list.add('a');
list.add('b');
list.add('c');
 
for each (var el in list) print(el);  // a, b, c
```

为了遍历集合和数组中的元素，Nashorn 引入了 for each 语句。这就像是 Java的 for 循环一样。这里是一个对集合元素进行遍历的例子，使用的是 :

``` java
var map = new java.util.HashMap();
map.put('foo', 'val1');
map.put('bar', 'val2');
 
for each (var e in map.keySet()) print(e);  // foo, bar
 
for each (var e in map.values()) print(e);  // val1, val2
```

### Lambda 表达式和 Streams

似乎大家都比较喜欢 Lambda 和 Streams ------ Nashorn 也是！虽然ECMAScript 5.1 中缺少 Java 8 Lambda表达式中的紧缩箭头的语法，但我们可以在接受 Lambda表达式的地方使用函数来替代。

``` java
var list2 = new java.util.ArrayList();
list2.add("ddd2");
list2.add("aaa2");
list2.add("bbb1");
list2.add("aaa1");
list2.add("bbb3");
list2.add("ccc");
list2.add("bbb2");
list2.add("ddd1");
 
list2
    .stream()
    .filter(function(el) {
        return el.startsWith("aaa");
    })
    .sorted()
    .forEach(function(el) {
        print(el);
    });
    // aaa1, aaa2
```

### 扩展类

Java 的类型可以简单的通过 Java.extend进行扩展，在下个例子你将在脚本中创建一个多线程示例：

``` java
var Runnable = Java.type('java.lang.Runnable');
var Printer = Java.extend(Runnable, {
    run: function() {
        print('printed from a separate thread');
    }
});
 
var Thread = Java.type('java.lang.Thread');
new Thread(new Printer()).start();
 
new Thread(function() {
    print('printed from another thread');
}).start();
 
// printed from a separate thread
// printed from another thread
```

### 参数重载

方法和函数可以使用点符号或方括号来进行调用。

``` java
var System = Java.type('java.lang.System');
System.out.println(10);              // 10
System.out["println"](11.0);         // 11.0
System.out["println(double)"](12);   // 12.0
```

在使用重载的参数来调用方法时可以传递可选参数来确定具体调用了哪个方法，如

```
println(double)。
Java Beans
```

我们不需要常规的用 getter 或者 setter来访问类成员属性，可直接用属性名简单访问 Java Bean 中的属性。例如：

``` java
var Date = Java.type('java.util.Date');
var date = new Date();
date.year += 1900;
print(date.year);  // 2014
```

### 函数语法

如果只是简单的一行函数我们可以不用大括号：

```
function sqr(x) x # x;
print(sqr(3));    // 9
```

### 属性绑定

来自不同对象的属性可以绑定在一起：

``` java
var o1 = {};
var o2 = { foo: 'bar'};
 
Object.bindProperties(o1, o2);
 
print(o1.foo);    // bar
o1.foo = 'BAM';
print(o2.foo);    // BAM
```

### 字符串处理

我喜欢字符串裁剪.

``` java
print("   hehe".trimLeft());            // hehe
print("hehe    ".trimRight() + "he");   // hehehe
```

### 在哪里

以防忘记你在哪里:

```
print(__FILE__, __LINE__, __DIR__);
```

### Import 的范围

有时，这在一次性导入多个java包时非常有用。我们可以使用JavaImporter并结合with，在with块范围内引用：

``` java
var imports = new JavaImporter(java.io, java.lang);
with (imports) {
    var file = new File(__FILE__);
    System.out.println(file.getAbsolutePath());
    // /path/to/my/script.js
}
```

### 数组转换

有些包时可以直接使用而不必利用 Java.type 或JavaImporter引入，如java.util:

``` java
var list = new java.util.ArrayList();
list.add("s1");
list.add("s2");
list.add("s3");
```

如下的代码演示了将java list转换为JavaScript的数组：

``` java
var jsArray = Java.from(list);
print(jsArray);                                  // s1,s2,s3
print(Object.prototype.toString.call(jsArray));  // [object Array]
```

其他的方式：

``` java
var javaArray = Java.to([3, 5, 7, 11], "int[]");
```

### 调用父类函数

在 JavaScript 中访问重载的成员会有一点点尴尬，因为 ECMAScript 没有类似
Java 的 super 关键字一样的东西。所幸的是 Nashorn 有办法解决。

首先我们在 Java 代码中定义一个超类：

``` java
class SuperRunner implements Runnable {
    @Override
    public void run() {
        System.out.println("super run");
    }
}
```

接下来我们在 JavaScript 中重载 SuperRunner 。创建一个新的 Runner实例时请注意 Nashorn 的扩展语法：其重载成员的语法是参考 Java的匿名对象的做法。

``` java
var SuperRunner = Java.type('com.winterbe.java8.SuperRunner');
var Runner = Java.extend(SuperRunner);
 
var runner = new Runner() {
    run: function() {
        Java.super(runner).run();
        print('on my run');
    }
}
runner.run();
 
// super run
// on my run
```

我们使用Java.super调用了重载方法SuperRunner.run()。在JavaScript中执行其它脚本是十分容易的。我们可以load函数载入本地或远程的脚本。在我的很多web前端中都使用了 Underscore.js ，因此在Nashorn中我们可以重用Underscore：

``` java
load('http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js');
 
var odds = _.filter([1, 2, 3, 4, 5, 6], function (num) {
    return num % 2 == 1;
});
 
print(odds);  // 1, 3, 5
```

扩展脚本的执行是在同一个 JavaScript 上下文中，因此我们可以直接访问underscore变量。记住脚本的加载可能会因为变量名的重叠导致代码出问题。我们可以通过将加载的脚本文件放置到一个新的全局上下文来解决这个问题：

```
loadWithNewGlobal('script.js');
```

### 命令行脚本

如果你对用 Java 编写命令行脚本很感兴趣的话，可以试试 Nake 。Nake是一个为 Java 8 Nashorn 准备的简单 Make 工具。你可以在 Nakefile文件中定义任务，然后使用 nake --- myTask 来运行任务。任务使用 JavaScript编写并通过 Nashorn 脚本模式运行，因此你可以让你的终端应用完全利用 Java 8API 和其他 Java 库强大的功能。

对 Java 开发者而言，编写命令行脚本从来没有如此简单过。
