
# java8

## java Base64
对Base64编码的支持已经被加入到Java
8官方库中，这样不需要使用第三方库就可以进行Base64编码，例子代码如下：

``` java
package com.edu.java8.base64;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class Base64s {
    public static void main(String[] args) {
        final String text = "Base64 finally in Java 8!";

        final String encoded = Base64
            .getEncoder()
            .encodeToString( text.getBytes( StandardCharsets.UTF_8 ) );
        System.out.println( encoded );

        final String decoded = new String( 
            Base64.getDecoder().decode( encoded ),
            StandardCharsets.UTF_8 );
        System.out.println( decoded );
    }
}
```

这个例子的输出结果如下：

```
QmFzZTY0IGZpbmFsbHkgaW4gSmF2YSA4IQ==
Base64 finally in Java 8!
```

新的Base64API也支持URL和MINE的编码解码。

```
Base64.getUrlEncoder()
Base64.getUrlDecoder()
Base64.getMimeEncoder()
Base64.getMimeDecoder()
```

## java DateTime API
Java 8引入了新的Date-Time API(JSR310)来改进时间、日期的处理。时间和日期的管理一直是最令Java开发者痛苦的问题。java.util.Date和后来的java.util.Calendar一直没有解决这个问题（甚至令开发者更加迷茫）。

因为上面这些原因，诞生了第三方库Joda-Time，可以替代Java的时间管理API。Java8中新的时间和日期管理API深受Joda-Time影响，并吸收了很多Joda-Time的精华。新的java.time包包含了所有关于日期、时间、时区、Instant（跟日期类似但是精确到纳秒）、duration（持续时间）和时钟操作的类。新设计的API认真考虑了这些类的不变性（从java.util.Calendar吸取的教训），如果某个实例需要修改，则返回一个新的对象。

我们接下来看看java.time包中的关键类和各自的使用例子。首先，Clock类使用时区来返回当前的纳秒时间和日期。Clock可以替代System.currentTimeMillis()和TimeZone.getDefault()。

```
// Get the system clock as UTC offset 
final Clock clock = Clock.systemUTC();
System.out.println( clock.instant() );
System.out.println( clock.millis() );
```

这个例子的输出结果是：

```
2014-04-12T15:19:29.282Z
1397315969360
```

第二，关注下LocalDate和LocalTime类。LocalDate仅仅包含ISO-8601日历系统中的日期部分；LocalTime则仅仅包含该日历系统中的时间部分。这两个类的对象都可以使用Clock对象构建得到。

``` java
// Get the local date and local time
final LocalDate date = LocalDate.now();
final LocalDate dateFromClock = LocalDate.now( clock );

System.out.println( date );
System.out.println( dateFromClock );

// Get the local date and local time
final LocalTime time = LocalTime.now();
final LocalTime timeFromClock = LocalTime.now( clock );

System.out.println( time );
System.out.println( timeFromClock );
```

上述例子的输出结果如下：

```
2014-04-12
2014-04-12
11:25:54.568
15:25:54.568
```

LocalDateTime类包含了LocalDate和LocalTime的信息，但是不包含ISO-8601日历系统中的时区信息。这里有一些关于LocalDate和LocalTime的例子：

``` java
// Get the local date/time
final LocalDateTime datetime = LocalDateTime.now();
final LocalDateTime datetimeFromClock = LocalDateTime.now( clock );
System.out.println( datetime );
System.out.println( datetimeFromClock );
```

上述这个例子的输出结果如下：

```
2014-04-12T11:37:52.309
2014-04-12T15:37:52.309
```

如果你需要特定时区的data/time信息，则可以使用ZoneDateTime，它保存有ISO-8601日期系统的日期和时间，而且有时区信息。下面是一些使用不同时区的例子：

``` java
// Get the zoned date/time
final ZonedDateTime zonedDatetime = ZonedDateTime.now();
final ZonedDateTime zonedDatetimeFromClock = ZonedDateTime.now( clock );
final ZonedDateTime zonedDatetimeFromZone = ZonedDateTime.now( ZoneId.of( "America/Los_Angeles" ) );

System.out.println( zonedDatetime );
System.out.println( zonedDatetimeFromClock );
System.out.println( zonedDatetimeFromZone );
```

这个例子的输出结果是：

```
2014-04-12T11:47:01.017-04:00[America/New_York]
2014-04-12T15:47:01.017Z
2014-04-12T08:47:01.017-07:00[America/Los_Angeles]
```

最后看下Duration类，它持有的时间精确到秒和纳秒。这使得我们可以很容易得计算两个日期之间的不同，例子代码如下：

``` java
// Get duration between two dates
final LocalDateTime from = LocalDateTime.of( 2014, Month.APRIL, 16, 0, 0, 0 );
final LocalDateTime to = LocalDateTime.of( 2015, Month.APRIL, 16, 23, 59, 59 );

final Duration duration = Duration.between( from, to );
System.out.println( "Duration in days: " + duration.toDays() );
System.out.println( "Duration in hours: " + duration.toHours() );
```

这个例子用于计算2014年4月16日和2015年4月16日之间的天数和小时数，输出结果如下：

```
Duration in days: 365
Duration in hours: 8783
```

对于Java
8的新日期时间的总体印象还是比较积极的，一部分是因为Joda-Time的积极影响，另一部分是因为官方终于听取了开发人员的需求。如果希望了解更多细节，可以参考官方文档。

## java default method

Java8使用两个新概念扩展了接口的含义：默认方法和静态方法。默认方法使得接口有点类似traits，不过要实现的目标不一样。默认方法使得开发者可以在不破坏二进制兼容性的前提下，往现存接口中添加新的方法，即不强制那些实现了该接口的类也同时实现这个新加的方法。

默认方法和抽象方法之间的区别在于抽象方法需要实现，而默认方法不需要。接口提供的默认方法会被接口的实现类继承或者覆写，例子代码如下：

``` java
private interface Defaulable {
    // Interfaces now allow default methods, the implementer may or
    // may not implement (override) them.
    default String notRequired() {
        return "Default implementation";
    }
}

private static class DefaultableImpl implements Defaulable {
}

private static class OverridableImpl implements Defaulable {
    @Override
    public String notRequired() {
        return "Overridden implementation";
    }
}
```

Defaulable接口使用关键字default定义了一个默认方法notRequired()。DefaultableImpl类实现了这个接口，同时默认继承了这个接口中的默认方法；OverridableImpl类也实现了这个接口，但覆写了该接口的默认方法，并提供了一个不同的实现。

Java 8带来的另一个有趣的特性是在接口中可以定义静态方法，例子代码如下：

``` java
private interface DefaulableFactory {
    // Interfaces now allow static methods
    static Defaulable create( Supplier< Defaulable > supplier ) {
        return supplier.get();
    }
}
```

下面的代码片段整合了默认方法和静态方法的使用场景：

``` java 
public static void main( String[] args ) {
    Defaulable defaulable = DefaulableFactory.create( DefaultableImpl::new );
    System.out.println( defaulable.notRequired() );

    defaulable = DefaulableFactory.create( OverridableImpl::new );
    System.out.println( defaulable.notRequired() );
}
```

这段代码的输出结果如下：

```
Default implementation
Overridden implementation
```

由于JVM上的默认方法的实现在字节码层面提供了支持，因此效率非常高。默认方法允许在不打破现有继承体系的基础上改进接口。该特性在官方库中的应用是：给java.util.Collection接口添加新方法，如stream()、parallelStream()、forEach()和removeIf()等等。

尽管默认方法有这么多好处，但在实际开发中应该谨慎使用：在复杂的继承体系中，默认方法可能引起歧义和编译错误。如果你想了解更多细节，可以参考官方文档。

## java Optional
新版本的Java，比如Java8引入了一个新的Optional类。Optional类的Javadoc描述如下：

这是一个可以为null的容器对象。如果值存在则isPresent()方法会返回true，调用get()方法会返回该对象。

本文会逐个探讨Optional类包含的方法，并通过一两个示例展示如何使用。

### of

为非null的值创建一个Optional。

of方法通过工厂方法创建Optional类。需要注意的是，创建对象时传入的参数不能为null。如果传入参数为null，则抛出NullPointerException。

``` java
//调用工厂方法创建Optional实例
Optional<String> name = Optional.of("Sanaulla");
//传入参数为null，抛出NullPointerException.
Optional<String> someNull = Optional.of(null);
```

### ofNullable

为指定的值创建一个Optional，如果指定的值为null，则返回一个空的Optional。

ofNullable与of方法相似，唯一的区别是可以接受参数为null的情况。示例如下：

``` java
//下面创建了一个不包含任何值的Optional实例
//例如，值为'null'
Optional empty = Optional.ofNullable(null);
```

### isPresent

非常容易理解，如果值存在返回true，否则返回false。

类似下面的代码：

``` java
//isPresent方法用来检查Optional实例中是否包含值
if (name.isPresent()) {
  //在Optional实例内调用get()返回已存在的值
  System.out.println(name.get());//输出Sanaulla
}
```

### get

如果Optional有值则将其返回，否则抛出NoSuchElementException。

上面的示例中，get方法用来得到Optional实例中的值。下面我们看一个抛出NoSuchElementException的例子：

``` java
//执行下面的代码会输出：No value present 
try {
  //在空的Optional实例上调用get()，抛出NoSuchElementException
  System.out.println(empty.get());
} catch (NoSuchElementException ex) {
  System.out.println(ex.getMessage());
}
```

### ifPresent

如果Optional实例有值则为其调用consumer，否则不做处理

要理解ifPresent方法，首先需要了解Consumer类。简答地说，Consumer类包含一个抽象方法。该抽象方法对传入的值进行处理，但没有返回值。Java8支持不用接口直接通过lambda表达式传入参数。

如果Optional实例有值，调用ifPresent()可以接受接口段或lambda表达式。类似下面的代码：

``` java
//ifPresent方法接受lambda表达式作为参数。
//lambda表达式对Optional的值调用consumer进行处理。
name.ifPresent((value) -> {
  System.out.println("The length of the value is: " + value.length());
});
```

### orElse

如果有值则将其返回，否则返回指定的其它值。

如果Optional实例有值则将其返回，否则返回orElse方法传入的参数。示例如下：

``` java
//如果值不为null，orElse方法返回Optional实例的值。
//如果为null，返回传入的消息。
//输出：There is no value present!
System.out.println(empty.orElse("There is no value present!"));
//输出：Sanaulla
System.out.println(name.orElse("There is some value!"));
```

### orElseGet

orElseGet与orElse方法类似，区别在于得到的默认值。orElse方法将传入的字符串作为默认值，orElseGet方法可以接受Supplier接口的实现用来生成默认值。示例如下：

``` java
//orElseGet与orElse方法类似，区别在于orElse传入的是默认值，
//orElseGet可以接受一个lambda表达式生成默认值。
//输出：Default Value
System.out.println(empty.orElseGet(() -> "Default Value"));
//输出：Sanaulla
System.out.println(name.orElseGet(() -> "Default Value"));
```

### orElseThrow

如果有值则将其返回，否则抛出supplier接口创建的异常。

在orElseGet方法中，我们传入一个Supplier接口。然而，在orElseThrow中我们可以传入一个lambda表达式或方法，如果值不存在来抛出异常。示例如下：

``` java
try {
  //orElseThrow与orElse方法类似。与返回默认值不同，
  //orElseThrow会抛出lambda表达式或方法生成的异常
  empty.orElseThrow(ValueAbsentException::new);
} catch (Throwable ex) {
  //输出: No value present in the Optional instance
  System.out.println(ex.getMessage());
}
```

ValueAbsentException定义如下：

``` java
class ValueAbsentException extends Throwable {

  public ValueAbsentException() {
    super();
  }

  public ValueAbsentException(String msg) {
    super(msg);
  }

  @Override
  public String getMessage() {
    return "No value present in the Optional instance";
  }
}
```

### map

map方法文档说明如下：

如果有值，则对其执行调用mapping函数得到返回值。如果返回值不为null，则创建包含mapping返回值的Optional作为map方法返回值，否则返回空Optional。

map方法用来对Optional实例的值执行一系列操作。通过一组实现了Function接口的lambda表达式传入操作。如果你不熟悉Function接口，可以参考我的这篇博客。map方法示例如下：

``` java
//map方法执行传入的lambda表达式参数对Optional实例的值进行修改。
//为lambda表达式的返回值创建新的Optional实例作为map方法的返回值。
Optional<String> upperName = name.map((value) -> value.toUpperCase());
System.out.println(upperName.orElse("No value found"));
```

### flatMap

如果有值，为其执行mapping函数返回Optional类型返回值，否则返回空Optional。flatMap与map（Funtion）方法类似，区别在于flatMap中的mapper返回值必须是Optional。调用结束时，flatMap不会对结果用Optional封装。

flatMap方法与map方法类似，区别在于mapping函数的返回值不同。map方法的mapping函数返回值可以是任何类型T，而flatMap方法的mapping函数必须是Optional。

参照map函数，使用flatMap重写的示例如下：

``` java
//flatMap与map（Function）非常类似，区别在于传入方法的lambda表达式的返回类型。
//map方法中的lambda表达式返回值可以是任意类型，在map函数返回之前会包装为Optional。 
//但flatMap方法中的lambda表达式返回值必须是Optionl实例。 
upperName = name.flatMap((value) -> Optional.of(value.toUpperCase()));
System.out.println(upperName.orElse("No value found"));//输出SANAULLA
```

### filter

filter个方法通过传入限定条件对Optional实例的值进行过滤。文档描述如下：

如果有值并且满足断言条件返回包含该值的Optional，否则返回空Optional。

读到这里，可能你已经知道如何为filter方法传入一段代码。是的，这里可以传入一个lambda表达式。对于filter函数我们应该传入实现了Predicate接口的lambda表达式。如果你不熟悉Predicate接口，可以参考这篇文章。

现在我来看看filter的各种用法，下面的示例介绍了满足限定条件和不满足两种情况：

``` java 
//filter方法检查给定的Option值是否满足某些条件。
//如果满足则返回同一个Option实例，否则返回空Optional。
Optional<String> longName = name.filter((value) -> value.length() > 6);
System.out.println(longName.orElse("The name is less than 6 characters"));//输出Sanaulla

//另一个例子是Optional值不满足filter指定的条件。
Optional<String> anotherName = Optional.of("Sana");
Optional<String> shortName = anotherName.filter((value) -> value.length() > 6);
//输出：name长度不足6字符
System.out.println(shortName.orElse("The name is less than 6 characters"));
```

以上，我们介绍了Optional类的各个方法。下面通过一个完整的示例对用法集中展示：

``` java
public class OptionalDemo {
 
  public static void main(String[] args) {
    //创建Optional实例，也可以通过方法返回值得到。
    Optional<String> name = Optional.of("Sanaulla");
 
    //创建没有值的Optional实例，例如值为'null'
    Optional empty = Optional.ofNullable(null);
 
    //isPresent方法用来检查Optional实例是否有值。
    if (name.isPresent()) {
      //调用get()返回Optional值。
      System.out.println(name.get());
    }
 
    try {
      //在Optional实例上调用get()抛出NoSuchElementException。
      System.out.println(empty.get());
    } catch (NoSuchElementException ex) {
      System.out.println(ex.getMessage());
    }
 
    //ifPresent方法接受lambda表达式参数。
    //如果Optional值不为空，lambda表达式会处理并在其上执行操作。
    name.ifPresent((value) -> {
      System.out.println("The length of the value is: " + value.length());
    });
 
    //如果有值orElse方法会返回Optional实例，否则返回传入的错误信息。
    System.out.println(empty.orElse("There is no value present!"));
    System.out.println(name.orElse("There is some value!"));
 
    //orElseGet与orElse类似，区别在于传入的默认值。
    //orElseGet接受lambda表达式生成默认值。
    System.out.println(empty.orElseGet(() -> "Default Value"));
    System.out.println(name.orElseGet(() -> "Default Value"));
 
    try {
      //orElseThrow与orElse方法类似，区别在于返回值。
      //orElseThrow抛出由传入的lambda表达式/方法生成异常。
      empty.orElseThrow(ValueAbsentException::new);
    } catch (Throwable ex) {
      System.out.println(ex.getMessage());
    }
 
    //map方法通过传入的lambda表达式修改Optonal实例默认值。 
    //lambda表达式返回值会包装为Optional实例。
    Optional<String> upperName = name.map((value) -> value.toUpperCase());
    System.out.println(upperName.orElse("No value found"));
 
    //flatMap与map（Funtion）非常相似，区别在于lambda表达式的返回值。
    //map方法的lambda表达式返回值可以是任何类型，但是返回值会包装成Optional实例。
    //但是flatMap方法的lambda返回值总是Optional类型。
    upperName = name.flatMap((value) -> Optional.of(value.toUpperCase()));
    System.out.println(upperName.orElse("No value found"));
 
    //filter方法检查Optiona值是否满足给定条件。
    //如果满足返回Optional实例值，否则返回空Optional。
    Optional<String> longName = name.filter((value) -> value.length() > 6);
    System.out.println(longName.orElse("The name is less than 6 characters"));
 
    //另一个示例，Optional值不满足给定条件。
    Optional<String> anotherName = Optional.of("Sana");
    Optional<String> shortName = anotherName.filter((value) -> value.length() > 6);
    System.out.println(shortName.orElse("The name is less than 6 characters"));
 
  }
 
}
```

上述代码输出如下：

```
Sanaulla
No value present
The length of the value is: 8
There is no value present!
Sanaulla
Default Value
Sanaulla
No value present in the Optional instance
SANAULLA
SANAULLA
Sanaulla
The name is less than 6 characters
```
