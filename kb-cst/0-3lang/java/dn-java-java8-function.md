# java8方法引用

在学习lambda表达式之后，我们通常使用lambda表达式来创建匿名方法。然而，有时候我们仅仅是调用了一个已存在的方法。如下:

``` java
Arrays.sort(stringsArray,(s1,s2)->s1.compareToIgnoreCase(s2));
```

在Java8中，我们可以直接通过方法引用来简写lambda表达式中已经存在的方法。

``` java
Arrays.sort(stringsArray, String::compareToIgnoreCase);
```

这种特性就叫做方法引用(Method Reference)。

## 方法引用的形式

方法引用的标准形式是: 类名::方法名 。（注意：只需要写方法名，不需要写括号）

有以下四种形式的方法引用:

| 类型                             | 示例                                 |
|----------------------------------|--------------------------------------|
| 引用静态方法                     | ContainingClass::staticMethodName    |
| 引用某个对象的实例方法           | containingObject::instanceMethodName |
| 引用某个类型的任意对象的实例方法 | ContainingType::methodName           |
| 引用构造方法                     | ClassName::new                       |

方法引用必须赋值给函数式接口。

# java functional interface

函数式接口(Functional Interface)就是一个有且仅有一个抽象方法，但是可以有多个非抽象方法的接口。

函数式接口可以被隐式转换为 lambda 表达式。

Lambda表达式和方法引用（实际上也可认为是Lambda表达式）必须赋值在函数接口上。

*函数接口中，方法名不重要，关键是入参数和出参数类型要和传入的函数引用一致。*

## lambda示例

``` java
public class Java8D1Lambda {

    public static void main(String[] args) {
        println("3+4="+operate(3,4,add));
        println("3-4="+operate(3,4,sub));
        println("3*4="+operate(3,4,multi));
    }

    static void println(String s){
        System.out.println(s);
    }

    static MathOperation add=(int a, int b) -> a + b;
    static MathOperation sub=(int a, int b) -> a - b;
    static MathOperation multi=(int a, int b) -> a * b;

    @FunctionalInterface
    interface MathOperation {
        int operation(int a, int b);
    }

    private static int operate(int a, int b, MathOperation mathOperation){
        return mathOperation.operation(a, b);
    }
}
```

在java包java.util.function已经有了很多类型的函数接口，命名规范如下：

1. XxxPredicate: 断言，传入一参数，返回真假
2. XxxConsumer: 消费者，传入一参数，不返回值
3. XxxOperator: 操作者：传入一参数，返回值
4. XxxSupplier: 生产者，无入参数，返回指定类型值
5. XxxFunction: 函数，一入参一返参

## 方法示例

``` java
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class Java8Function {
    public static void main(String[] args) {
        Map<String, Consumer<String>> mf=new HashMap<String, Consumer<String>>(){
            {
                put("c1",(new C1())::sync);
                put("c2",(new C2())::sync);
            }
        };
        mf.get("c1").accept("aaa");
        mf.get("c2").accept("bbb");
    }

    static class C1 {
        public void sync(String s){
            System.out.println("c1 sync:"+s);
        }
    }
    static class C2 {
        public void sync(String s){
            System.out.println("c2 sync:"+s);
        }
    }
}
```

下面我们通过一个小Demo来分别学习这几种形式的方法引用:

``` java
public class Person {
    public enum Sex{
        MALE,FEMALE
    }

    String name;
    LocalDate birthday;
    Sex gender;
    String emailAddress;

    public String getEmailAddress() {
        return emailAddress;
    }

    public Sex getGender() {
        return gender;
    }

    public LocalDate getBirthday() {
        return birthday;
    }

    public String getName() {
        return name;
    }

    public static int compareByAge(Person a,Person b){
        return a.birthday.compareTo(b.birthday);
    }

}
```

## 引用静态方法

``` java
Person [] persons = new Person[10];

//使用匿名类
Arrays.sort(persons, new Comparator<Person>() {
            @Override
            public int compare(Person o1, Person o2) {
                return o1.birthday.compareTo(o2.birthday);
            }
 });

//使用lambda表达式
Arrays.sort(persons, (o1, o2) -> o1.birthday.compareTo(o2.birthday));

//使用lambda表达式和类的静态方法
Arrays.sort(persons, (o1, o2) -> Person.compareByAge(o1,o2));

//使用方法引用
//引用的是类的静态方法
Arrays.sort(persons, Person::compareByAge);
```

## 引用对象的实例方法

``` java
class ComparisonProvider{
            public int compareByName(Person a,Person b){
                return a.getName().compareTo(b.getName());
            }

            public int compareByAge(Person a,Person b){
                return a.getBirthday().compareTo(b.getBirthday());
            }
        }

ComparisonProvider provider = new ComparisonProvider();

//使用lambda表达式
//对象的实例方法
Arrays.sort(persons,(a,b)->provider.compareByAge(a,b));

//使用方法引用
//引用的是对象的实例方法
Arrays.sort(persons, provider::compareByAge);
```

## 引用类型对象的实例方法

``` java
String[] stringsArray = {"Hello","World"};

//使用lambda表达式和类型对象的实例方法
Arrays.sort(stringsArray,(s1,s2)->s1.compareToIgnoreCase(s2));

//使用方法引用
//引用的是类型对象的实例方法
Arrays.sort(stringsArray, String::compareToIgnoreCase);
```

## 引用构造方法

``` java
public static <T, SOURCE extends Collection<T>, DEST extends Collection<T>>
    DEST transferElements(SOURCE sourceColletions, Supplier<DEST> colltionFactory) {
        DEST result = colltionFactory.get();
        for (T t : sourceColletions) {
            result.add(t);
        }
        return result;
    }
...

final List<Person> personList = Arrays.asList(persons);

//使用lambda表达式
Set<Person> personSet = transferElements(personList,()-> new HashSet<>());

//使用方法引用
//引用的是构造方法
Set<Person> personSet2 = transferElements(personList, HashSet::new);
```
