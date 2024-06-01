
# java reflect

## JAVA反射机制定义

JAVA反射机制是在运行状态中，对于任意一个类，都能够知道这个类的所有属性和方法；对于任意一个对象，都能够调用它的任意一个方法；这种动态获取的信息以及动态调用对象的方法的功能称为java语言的反射机制。

1. Reflection 是 Java 程序开发语言的特征之一
2. 允许运行中的 Java 程序对自身进行 "自审"
3. 反射API用于反应在当前Java虚拟机中的类、接口或者对象信息

Java反射机制主要提供了以下功能：

1. 获取一个对象的类信息.
2. 获取一个类的访问修饰符、成员、方法、构造方法以及超类的信息.
3. 检获属于一个接口的常量和方法声明.
4. 创建一个直到程序运行期间才知道名字的类的实例.
5. 获取并设置一个对象的成员，甚至这个成员的名字是在程序运行期间才知道.
6. 检测一个在运行期间才知道名字的对象的方法

使用 Reflection遵循三个步骤：

1. 第一步是获得你想操作的类的 java.lang.Class 对象
2. 第二步是调用诸如 getDeclaredMethods 的方法
3. 第三步使用 reflection API 来操作这些信息

## 一个反射简单的例子

``` java
import java.lang.reflect.#;
public class DumpMethods {
    public static void main(String args[]) {
        try {
            Class c = Class.forName(args[0]);
            Method m[] = c.getDeclaredMethods();
            for (int i = 0; i < m.length; i++)
                System.out.println(m[i].toString());
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
```

执行如下语句：

``` java
java DumpMethods java.util.Stack
```

## Class对象

众所周知Java有个Object class，是所有Java classes的继承根源，其内声明了数个应该在所有Java class中被改写的methods：hashCode()、equals()、clone()、toString()、getClass()等。其中getClass()返回一个Class object。

Class class十分特殊。它和一般classes一样继承自Object，其实体用以表达Java程序运行时的classes和interfaces，也用来表达enum、array、primitive Java types（boolean, byte, char, short, int, long, float,double）以及关键词void。当一个class被加载，或当加载器（class loader）的defineClass()被JVM调用，JVM 便自动产生一个Class object。


## 获得一个 Class 对象
如果一个类的实例已经得到，你可以使用

``` java
 Class c = 对象名.getClass(); 
```

例：

``` java
TextField t = new TextField(); 
Class c = t.getClass();
Class s = c.getSuperclass(); 
```

如果你在编译期知道类的名字，你可以使用如下的方法

``` java
Class c = java.awt.Button.class; 
```

或者

``` java
 Class c = Integer.TYPE;
```

如果类名在编译期不知道, 但是在运行期可以获得, 你可以使用下面的方法

``` java
 Class c = Class.forName(strg); 
```

例如：

``` java
Class c1 = Class.forName ("java.lang.String");
Class c2 = Class.forName ("java.awt.Button");
Class c3 = Class.forName ("java.util.LinkedList$Entry"); 
```

## 获取类的方法信息

1. 获取方法名
2. 获取方法参数信息


``` java
import java.lang.reflect.#;
public class method1 {
    private int f1(Object p, int x) throws NullPointerException {
        if (p == null)
        throw new NullPointerException();
        return x;
    }
    public static void main(String args[]) {
        try {
            Class cls = Class.forName("method1");
            Method methlist[] = cls.getDeclaredMethods();
            for (int i = 0; i < methlist.length; i++) {
                Method m = methlist[i];
                System.out.println("name = " + m.getName());
                System.out.println("decl class = " + m.getDeclaringClass());
                Class pvec[] = m.getParameterTypes();
                for (int j = 0; j < pvec.length; j++)
                        System.out.println("param #" + j + " " + pvec[j]);
                Class evec[] = m.getExceptionTypes();
                for (int j = 0; j < evec.length; j++)
                        System.out.println("ExceptionType #" + j + " " + evec[j]);
                System.out.println("return type = " + m.getReturnType());
                System.out.println("-----");
            }
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
```

## 获取构造方法信息

```  java
import java.lang.reflect.#;
public class constructor1 {
    public constructor1() { }
    protected constructor1(int i, double d) {  }
    public static void main(String args[])  {
        try {
            Class cls = Class.forName("constructor1");
            Constructor ctorlist[] = cls.getDeclaredConstructors();
            for (int i = 0; i < ctorlist.length; i++) {
                Constructor ct = ctorlist[i];
                System.out.println("name = " + ct.getName());
                System.out.println("decl class = " + ct.getDeclaringClass());
                Class pvec[] = ct.getParameterTypes();
                for (int j = 0; j < pvec.length; j++)
                        System.out.println("ParameterType #" + j + " " + pvec[j]);
                Class evec[] = ct.getExceptionTypes();
                for (int j = 0; j < evec.length; j++)
                        System.out.println("ExceptionType #" + j + " " + evec[j]);
                System.out.println("-----");
            }
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
```

## 获取类的属性信息

```  java
import java.lang.reflect.#;
public class field1 {
    private double d;
    public static final int i = 37;
    String s = "testing";
    public static void main(String args[]) {
        try {
            Class cls = Class.forName("field1");
            Field fieldlist[] = cls.getDeclaredFields();
            for (int i = 0; i < fieldlist.length; i++) {
                Field fld = fieldlist[i];
                System.out.println("name = " + fld.getName());
                System.out.println("decl class = " + fld.getDeclaringClass());
                System.out.println("type = " + fld.getType());
                int mod = fld.getModifiers();
                System.out.println("modifiers = " + Modifier.toString(mod));
                System.out.println("-----");            }
        } catch (Throwable e) {System.err.println(e);}
    }
}
```

## 用反射根据方法的名称来执行方法
获取指定方法要指定两部分内容：

1. 方法名
2. 方法所需参数

因为java中有重载一说，同名方法只要参数相异是可以共存的，所以确定一个方法时方法参数也是必不可少的。

执行方法要有一个对象实体，一般我们都是执行某个对象的某个方法，所以一般格式如下：

```  java
方法名.invoke(对象实体,参数列表)
```

```  java
import java.lang.reflect.#;
public class method2 {
    public int add(int a, int b) {return a + b;}
    public static void main(String args[]) {
        try {
            Class cls = Class.forName("method2");
            Class partypes[] = new Class[2];
            partypes[0] = Integer.TYPE;
            partypes[1] = Integer.TYPE;
            Method meth = cls.getMethod("add", partypes);
            method2 methobj = new method2();
            Object arglist[] = new Object[2];
            arglist[0] = new Integer(37);
            arglist[1] = new Integer(47);
            Object retobj = meth.invoke(methobj, arglist);
            Integer retval = (Integer) retobj;
            System.out.println(retval.intValue());
        } catch (Throwable e) {System.err.println(e);}
    }
}
```

## 用反射创建新的对象

如果创建对象不要参数，可以用Class对象的newInstance()来实现。

如果要参数的话，就要根据参数类型找到构造函数，再用构造函数来创建了。

```  java
import java.lang.reflect.#;
public class constructor2 {
    public constructor2() {}
    public constructor2(int a, int b) {
        System.out.println("a = " + a + " b = " + b);
    }
    public static void main(String args[]) {
        try {
            Class cls = Class.forName("constructor2");
            Class partypes[] = new Class[2];
            partypes[0] = Integer.TYPE;
            partypes[1] = Integer.TYPE;
            Constructor ct = cls.getConstructor(partypes);
            Object arglist[] = new Object[2];
            arglist[0] = new Integer(37);
            arglist[1] = new Integer(47);
            Object retobj = ct.newInstance(arglist);
        } catch (Throwable e) {System.err.println(e);}
    }
}
```

## 用反射改变属性的值

```  java
import java.lang.reflect.#;

public class field2 {
    public double d;

    public static void main(String args[]) {
        try {
            Class cls = Class.forName("field2");
            Field fld = cls.getField("d");
            //fld.isAccessible();//非public属性要加
            field2 f2obj = new field2();
            System.out.println("d = " + f2obj.d);
            fld.setDouble(f2obj, 12.34);
            System.out.println("d = " + f2obj.d);
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
```

## 用反射使用数组

```  java
import java.lang.reflect.#;

public class Array1 {
    public static void main(String args[]) {
        try {
            Class cls = Class.forName("java.lang.String");
            Object arr = Array.newInstance(cls, 10);
            Array.set(arr, 5, "this is a test");
            String s = (String) Array.get(arr, 5);
            System.out.println(s);
        } catch (Throwable e) {
            System.err.println(e);
        }
    }
}
```

## 注释的反射的一个简单例子
``` java
public class Bean {
    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }
    
    @Column("column1")
    private String field;
}

import java.lang.annotation.#;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Column {
    String value() default "";
}

import java.lang.reflect.Field;

public class Main {
    public static void main(String[] args) {
        Class<Bean > c=Bean.class;
        Field[] fs=c.getDeclaredFields();
        for(Field f:fs){
            System.out.println(f.getName());
            System.out.println(f.isAccessible());
            Column ca=f.getAnnotation(Column.class);
            System.out.println(ca.value());
        }
    }
}
```

## 对属性赋值

``` java
import java.lang.reflect.Field;

public class Main {
    public static void main(String[] args) {
        // TODO Auto-generated method stub
        Class<Bean> c=Bean.class;
        Field[] fs=c.getDeclaredFields();
        Bean b = null;
        try {
            b=c.newInstance();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        Field f=fs[0];//由于只有一个属性所以这样测试
        System.out.println(f.getName()+"="+b.getField());
        f.setAccessible(true);//使私有属性可以被设置
        try {
            if(f.getType()==String.class)
                f.set(b, "iteedu");//将b对象的f属性设置为"iteedu"
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        System.out.println(f.getName()+"="+b.getField());
        
    }
}
```

## 根据属性调用set方法

``` java
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Main {
    public static void main(String[] args) {
        // TODO Auto-generated method stub
        Class<Bean> c = Bean.class;
        Field[] fs = c.getDeclaredFields();
        Bean b = null;
        try {
            b = c.newInstance();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        Field f = fs[0];// 由于只有一个属性所以这样测试
        System.out.println(f.getName() + "=" + b.getField());
        String fn = f.getName();
        //用属性名合成set方法名
        fn = fn.replaceFirst(fn.substring(0, 1), fn.substring(0, 1)
                .toUpperCase());
        System.out.println(fn);
        Method m = null;

        Class[] partypes = new Class[]{String.class};
        
        try {
            m = c.getMethod("set" + fn, partypes);
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
        System.out.println(m.getName());
        Object arglist[] = new Object[]{"iteedu"};

        try {
            m.invoke(b, arglist);
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }
        System.out.println(f.getName() + "=" + b.getField());

    }
}
```
