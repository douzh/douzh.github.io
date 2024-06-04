[toc]

# java spi机制

## 什么是SPI

SPI全称Service Provider Interface，是Java提供的一套用来被第三方实现或者扩展的接口，它可以用来启用框架扩展和替换组件。 SPI的作用就是为这些被扩展的API寻找服务实现。

## SPI的简单实现

```java
public interface IA {
    void sayHello();
}
public interface IB {
    void sayHello();
}
```

在resources目录下新建META-INF/services目录，并且在这个目录下新建一个与上述接口的全限定名一致的文件，在这个文件中写入接口的实现类的全限定名：

文件：com.iteedu.javaspi.intf.IA

```
com.iteedu.javaspi.intf.impl.IA1
com.iteedu.javaspi.intf.impl.IA2
```

文件：com.iteedu.javaspi.intf.IB

```
com.iteedu.javaspi.intf.impl.IB1
com.iteedu.javaspi.intf.impl.IB2
```

## 调用

```java
public class MainApp {
    public static void main(String[] args) {
        ServiceLoader<IA> ia=ServiceLoader.load(IA.class);
        for (IA ia1 : ia) {
            ia1.sayHello();
        }
        ServiceLoader<IB> ib=ServiceLoader.load(IB.class);
        for (IA ia1 : ia) {
            ia1.sayHello();
        }

    }
}
```

输出

```
IA1 hello
IA2 hello
IA1 hello
IA2 hello
```

## 总结

以META-INF/services下的文件名映射接口全限定名，以文件内容指定接口的实现类，以ServiceLoader自动加载创建实例。

不足之处：不能关联装配。

## DriverManager

java.sql.DriverManager

```java
static {
        loadInitialDrivers();
        println("JDBC DriverManager initialized");
}
private static void loadInitialDrivers() {
        String drivers;
        try {
            drivers = AccessController.doPrivileged(new PrivilegedAction<String>() {
                public String run() {
                    return System.getProperty("jdbc.drivers");
                }
            });
        } catch (Exception ex) {
            drivers = null;
        }
        AccessController.doPrivileged(new PrivilegedAction<Void>() {
            public Void run() {

                ServiceLoader<Driver> loadedDrivers = ServiceLoader.load(Driver.class);
                Iterator<Driver> driversIterator = loadedDrivers.iterator();
                try{
                    while(driversIterator.hasNext()) {
                        driversIterator.next();
                    }
                } catch(Throwable t) {
                // Do nothing
                }
                return null;
            }
        });

        println("DriverManager.initialize: jdbc.drivers = " + drivers);

        if (drivers == null || drivers.equals("")) {
            return;
        }
        String[] driversList = drivers.split(":");
        println("number of Drivers:" + driversList.length);
        for (String aDriver : driversList) {
            try {
                println("DriverManager.Initialize: loading " + aDriver);
                Class.forName(aDriver, true,
                        ClassLoader.getSystemClassLoader());
            } catch (Exception ex) {
                println("DriverManager.Initialize: load failed: " + ex);
            }
        }
    }
```

