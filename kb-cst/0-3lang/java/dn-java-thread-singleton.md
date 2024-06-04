# singletion

单例对象（Singleton）是一种常用的设计模式。在Java应用中，单例对象能保证在一个JVM中，该对象只有一个实例存在。正是由于这个特点，单例对象通常作为程序中的存放配置信息的载体，因为它能保证其他对象读到一致的信息。例如在某个服务器程序中，该服务器的配置信息可能存放在数据库或文件中，这些配置数据由某个单例对象统一读取，服务进程中的其他对象如果要获取这些配置信息，只需访问该单例对象即可。这种方式极大地简化了在复杂环境下，尤其是多线程环境下的配置管理，但是随着应用场景的不同，也可能带来一些同步问题。

本文将探讨一下在多线程环境下，使用单例对象作配置信息管理时可能会带来的几个同步问题，并针对每个问题给出可选的解决办法。

## 问题描述
在多线程环境下，单例对象的同步问题主要体现在两个方面，单例对象的初始化和单例对象的属性更新。

本文描述的方法有如下假设：

1. 单例对象的属性（或成员变量）的获取，是通过单例对象的初始化实现的。也就是说，在单例对象初始化时，会从文件或数据库中读取最新的配置信息。
2. 其他对象不能直接改变单例对象的属性，单例对象属性的变化来源于配置文件或配置数据库数据的变化。

### 单例对象的初始化
首先，讨论一下单例对象的初始化同步。单例模式的通常处理方式是，在对象中有一个静态成员变量，其类型就是单例类型本身；如果该变量为null，则创建该单例类型的对象，并将该变量指向这个对象；如果该变量不为null，则直接使用该变量。

其过程如下面代码所示：

``` java
  public class GlobalConfig {
    private static GlobalConfig instance = null;
    private Vector properties = null;
    private GlobalConfig() {
      //Load configuration information from DB or file
      //Set values for properties
    }
    public static GlobalConfig getInstance() {
      if (instance == null) {
        instance = new GlobalConfig();
      }
      return instance;
    }
    public Vector getProperties() {
      return properties;
    }
  }
```

这种处理方式在单线程的模式下可以很好的运行；但是在多线程模式下，可能产生问题。如果第一个线程发现成员变量为null，准备创建对象；这是第二个线程同时也发现成员变量为null，也会创建新对象。这就会造成在一个JVM中有多个单例类型的实例。如果这个单例类型的成员变量在运行过程中变化，会造成多个单例类型实例的不一致，产生一些很奇怪的现象。例如，某服务进程通过检查单例对象的某个属性来停止多个线程服务，如果存在多个单例对象的实例，就会造成部分线程服务停止，部分线程服务不能停止的情况。

### 单例对象的属性更新
通常，为了实现配置信息的实时更新，会有一个线程不停检测配置文件或配置数据库的内容，一旦发现变化，就更新到单例对象的属性中。在更新这些信息的时候，很可能还会有其他线程正在读取这些信息，造成意想不到的后果。还是以通过单例对象属性停止线程服务为例，如果更新属性时读写不同步，可能访问该属性时这个属性正好为空（null），程序就会抛出异常。

## 解决方法

### 单例对象的初始化同步
对于初始化的同步，可以通过如下代码所采用的方式解决。

``` java
  public class GlobalConfig {
    private static GlobalConfig instance = null;
    private Vector properties = null;
    private GlobalConfig() {
      //Load configuration information from DB or file
      //Set values for properties
    }
    private static synchronized void syncInit() {
      if (instance == null) {
        instance = new GlobalConfig();
      }
    }
    public static GlobalConfig getInstance() {
      if (instance == null) {
        syncInit();
      }
      return instance;
    }
    public Vector getProperties() {
      return properties;
    }
  }
```

这种处理方式虽然引入了同步代码，但是因为这段同步代码只会在最开始的时候执行一次或多次，所以对整个系统的性能不会有影响。

### 单例对象的属性更新同步
为了解决第2个问题，有两种方法：

#### 参照读者/写者的处理方式

设置一个读计数器，每次读取配置信息前，将计数器加1，读完后将计数器减1。只有在读计数器为0时，才能更新数据，同时要阻塞所有读属性的调用。代码如下。

``` java 
  public class GlobalConfig {
    private static GlobalConfig instance;
    private Vector properties = null;
    private boolean isUpdating = false;
    private int readCount = 0;
    private GlobalConfig() {
      //Load configuration information from DB or file
      //Set values for properties
    }
    private static synchronized void syncInit() {
        if (instance == null) {
            instance = new GlobalConfig();
        }
    }
    public static GlobalConfig getInstance() {
        if (instance==null) {
            syncInit();
        }
        return instance;
    }
    public synchronized void update(String p_data) {
        syncUpdateIn();
        //Update properties
    }
    private synchronized void syncUpdateIn() {
        while (readCount > 0) {
            try {
                wait();
            } catch (Exception e) {
            }
        }
    }
    private synchronized void syncReadIn() {
        readCount++;
    }
    private synchronized void syncReadOut() {
        readCount--;
        notifyAll();
    }
    public Vector getProperties() {
        syncReadIn();
        //Process data
        syncReadOut();
        return properties;
    }
  }
```

#### 采用”影子实例”的办法
具体说，就是在更新属性时，直接生成另一个单例对象实例，这个新生成的单例对象实例将从数据库或文件中读取最新的配置信息；然后将这些配置信息直接赋值给旧单例对象的属性。如下面代码所示。

``` java 
  public class GlobalConfig {
    private static GlobalConfig instance = null;
    private Vector properties = null;
    private GlobalConfig() {
      //Load configuration information from DB or file
      //Set values for properties
    }
    private static synchronized void syncInit() {
      if (instance = null) {
        instance = new GlobalConfig();
      }
    }
    public static GlobalConfig getInstance() {
      if (instance = null) {
        syncInit();
      }
      return instance;
    }
    public Vector getProperties() {
      return properties;
    }
    public void updateProperties() {
      //Load updated configuration information by new a GlobalConfig object
      GlobalConfig shadow = new GlobalConfig();
      properties = shadow.getProperties();
    }
  }
```

注意：在更新方法中，通过生成新的GlobalConfig的实例，从文件或数据库中得到最新配置信息，并存放到properties属性中。

上面两个方法比较起来，第二个方法更好，首先，编程更简单；其次，没有那么多的同步操作，对性能的影响也不大。
