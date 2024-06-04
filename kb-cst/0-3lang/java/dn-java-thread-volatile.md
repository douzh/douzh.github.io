
# volatile
*Java* 语言中的 *volatile* 变量可以被看作是一种"程度较轻的synchronized"；与 synchronized块相比，volatile变量所需的编码较少，并且运行时开销也较少，但是它所能实现的功能也仅是synchronized的一部分。

我们知道，在Java中设置变量值的操作，除了long和double类型的变量外都是原子操作，也就是说，对于变量值的简单读写操作没有必要进行同步。

这在JVM1.2之前，Java的内存模型实现总是从主存读取变量，是不需要进行特别的注意的。而随着JVM的成熟和优化，现在在多线程环境下volatile关键字的使用变得非常重要。

在当前的Java内存模型下，线程可以把变量保存在本地内存（比如机器的寄存器）中，而不是直接在主存中进行读写。这就可能造成一个线程在主存中修改了一个变量的值，而另外一个线程还继续使用它在寄存器中的变量值的拷贝，造成数据的不一致。

要解决这个问题，只需要像在本程序中的这样，把该变量声明为volatile（不稳定的）即可，这就指示JVM，这个变量是不稳定的，每次使用它都到主存中进行读取。一般说来，多任务环境下各任务间共享的标志都应该加volatile修饰。

Volatile修饰的成员变量在每次被线程访问时，都强迫从共享内存中重读该成员变量的值。而且，当成员变量发生变化时，强迫线程将变化值回写到共享内存。这样在任何时刻，两个不同的线程总是看到某个成员变量的同一个值。

Java语言规范中指出：为了获得最佳速度，允许线程保存共享成员变量的私有拷贝，而且只当线程进入或者离开同步代码块时才与共享成员变量的原始值对比。

这样当多个线程同时与某个对象交互时，就必须要注意到要让线程及时的得到共享成员变量的变化。

而volatile关键字就是提示VM：对于这个成员变量不能保存它的私有拷贝，而应直接与共享成员变量交互。

使用建议：在两个或者更多的线程访问的成员变量上使用volatile。当要访问的变量已在synchronized代码块中，或者为常量时，不必使用。

由于使用屏蔽掉了VM中必要的代码优化，所以在效率上比较低，因此一定在必要时才使用此关键字。

## 示例

``` java
public class Counter {

    public static int count = 0;
    public static void inc() {
        //这里延迟1毫秒，使得结果明显
        try {
            Thread.sleep(1);
        } catch (InterruptedException e) {

        }
        count++;
    }

    public static void main(String[] args) {
        //同时启动1000个线程，去进行i++计算，看看实际结果

        for (int i = 0; i < 1000; i++) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    Counter.inc();
                }
            }).start();
        }
        //这里每次运行的值都有可能不同,可能为1000
        System.out.println("运行结果:Counter.count=" + Counter.count);

    }
}
```

运行结果:Counter.count=995

实际运算结果每次可能都不一样，本机的结果为：运行结果:Counter.count=995，可以看出，在多线程的环境下，Counter.count并没有期望结果是1000

很多人以为，这个是多线程并发问题，只需要在变量count之前加上volatile就可以避免这个问题，那我们在修改代码看看，看看结果是不是符合我们的期望

``` java
public class Counter {
    public volatile static int count = 0;
    public static void inc() {

        //这里延迟1毫秒，使得结果明显
        try {
            Thread.sleep(1);
        } catch (InterruptedException e) {

        }
        count++;
    }
    public static void main(String[] args) {
        //同时启动1000个线程，去进行i++计算，看看实际结果
        for (int i = 0; i < 1000; i++) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    Counter.inc();
                }
            }).start();
        }

        //这里每次运行的值都有可能不同,可能为1000
        System.out.println("运行结果:Counter.count=" + Counter.count);
    }
}
```

运行结果:Counter.count=992

运行结果还是没有我们期望的1000，下面我们分析一下原因

在 java垃圾回收整理一文中，描述了jvm运行时刻内存的分配。其中有一个内存区域是jvm虚拟机栈，每一个线程运行时都有一个线程栈，线程栈保存了线程运行时候变量值信息。当线程访问某一个对象时候值的时候，首先通过对象的引用找到对应在堆内存的变量的值，然后把堆内存变量的具体值load到线程本地内存中，建立一个变量副本，之后线程就不再和对象在堆内存变量值有任何关系，而是直接修改副本变量的值，在修改完之后的某一个时刻（线程退出之前），自动把线程变量副本的值回写到对象在堆中变量。这样在堆中的对象的值就产生变化了。下面一幅图

描述这写交互

[[/images/java/counter.jpg][]]

read and load 从主存复制变量到当前工作内存 use andassign执行代码，改变共享变量值 store and write用工作内存数据刷新主存相关内容

其中use and assign 可以多次出现但是这一些操作并不是原子性，也就是在readload之后，如果主内存count变量发生修改之后，线程工作内存中的值由于已经加载，不会产生对应的变化，所以计算出来的结果会和预期不一样对于volatile修饰的变量，jvm虚拟机只是保证从主内存加载到线程工作内存的值是最新的

例如假如线程1，线程2在进行read,load操作中，发现主内存中count的值都是5，那么都会加载这个最新的值

在线程1堆count进行修改之后，会write到主内存中，主内存中的count变量就会变为6

线程2由于已经进行read,load操作，在进行运算之后，也会更新主内存count的变量值为6

导致两个线程及时用volatile关键字修改之后，还是会存在并发的情况。
