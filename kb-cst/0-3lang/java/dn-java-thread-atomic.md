# atomic

## CAS
要想理解java.util.concurrent.atomic包的原子类的原理和使用，先要理解CAS(Compare-And-Swap比较并交换)的概念。

现在大多数的处理器都提供对并发访问的支持，这个支持的反映方式就是提供硬件的指令支持多处理的特殊需求。比如检测或者阻止其它处理器的并发访问来更新共享变量的指令。

对于Intel
x86架构的处理器来说就是通过提供实现CAS或者比较并设置的硬件原语指令集。CAS操作的三个操作数：内存位置（V），预期原值（A）和新值（B）。执行的过程通常是：预测内存地址V应该包含值A，如果包含则将值B替换到位置V；否则，不更改任何值，告知地址V的当前值。CAS对待“读－修改－写”的操作一般是检测这个过程是否有其它的线程在修改变量，如果有那么这次的CAS操作失败，可以尝试重新进行CAS。

## 原子类

JDK5以后在java.util.concurrent.atomic包下提供了十几个原子类。常见的是AtomicInteger,AtomicLong,AtomicReference以及它们的数组形式，还有AtomicBoolean和为了处理ABA问题引入的AtomicStampedReference类，最后就是基于反射的对volatile变量进行更新的实用工具类：AtomicIntegerFieldUpdater,AtomicLongFieldUpdater,AtomicReferenceFieldUpdater。这些原子类理论上能够大幅的提升性能。并且java.util.concurrent内的并发集合，线程池，执行器，同步器的内部实现大量的依赖这些无锁原子类，从而争取性能的最大化。

原子类的核心方法是一个叫compareAndSet的方法，比如AtomicInteger的：

```
 public final boolean compareAndSet(int expect, int update) {
    return unsafe.compareAndSwapInt(this, valueOffset, expect, update);
    }
```

如果当前真实值为expect，那就更新其为update，成功返回true，失败返回false。

让我们看年compareAndSet怎么使用，也以AtomicInteger里的方法为例：

```
    public final int addAndGet(int delta) {
        for (;;) {
            int current = get();
            int next = current + delta;
            if (compareAndSet(current, next))
                return next;
        }
    }
```

addAndGet方法会给当前值加上delta，然后返回相加的值。他是如何保证“读-修改-写”的原子性的呢？

看代码可知，读和修改后，写会用compareAndSet，如果读和修改后在写之前，有其它线程改变了真实值，那current就和内存的值不一样了，这样会返回false，那return就不会执行，for循环会再来一次“读-修改-写”操作。直到成功完成操作，返回修改后的值。

原子类都是用这种方式修改值的，这样就保证了“读-修改-写”的原子性，这就是CAS的用途。

## 一个例子：

``` java
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import java.util.concurrent.atomic.AtomicInteger;

public class AtomicCounterSample extends Thread {
    private AtomicCounter atomicCounter;

    public AtomicCounterSample(AtomicCounter atomicCounter) {
        this.atomicCounter = atomicCounter;
    }

    @Override
    public void run() {
        long sleepTime = (long) (Math.random() * 100);
        try {
            Thread.sleep(sleepTime);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        atomicCounter.counterIncrement();
    }

    public static void main(String[] args) throws Exception {
        AtomicCounter atomicCounter = new AtomicCounter();

        for (int i = 0; i < 5000; i++) {
            new AtomicCounterSample(atomicCounter).start();
        }

        Thread.sleep(3000);

        System.out.println("counter=" + atomicCounter.getCounter());
    }
}

class AtomicCounter {
    private AtomicInteger counter = new AtomicInteger(0);

    public int getCounter() {
        return counter.get();
    }

    public void counterIncrement() {
        for (; ;) {
            int current = counter.get();
            int next = current + 1;
            if (counter.compareAndSet(current, next))
                return;
        }
    }
}

class AtomicCounter2 {
    private volatile int counter;
    private static final AtomicIntegerFieldUpdater< atomiccounter2> counterUpdater
             = AtomicIntegerFieldUpdater.newUpdater(AtomicCounter2.class, "counter");

    public int getCounter() {
        return counter;
    }

    public int counterIncrement() {
//        return counter++;
        return counterUpdater.getAndIncrement(this);
    }
}
```

这个例子实现了原子计数器的两个版本：AtomicCounter，AtomicCounter2。AtomicCounterSample作为Thread的子类对共享变量AtomicCounter或者AtomicCounter2内的counter变量进行增幅为1的递增。主函数的过程是开启5000线程，并且每个线程随机睡眠极短时间后执行递增。所以线程安全的执行结果应该是5000。

首先看版本1：AtomicCounter内的共享变量使用了Integer的原子类代替，在get()方法中不使用锁，也不用担心获取的过程中别的线程去改变counter的值，因为这些原子类可以看成volatile的范化扩展，可见性能够保证。而在counterIncrement()方法中揭示了使用原子类的重要技巧：循环结合CAS。这个技巧可以帮助我们实现复杂的非阻塞并发集合。方法中的counter.compareAndSet(current,next)就是原子类使用的精髓－－CAS操作。compareAndSet(...)可以说是原子类搭积木的原材料，在循环中使用它可以让我们的并发程序昂首挺胸。

再看版本2：AtomicCounter2内有个volatile的共享变量counter，并且有个类变量counterUpdater作为counter的更新器。在counterIncrement()里注释掉的代码是非线程安全的。而counterUpdater.getAndIncrement(this)的内部实现其实和版本1的几乎一样。唯一不同的是通过反射找到要原子操作更新的变量counter，但是“循环+CAS”的精髓是一样的。

最后看看结果吧：版本1和版本2的无锁同步的执行分别20次均是5000，正确。版本2把无锁同步的代码注释，把已注释的非线程安全的代码还原执行，平均每10次大概有1～2次出现<5000的数字。这个例子侧面证明了++的原子性操作非线程安全是保证不了的。因为“读－修改－写”的操作碰到如下场景：线程A“读－修改”后“写”之前，线程B完成“读－修改－写”。这时候A,B的写值是重复的，这就造成了结果<5000，又杯具了...
