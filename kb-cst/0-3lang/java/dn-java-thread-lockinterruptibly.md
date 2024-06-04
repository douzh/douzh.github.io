# lock interrupt

本文是对ReentrantLock可中断锁的介绍。可中断锁是通过ReentrantLock提供的lockInterruptibly()方法实现的。

## 响应中断是什么意思?
比如A、B两线程去竞争锁，A得到了锁，B等待，但是A有很多事情要处理，所以一直不返回。B可能就会等不及了，想中断自己，不再等待这个锁了，转而处理其他事情。在这种情况下，synchronized的做法是，B线程中断自己（或者别的线程中断它），我不去响应，继续让B线程等待，你再怎么中断，我全当耳边风。而lockInterruptibly()的做法是，B线程中断自己（或者别的线程中断它），ReentrantLock响应这个中断，不再让B等待这个锁的到来。有了这个机制，使用ReentrantLock时死锁了线程可以中断自己来解除死锁。

## 什么叫做中断自己？
比如A、B两线程去竞争锁，它们肯定是被父线程创建并启动的，那父线程一定有它们的引用。线程都有interrupt()方法，假设父线程创建的线程B的引用是b，那b.interrupt()就是中断自己。

## 怎么中断自己
lock.lockInterruptibly()，这个方法会抛出异常InterruptedException。

什么时候抛出异常呢？当调用interrupt()方法自我中断的时候。

这时线程就进入了中断处理的过程，不会再等待锁了。

至于异常处理是怎样的，有很多种选择呀。比如可以退出线程的run()方法使线程完结，也可以使线程处理另外的事情。

## 一个中断锁的例子

``` java
import java.util.concurrent.locks.ReentrantLock;

public class ReentrantLockSample {

    public static void main(String[] args) {
        testSynchronized();
        testReentrantLock();
    }

    public static void testReentrantLock() {
        final SampleSupport1 support = new SampleSupport1();
        Thread first = new Thread(new Runnable() {
            public void run() {
                try {
                    support.doSomething();
                }
                catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        },"first");

        Thread second = new Thread(new Runnable() {
            public void run() {
                try {
                    support.doSomething();
                }
                catch (InterruptedException e) {
                    System.out.println("Second Thread Interrupted without executing counter++,beacuse it waits a long time.");
                }
            }
        },"second");

        executeTest(first, second);
    }

    public static void testSynchronized() {
        final SampleSupport2 support2 = new SampleSupport2();

        Runnable runnable = new Runnable() {
            public void run() {
                support2.doSomething();
            }
        };

        Thread third = new Thread(runnable,"third");
        Thread fourth = new Thread(runnable,"fourth");

        executeTest(third, fourth);
    }

    /**
     * Make thread a run faster than thread b,
     * then thread b will be interruted after about 1s.
     * @param a
     * @param b
     */
    public static void executeTest(Thread a, Thread b) {
        a.start();
        try {
            Thread.sleep(100);
            b.start(); 
            Thread.sleep(1000);
            b.interrupt(); 
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

abstract class SampleSupport {

    protected int counter;

    /**
     * A simple countdown,it will stop after about 5s. 
     */
    public void startTheCountdown() {
        long currentTime = System.currentTimeMillis();
        for (;;) {
            long diff = System.currentTimeMillis() - currentTime;
            if (diff > 5000) {
                break;
            }
        }
    }
}

class SampleSupport1 extends SampleSupport {

    private final ReentrantLock lock = new ReentrantLock();

    public void doSomething() throws InterruptedException {
        lock.lockInterruptibly();
//        try {
//            lock.lockInterruptibly();
//        } catch (InterruptedException e) {
//            //做一些其它的事，不结束线程
//        }
        System.out.println(Thread.currentThread().getName() + " will execute counter++.");
        startTheCountdown();
        try {
            counter++;
        }
        finally {
            lock.unlock();
        }
    }
}

class SampleSupport2 extends SampleSupport {

    public synchronized void doSomething() {
        System.out.println(Thread.currentThread().getName() + " will execute counter++.");
        startTheCountdown();
        counter++;
    }
}
```

## 中断介绍
中断（Interrupt）一个线程意味着在该线程完成任务之前停止其正在进行的一切，有效地中止其当前的操作。线程是死亡、还是等待新的任务或是继续运行至下一步，就取决于这个程序。虽然初次看来它可能显得简单，但是，你必须进行一些预警以实现期望的结果。

从上面的介绍知道，interrupt()并不会使线程停止运行，那如何停止线程呢？

中断线程最好的，最受推荐的方式是，使用共享变量（shared）发出信号，告诉线程必须停止正在运行的任务。线程必须周期性的核查这一变量（尤其在冗余操作期间），然后有秩序地中止任务。

``` java 
class Example2 extends Thread {
    volatile boolean stop = false;

    public static void main(String args[]) throws Exception {
        Example2 thread = new Example2();
        System.out.println("Starting thread...");
        thread.start();
        Thread.sleep(3000);
        System.out.println("Asking thread to stop...");

        thread.stop = true;
        Thread.sleep(3000);
        System.out.println("Stopping application...");
        //System.exit( 0 );
    }

    public void run() {
        while (!stop) {
            System.out.println("Thread is running...");
            long time = System.currentTimeMillis();
            while ((System.currentTimeMillis() - time < 1000) && (!stop)) {
            }
        }
        System.out.println("Thread exiting under request...");
    }
}
```
