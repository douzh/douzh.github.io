# Executor

![]()./assets/Executor-class.png)

Java里面线程池的顶级接口是Executor，但是严格意义上讲Executor并不是一个线程池，而只是一个执行线程的工具。*真正的线程池接口是ExecutorService*。

1. =ExecutorService=：真正的线程池接口。
2. =ScheduledExecutorService=:能和Timer/TimerTask类似，解决那些需要任务重复执行的问题。
3. =ThreadPoolExecutor=: ExecutorService的默认实现。
4. =ScheduledThreadPoolExecutor=:继承=ThreadPoolExecutor=的=ScheduledExecutorService=接口实现，周期性任务调度的类实现。

## 使用步骤
1. 创建执行对象，要实现=Runnable=接口，用线程类Thread也一样。
2. 用Executors类或ThreadPoolExecutor创建线程池=ExecutorService=。
3. 用ExecutorService的execute(Runnable)添加任务。
4. 用ExecutorService的shutdown()关闭线程池，使之不可再加入新线程。
5. 如是有必要，用ExecutorService的awaitTermination(long timeout,TimeUnit unit)方法阻塞主线程，等待所有任务执行结束。

## Executors类
Executors类里面提供了一些静态工厂，生成一些常用的线程池。

newSingleThreadExecutor

创建一个单线程的线程池。这个线程池只有一个线程在工作，也就是相当于单线程串行执行所有任务。如果这个唯一的线程因为异常结束，那么会有一个新的线程来替代它。此线程池保证所有任务的执行顺序按照任务的提交顺序执行。

```
ExecutorService pool = Executors.newSingleThreadExecutor();
```

newFixedThreadPool

创建固定大小的线程池。每次提交一个任务就创建一个线程，直到线程达到线程池的最大大小。线程池的大小一旦达到最大值就会保持不变，如果某个线程因为执行异常而结束，那么线程池会补充一个新线程。

```
ExecutorService pool = Executors.newFixedThreadPool(corePoolSize);
```

newCachedThreadPool

创建一个可缓存的线程池。如果线程池的大小超过了处理任务所需要的线程，那么就会回收部分空闲（60秒不执行任务）的线程，当任务数增加时，此线程池又可以智能的添加新线程来处理任务。此线程池不会对线程池大小做限制，线程池大小完全依赖于操作系统（或者说JVM）能够创建的最大线程大小。

```
ExecutorService pool = Executors.newCachedThreadPool();
```

newScheduledThreadPool

创建一个大小无限的线程池。此线程池支持定时以及周期性执行任务的需求。

```
ExecutorService pool = Executors.newScheduledThreadPool(corePoolSize);
```

newSingleThreadScheduledExecutor

创建一个单线程的线程池。此线程池支持定时以及周期性执行任务的需求。

```
ExecutorService pool = Executors.newSingleThreadScheduledExecutor();
```

### 示例

*固定线程池：*

``` java 
package BackStage;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
public class JavaThreadPool {
    public static void main(String[] args) {
    // 创建一个可重用固定线程数的线程池
    ExecutorService pool = Executors.newFixedThreadPool(2);
    // 创建实现了Runnable接口对象，Thread对象当然也实现了Runnable接口
    Thread t1 = new MyThread();
    Thread t2 = new MyThread();
    Thread t3 = new MyThread();
    Thread t4 = new MyThread();
    Thread t5 = new MyThread();
    // 将线程放入池中进行执行
    pool.execute(t1);
    pool.execute(t2);
    pool.execute(t3);
    pool.execute(t4);
    pool.execute(t5);
    // 关闭线程池
    pool.shutdown();
    //等待任务执行完成，最长阻塞60秒
    pool.awaitTermination(60, TimeUnit.SECONDS);
    }
}
class MyThread extends Thread {
    @Override
    public void run() {
    System.out.println(Thread.currentThread().getName() + "正在执行。。。");
    }
}
```

*定时线程池：*

``` java 
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Java线程：线程池
 */
public class Test
{
    public static void main(String[] args)
    {
        // 创建一个线程池，它可安排在给定延迟后运行命令或者定期地执行。
        ScheduledExecutorService pool = Executors.newScheduledThreadPool(2);
       // 创建实现了Runnable接口对象，Thread对象当然也实现了Runnable接口
        Thread t1 = new MyThread();
        Thread t2 = new MyThread();
        Thread t3 = new MyThread();
        Thread t4 = new MyThread();
        Thread t5 = new MyThread();
        // 将线程放入池中进行执行
        pool.execute(t1);
        pool.execute(t2);
        pool.execute(t3);
        // 使用延迟执行风格的方法
        //t4,t5每10秒执行一次
        pool.schedule(t4, 10, TimeUnit.SECONDS); 
        pool.schedule(t5, 10, TimeUnit.SECONDS);
        // 关闭线程池
        pool.shutdown();
    }
}

class MyThread extends Thread
{
    @Override
    public void run()
    {
        System.out.println(Thread.currentThread().getName() + "正在执行。。。");
    }
}
```

## ThreadPoolExecutor
```
public ThreadPoolExecutor(int corePoolSize,
                           int maximumPoolSize,
                           long keepAliveTime,
                           TimeUnit unit,
                           BlockingQueue< runnable> workQueue,
                           ThreadFactory threadFactory,
                           RejectedExecutionHandler handler)
```

参数：

1. corePoolSize -- 池中所保存的线程数，包括空闲线程。
2. maximumPoolSize -- 池中允许的最大线程数。
3. keepAliveTime -- 当线程数大于核心时，此为终止前多余的空闲线程等待新任务的最长时间。
4. unit -- keepAliveTime 参数的时间单位。
5. workQueue -- 执行前用于保持任务的队列。此队列仅保持由
   execute方法提交的 Runnable任务。
6. threadFactory -- 执行程序创建新线程时使用的工厂。
7. handler -- 由于超出线程范围和队列容量而使执行被阻塞时所使用的处理程序。

=unit=可选的参数为=java.util.concurrent.TimeUnit=中的几个静态属性：

```
NANOSECONDS、MICROSECONDS、MILLISECONDS、SECONDS
```

workQueue常用的是：

```
java.util.concurrent.ArrayBlockingQueue
```

*handler有四个选择：*

1. ThreadPoolExecutor.AbortPolicy() 抛出java.util.concurrent.RejectedExecutionException异常
2. ThreadPoolExecutor.CallerRunsPolicy() 重试添加当前的任务，他会自动重复调用execute()方法
3. ThreadPoolExecutor.DiscardOldestPolicy() 抛弃旧的任务
4. ThreadPoolExecutor.DiscardPolicy() 抛弃当前的任务

*示例：*

```
//这句话的意思是
//初始线程池是3各线程
//如果一下子任务过多，就创建线程但是不能超过5各
//也就是说最多可以一次处理5各任务
//如果一次来30各任务，第一次只能处理5个任务
//剩下的任务中有4各可以放到队列里面
//如果队列都放不下，拒绝接受任务
//因此来了30各任务，只能处理9个
ThreadPoolExecutor te = new ThreadPoolExecutor(3, 5, 5,
         TimeUnit.SECONDS, new ArrayBlockingQueue<runnable>(4),
         new ThreadPoolExecutor.DiscardOldestPolicy());
```

## 源码分析

*** ScheduledThreadPoolExecutor

ScheduledThreadPoolExecutor最终提交的runnable接口会封装成ScheduledFutureTask，主要对FutureTask添加了几个字段扩展

- sequenceNumber
- time: 定时的时间设置
- period: 0为不循环 正为周期循环 负为延时执行
- outerTask
- heapIndex
