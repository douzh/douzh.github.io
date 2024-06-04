# CountDownLatch

## 一、CountDownLatch简介
CountDownLatch(闭锁)是一个同步协助类，允许一个线程或多个线程阻塞等待，直到其他线程完成操作后，被阻塞的线程才会被唤醒，然后执行后面的业务逻辑。

CountDownLatch的构造方法需要给定一个计数值(count)进行初始化。CountDownLatch简单理解就是一个倒计数器，调用它的=await()=会被阻塞，直到它的计数值(count)为0时，被阻塞的线程才会被唤醒，而调用它的=countDown()=方法会对计数器值=-1=，直到计数值为0后，之前被阻塞的线程都会被唤醒，*同时后续再调用=await()=方法时就不会再被阻塞，因为计数值=count=是一次性的，当它的值减为0后就不会再变化了，所以后续调用=await()=方法时不会被阻塞，而是立即返回*。

如果使用场景中，需要计数值能够被重置，可以考虑使用=CyclicBarrier=，关于=CyclicBarrier=会在下一篇文章进行详细介绍。


## 二、使用场景
CountDownLatch一般用于多线程倒计时的计数器，强制它们等待其他一组线程(由CountDownLatch的初始化决定)任务执行完成。

CountDownLatch有两种使用场景：

- 让多个线程等待
- 让单个线程等待

### 2.1 多线程等待：并发线程同时执行
这类场景就很像田径远动员比赛，每一个运动员就是一个线程，然后发号员是一个主线程，每个远动员准备就绪后都需要等待发号员发令才能开始，发号员准备完毕后开枪，然后所有远动员同时开始开跑。

下面的代码就模拟了远动员比赛的场景：

三个远动员都阻塞在调用=await()=方法这里，当发号员准备两秒后发令，这是三个远动员同时往下进行

``` java
CountDownLatch countDownLatch = new CountDownLatch(1);

for (int num = 0; num < 3; num++){
    new Thread(() ->{
        try {
            //准备完毕,运动员都阻塞在这里,等待号令
            countDownLatch.await();
            System.out.println(Thread.currentThread().getName()+"开始跑……");
        } catch (InterruptedException e) {
            log.error(e);
        }
    }).start();
}
// 发号员准备发令
Thread.sleep(2000);
// 发令枪：发令
countDownLatch.countDown();
```

### 2.2 单线程等待：并发线程完成后合并
在有些场景中，并发任务前后存在依赖关系，比如数据详情页需要同时调用多个接口获取数据，并发请求获取到数据后，需要对结果进行合并。这就是典型的并发线程完成后需要合并的场景。

等待合并的线程调用=await()=方法进行阻塞等待，当每个任务执行完成后都会调用=countDown()=方法，将计数值-1，当最后一个线程执行完任务后，计数值被减为0，这个时候就去唤醒等待汇总的线程。

``` java
CountDownLatch countDownLatch = new CountDownLatch(3);
for (int num = 0; num < 3; num++){
    new Thread(() ->{
        try {
            Thread.sleep(1000+ ThreadLocalRandom.current().nextInt(1000));
            System.out.println(Thread.currentThread().getName() + "finish task");

            countDownLatch.countDown();
        } catch (InterruptedException e) {
            log.error(e);
        }
    }).start();
}

countDownLatch.await();
System.out.println("所有线程执行完成,对结果进行汇总");
```

## 三、源码分析
在理解CountDownLatch的源码之前，可以先看一下[[https://blog.csdn.net/sermonlizhi/article/details/122707988?spm=1001.2014.3001.5501][《Semaphore源码分析》]]这篇文章，后面理解CountDownLatch的源码就非常简单了。

CountDownLatch的源码相比ReentrantLock和Semaphore要简单很多，因为它就是一个到计数器的功能，所以也不会存在公平与非公平的概念，同时它的底层实现与Semaphore是一模一样的，只是它重写了=tryAcquireShared()=和=tryReleaseShared()=两个方法

CountDownLatch同样也是基于AbstractQueuedSynchronizer实现的，所以的通过内部类=Sync=继承了AbstractQueuedSynchronizer，然后使用AQS的=state=属性来记录=count=的计数值。

CountDownLatch的整个结构是非常简单了，它只提供了以下几个方法：

``` java
// 构造方法需要指定count值
public CountDownLatch(int count) {
    if (count < 0) throw new IllegalArgumentException("count < 0");
    this.sync = new Sync(count);
}

public void await() throws InterruptedException {
    sync.acquireSharedInterruptibly(1);
}

// 阻塞等待timeout时长后,count值还没变为0,则不再等待,继续执行
public boolean await(long timeout, TimeUnit unit)
    throws InterruptedException {
    return sync.tryAcquireSharedNanos(1, unit.toNanos(timeout));
}

// 将count值-1,直至为0
public void countDown() {
    sync.releaseShared(1);
}

public long getCount() {
    return sync.getCount();
}
```

### 3.1 阻塞等待
在上面方法的源码中，可以看到=await()=方法是调用的内部类=Sync=的=acquireSharedInterruptibly()=方法，该方法在AQS中已经实现，=Semaphore=获取共享资源时也是调用该方法

``` java
public final void acquireSharedInterruptibly(int arg)
    throws InterruptedException {
    if (Thread.interrupted())
        throw new InterruptedException();
    if (tryAcquireShared(arg) < 0)
        doAcquireSharedInterruptibly(arg);
}
```

是否需要进行阻塞的核心在于=tryAcquireShared()=，只要这个方法的返回值小于-1，就会调用=doAcquireSharedInterruptibly()=方法将线程进行阻塞

而CountDownLatch的内部类=Sync=中对=tryAcquireShared()=方法的实现也非常简单，只要=count=的数值不等于0，就返回-1，表示需要进行阻塞。

``` java
protected int tryAcquireShared(int acquires) {
    return (getState() == 0) ? 1 : -1;
}
```

然后就是去调用=doAcquireSharedInterruptibly()=对线程进行阻塞，该方法的详细介绍在=Semaphore=源码分析的文章中已经做了详细的介绍。

### 3.2 唤醒执行
CountDownLatch中因调用=awati()=方法被阻塞的线程，能否被唤醒完全取决于=countDown()=方法，该方法会使=count=的计数值-1。

CountDownLatch的=countDown()=方法调用的是=Sync=的=releaseShared()=方法，同样，该方法也在AQS中已经实现了

``` java
public final boolean releaseShared(int arg) {
    if (tryReleaseShared(arg)) {
        doReleaseShared();
        return true;
    }
    return false;
}
```

能不能调用=doReleaseShared()=唤醒线程，取决于CountDownLatch中=Sync=的=tryReleaseShared()=方法

该方法中，先判断=count=的值是否为0，如果为0说明已经唤醒过了，不需要重复唤醒，所以直接返回false

如果不为0，就利用自旋+CAS将=state=的属性值-1，修改后的=count=值为0，就会返回true

``` java
protected boolean tryReleaseShared(int releases) {
    // Decrement count; signal when transition to zero
    for (;;) {
        int c = getState();
        if (c == 0)
            return false;
        int nextc = c-1;
        if (compareAndSetState(c, nextc))
            return nextc == 0;
    }
}
```

返回true之后，就会调用AQS的=doReleaseShared()=唤醒线程，该方法也已经在=Semaphore=的源码中详细介绍过了。

从上面CountDownLatch的源码可以看出，它完全就是借助=Semaphore=的特性(=被唤醒的线程会去尝试唤醒后面的线程=)来唤醒所有被阻塞的线程

*总结：从功能上来看，CountDownLatch的功能与=Thread.join()=方法非常相似，都是等待其他线程执行完成之后再执行后续的逻辑，但在实现上，CountDownLatch提供了比=join()=方法更灵活的API。同时CountDownLatch即可以手动控制在一个线程调用多次=countDown()=方法，也可以在多个线程调用多次。=join()=方法的实现原理是不停检查join线程是否存在，只要存活就让当前线程一直阻塞。*
