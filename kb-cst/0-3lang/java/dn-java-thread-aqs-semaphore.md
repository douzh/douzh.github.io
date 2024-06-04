# Semaphore
## 一、Semaphore介绍
Semaphore，俗称信号量，它是操作系统PV操作原语在JDK中的实现，同样，它也是基于AbstractQueuedSynchronizer来实现的。

Semaphore通俗理解就是常说的共享锁，它可以定义共享资源的个数，只要共享资源还有，其他线程就可以执行，否则就会被阻塞。

而关于操作系统中PV操作的原语可以参考[《详解进程同步》](https://blog.csdn.net/sermonlizhi/article/details/119234232)这篇文章中对信号量的详细介绍

Semaphore的功能非常强大，大小为1的信号量，它的功能就类似于互斥锁(ReentrantLock)，同时只能有一个线程可以获取信号量，进而执行线程自己的业务逻辑。而大小为n的信号量，可以实现限流的功能，即它可以保证只能有n个线程可以同时获取信号量来执行业务逻辑。

## 二、Semaphore使用
Semaphore可以实现限流功能

下面的代码定义了一个共享资源数为3的Semaphore，并创建了一个线程池

在=test()=方法中，通过=sleep()=方法来控制每秒只提交10个任务，而在=exec()=方法中，可以适当将休眠的时间调大一点儿，这样可以比较清晰的看到执行情况

由于信号量为3，所以同时只能有三个线程可以调用=acquire()=方法成功，然后执行后面的逻辑，当第四个线程进来的时候，调用=acquire()=方法就会被阻塞，直到前面有线程执行完释放了共享资源

``` java
private static Semaphore semaphore = new Semaphore(3);
private static ThreadPoolExecutor executorService = new ThreadPoolExecutor(10,50,60, TimeUnit.SECONDS,new LinkedBlockingQueue<>(200));

@SneakyThrows
static void exec(){
    try {
        semaphore.acquire();
        System.out.println("执行exec方法");
        Thread.sleep(10000);
    } finally {
        semaphore.release();
    }
}

@Test
@SneakyThrows
public void test(){
    for (;;){
        Thread.sleep(100);
        executorService.execute(() -> exec());
    }
}
```

## 三、Semaphore源码分析
### 3.1 常用方法
Semaphore同样使用内部类=Sync=继承了AbstractQueuedSynchronizer，同时也提供了=NonfairSync=和=FairSync=两个公平和非公平的内部类，在这一点上与ReentrantLock如出一辙。

它的构造方法如下，默认使用非公平的方式，参数=permits=表示许可证的数量(即共享资源的个数)，构造方法会去调用=Sync=的构造方法，然后再调用AQS的=setState()=方法，使用=state=属性来记录资源个数

``` java
public Semaphore(int permits) {
    sync = new NonfairSync(permits);
}

public Semaphore(int permits, boolean fair) {
    sync = fair ? new FairSync(permits) : new NonfairSync(permits);
}

Sync(int permits) {
    setState(permits);
}
```

其中常用的方法如下：

``` java
public void acquire() throws InterruptedException
public boolean tryAcquire()
public void release()
public int availablePermits()
public final int getQueueLength() 
public final boolean hasQueuedThreads()
protected void reducePermits(int reduction)
```

- *acquire()*：获取许可成功，直接返回；如果失败，需要将添加都同步队列进行阻塞
- *tryAcquire()*：尝试获取许可，不论成功与否都立即返回，获取失败就返回false，并不会阻塞获线程
- *release()*：释放许可并唤醒同步队列中阻塞的线程
- *availablePermits()*：返回信号量中当前可用的许可证数
- *getQueueLength()*：返回正在等待获取许可证的线程数
- *hasQueuedThreads()*：是否有线程正在等待获取许可证
- *reducePermits()*：减少指定数量的许可证

### 3.2 获取许可证
Semaphore中的=acquire()=方法是有两个重载方法的，可以指定需要获取的许可证数，它们内部都是会去调用AQS的=acquireSharedInterruptibly()=方法

``` java
public void acquire() throws InterruptedException {
    sync.acquireSharedInterruptibly(1);
}

public void acquire(int permits) throws InterruptedException {
    if (permits < 0) throw new IllegalArgumentException();
    sync.acquireSharedInterruptibly(permits);
}
```

在=acquireSharedInterruptibly()=方法中，会去调用=tryAcquireShared()=方法去尝试获取指定数量的许可，如果失败就回去调用=doAcquireSharedInterruptibly()=方法来进行阻塞

由于Semaphore的=acquire()=方法不支持线程中断，所以当检测到线程中断时就会直接抛出中断异常

而它还有一个=acquireUninterruptibly()=方法，这个方法是支持中断的

``` java
public final void acquireSharedInterruptibly(int arg)
    throws InterruptedException {
    if (Thread.interrupted())
        throw new InterruptedException();
    if (tryAcquireShared(arg) < 0)
        doAcquireSharedInterruptibly(arg);
}
```

#### 3.2.1 尝试获取许可
AQS中并没有具体去实现=tryAcquireShared()=方法，都是交由子类按需实现，以Semaphore内部类=FairSync=的=tryAcquireShared()=方法来看源码

``` java
protected int tryAcquireShared(int acquires) {
    for (;;) {
        if (hasQueuedPredecessors())
            return -1;
        int available = getState();
        int remaining = available - acquires;
        if (remaining < 0 ||
            compareAndSetState(available, remaining))
            return remaining;
    }
}
```

上面这段代码也是比较简单的，首先判断一下同步队列中是否已经有等待线程了，如果有，那么当前线程肯定无法获取许可的，直接返回-1，继续后面入队的方法

如果没有，则计算剩余许可证数，如果大于等于0，就更新剩余许可证数量，最后返回剩余许可数，当返回负数时，说明获取许可失败

#### 3.2.2 线程入队并阻塞
如果尝试获取许可时返回得是负数，说明失败了，就要调用=doAcquireSharedInterruptibly()=方法来进行入队和阻塞

``` java
private void doAcquireSharedInterruptibly(int arg)
    throws InterruptedException {
    final Node node = addWaiter(Node.SHARED);
    boolean failed = true;
    try {
        for (;;) {
            final Node p = node.predecessor();
            if (p == head) {
                int r = tryAcquireShared(arg);
                if (r >= 0) {
                    setHeadAndPropagate(node, r);
                    p.next = null; // help GC
                    failed = false;
                    return;
                }
            }
            if (shouldParkAfterFailedAcquire(p, node) &&
                parkAndCheckInterrupt())
                throw new InterruptedException();
        }
    } finally {
        if (failed)
            cancelAcquire(node);
    }
}
```

=doAcquireSharedInterruptibly()=方法与ReentrantLock中=acquireQueued()=方法的整体逻辑是一样的，只是实现上有个别不同，可以先看一下[《AQS&ReentrantLock源码解析》](https://blog.csdn.net/sermonlizhi/article/details/122702520)中对ReentrantLock的=acquireQueued()=方法的介绍

在调用=addWaiter()=方法时，ReentrantLock中创建的是一个独占类型的节点，而Semaphore中创建的是一个共享类型的节点。

最大的不同在于尝试获取许可的地方，在ReentrantLock中，如果获取同步状态成功，就直接从同步队列中移除当前节点即可；而在Semaphore中，不仅会移除当前节点，它还会尝试去唤醒下一个阻塞节点的线程

其实现就是在=setHeadAndPropagate()=方法中，首先也是调用=setHead()=方法将当前节点先作为头节点，然后如果当前节点的下一个节点是共享类型的节点，就会去调用=doReleaseShared()=尝试去唤醒下一个节点的线程

``` java
private void setHeadAndPropagate(Node node, int propagate) {
    Node h = head; // Record old head for check below
    setHead(node);
    
    if (propagate > 0 || h == null || h.waitStatus < 0 ||
        (h = head) == null || h.waitStatus < 0) {
        Node s = node.next;
        if (s == null || s.isShared())
            doReleaseShared();
    }
}
```

之所以采用传播唤醒的方式是因为，前面释放许可的线程可能释放了多个许可，而阻塞队列中的第一个共享节点获取到满足的许可数后还有剩余，那么下一个的共享节点就可以继续去获取许可来执行

当第二个线程被唤醒后，它又会去执行=setHeadAndPropagate()=提出当前节点，并尝试唤醒下一个节点，依次向后去唤醒

### 3.3 释放许可证
与=acquire()=方法对应，也有两个重载的=release()=方法，都会去调用=AQS=的=releaseShared()=方法

``` java
public void release() {
    sync.releaseShared(1);
}

public void release(int permits) {
    if (permits < 0) throw new IllegalArgumentException();
    sync.releaseShared(permits);
}
```

在=releaseShared()=方法中，同样也是先尝试去释放许可，只有释放成功了，才会去调用=doReleaseShared()=方法去唤醒线程

``` java
public final boolean releaseShared(int arg) {
    if (tryReleaseShared(arg)) {
        doReleaseShared();
        return true;
    }
    return false;
}
```

#### 3.3.1 尝试释放许可
AQS中没有具体实现=tryReleaseShared()=方法，在Semaphore的内部类=Sync=中，可以看到具体的实现

释放许可的逻辑也很简单，就是通过自旋+CAS来更新=state=属性值，直到更新成功才会退出

``` java
protected final boolean tryReleaseShared(int releases) {
    for (;;) {
        int current = getState();
        int next = current + releases;
        if (next < current) // overflow
            throw new Error("Maximum permit count exceeded");
        if (compareAndSetState(current, next))
            return true;
    }
}
```

#### 3.3.2 唤醒线程
唤醒线程的=doReleaseShared()=方法已经在AQS中实现，唤醒线程也是通过自旋+CAS来实现，首先获取同步队列的头节点，只有头节点的状态为=SIGNAL=才表示可以唤醒后续的节点，然后通过CAS将头节点的状态修改为0(初始状态)，修改失败就重试，修改成功就调用=unparkSuccessor()=方法去唤醒线程，该方法的源码在ReentrantLock的解锁逻辑中已经讲过，这里就不赘述了。

那么这个自旋什么时候结束呢？看后面的=h == head=，当调用完=unparkSucessor()=方法后，就会接续往下执行，然后就退出循环了。

``` java
private void doReleaseShared() {
    for (;;) {
        Node h = head;
        if (h != null && h != tail) {
            int ws = h.waitStatus;
            if (ws == Node.SIGNAL) {
                if (!compareAndSetWaitStatus(h, Node.SIGNAL, 0))
                    continue;            // loop to recheck cases
                unparkSuccessor(h);
            }
            else if (ws == 0 &&
                     !compareAndSetWaitStatus(h, 0, Node.PROPAGATE))
                continue;                // loop on failed CAS
        }
        if (h == head)                   // loop if head changed
            break;
    }
}
```
