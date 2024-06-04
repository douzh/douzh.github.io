# BlockingQueue


Java提供的线程安全的Queue可以分为阻塞队列和非阻塞队列，其中阻塞队列的典型例子是BlockingQueue，非阻塞队列的典型例子是ConcurrentLinkedQueue，在实际应用中要根据实际需要选用阻塞队列或者非阻塞队列。

BlockingQueue是个接口，顾名思义，"阻塞队列"：可以提供阻塞功能的队列。具体实现类有以下三个：

- ArrayBlockingQueue，其构造函数必须带一个int参数来指明其大小
- LinkedBlockingQueue，若其构造函数带一个规定大小的参数，生成的BlockingQueue有大小限制，若不带大小参数，所生成的BlockingQueue的大小由Integer.MAX_VALUE来决定
- PriorityBlockingQueue，其所含对象的排序不是FIFO,而是依据对象的自然排序顺序或者是构造函数的Comparator决定的顺序

| 功能  | 可能报异常 | 返回布尔值 | 可能阻塞 | 设定等待时间            |
|-------|------------|------------|----------|-------------------------|
| 入队  | add(e)     | offer(e)   | put(e)   | offer(e, timeout, unit) |
| 出队  | remove()   | poll()     | take()   | poll(timeout, unit)     |
| 查看  | element()  | peek()     | 无       | 无                      |

从上表可以很明显看出每个方法的作用，这个不用多说。我想说的是：

add(e) remove() element()方法不会阻塞线程。当不满足约束条件时，会抛出 IllegalStateException 异常。例如：当队列被元素填满后，再调用add(e)，则会抛出异常。

offer(e) poll() peek()方法即不会阻塞线程，也不会抛出异常。例如：当队列被元素填满后，再调用offer(e)，则不会插入元素，函数返回false。

要想要实现阻塞功能，需要调用 put(e) take()方法。当不满足约束条件时，会阻塞线程。


# ArrayBlockingQueue


ArrayBlockingQueue内容是一个数组容器，通过一个ReentrantLock控制数据的插入与删除。下面看一下源码，很简单。

java.util.concurrent.ArrayBlockingQueue

``` java
public class ArrayBlockingQueue< e> extends AbstractQueue< e> implements BlockingQueue< e>, java.io.Serializable {
    ...
    /** Main lock guarding all access */
    private final ReentrantLock lock;

    /** Condition for waiting takes */
    private final Condition notEmpty;

    /** Condition for waiting puts */
    private final Condition notFull;

    ...
    public ArrayBlockingQueue(int capacity, boolean fair) {
        if (capacity <= 0)
            throw new IllegalArgumentException();
        this.items = (E[]) new Object[capacity];
        lock = new ReentrantLock(fair);
        notEmpty = lock.newCondition();
        notFull =  lock.newCondition();
    }

    public void put(E e) throws InterruptedException {
        if (e == null) throw new NullPointerException();
        final E[] items = this.items;
        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();
        try {
            try {
                while (count == items.length)
                    notFull.await();
            } catch (InterruptedException ie) {
                notFull.signal(); // propagate to non-interrupted thread
                throw ie;
            }
            insert(e);
        } finally {
            lock.unlock();
        }
    }

    private void insert(E x) {
        items[putIndex] = x;
        putIndex = inc(putIndex);
        ++count;
        notEmpty.signal();
    }

    public E take() throws InterruptedException {
        final ReentrantLock lock = this.lock;
        lock.lockInterruptibly();
        try {
            try {
                while (count == 0)
                    notEmpty.await();
            } catch (InterruptedException ie) {
                notEmpty.signal(); // propagate to non-interrupted thread
                throw ie;
            }
            E x = extract();
            return x;
        } finally {
            lock.unlock();
        }
    }

    private E extract() {
        final E[] items = this.items;
        E x = items[takeIndex];
        items[takeIndex] = null;
        takeIndex = inc(takeIndex);
        --count;
        notFull.signal();
        return x;
    }
    ...
}
```

这里notEmpty和notFull作为lock的两个条件，分别负责管理想要加入元素的线程和想要取出元素的线程的wait和notify，通过await()、signal()、signalAll()方法，有效的分离了不同职责的线程。

例如put()方法在元素个数达到最大限制时会使用notFull条件把试图继续插入元素的线程都扔到等待集中，而执行了take()方法时如果顺利进入extract()则会空出空间，这时notFull负责随机的通知被其扔到等待集中的线程执行插入元素的操作。这样的设计使得线程按照功能行为职责管理成为了现实。


# LinkedBlockingQueue
队列这个数据结构已经很熟悉了，利用其先进先出的特性，多数生产消费模型的首选数据结构就是队列。对于有多个生产者和多个消费者线程的模型来说，最重要是他们共同访问的Queue是线程安全的。JDK中提供的线程安全的Queue的实现还是很丰富的：

ArrayBlockingQueue,LinkedBlockingQueue,PriorityBlockingQueue,DelayQueue,ConcurrentLinkedQueue等等，多数情况下使用这些数据结构编写并发程序足够了。

这里分析一下LinkedBlockingQueue的实现。

首先从简单的开始，先看看LinkedBlockingQueue线程安全的实现。之所以介绍它是因为其实现比较典型，对比ArrayBlokcingQueue使用一个ReentrantLock和两个Condition维护内部的数组来说，它使用了两个ReentrantLock，并且分别对应一个Condition来实现对内部数据结构Node型变量的维护。

``` java
public class LinkedBlockingQueue< e> extends AbstractQueue< e>
        implements BlockingQueue< e>, java.io.Serializable {
    private static final long serialVersionUID = -6903933977591709194L;

    /**
     * 节点数据结构
     */
    static class Node< e> {
        /** The item, volatile to ensure barrier separating write and read */
        volatile E item;
        Node< e> next;
        Node(E x) { item = x; }
    }

    /** 队列的容量 */
    private final int capacity;

    /** 持有节点计数器 */
    private final AtomicInteger count = new AtomicInteger(0);

    /** 头指针 */
    private transient Node< e> head;

    /** 尾指针 */
    private transient Node< e> last;

    /** 用于读取的独占锁*/
    private final ReentrantLock takeLock = new ReentrantLock();

    /** 队列是否为空的条件 */
    private final Condition notEmpty = takeLock.newCondition();

    /** 用于写入的独占锁 */
    private final ReentrantLock putLock = new ReentrantLock();

    /** 队列是否已满的条件 */
    private final Condition notFull = putLock.newCondition();

    private void signalNotEmpty() {
        final ReentrantLock takeLock = this.takeLock;
        takeLock.lock();
        try {
            notEmpty.signal();
        } finally {
            takeLock.unlock();
        }
    }

    private void signalNotFull() {
        final ReentrantLock putLock = this.putLock;
        putLock.lock();
        try {
            notFull.signal();
        } finally {
            putLock.unlock();
        }
    }

    private void insert(E x) {
        last = last.next = new Node< e>(x);
    }

    private E extract() {
        Node< e> first = head.next;
        head = first;
        E x = first.item;
        first.item = null;
        return x;
    }

    private void fullyLock() {
        putLock.lock();
        takeLock.lock();
    }

    private void fullyUnlock() {
        takeLock.unlock();
        putLock.unlock();
    }

    public LinkedBlockingQueue(int capacity) {
        if (capacity <= 0) throw new IllegalArgumentException();
        this.capacity = capacity;
        last = head = new Node< e>(null);
    }
   ...
}
```

这里仅仅展示部分源码，主要的方法在后面的分析中列出。分析之前明确一个最基本的概念。天天念叨着编写线程安全的类，什么是线程安全的类？那就是类内共享的全局变量的访问必须保证是不受多线程形式影响的。如果由于多线程的访问（改变，遍历，查看）而使这些变量结构被破坏或者针对这些变量操作的原子性被破坏，则这个类的编写不是线程安全的。

明确了这个基本的概念就可以很好的理解这个Queue的实现为什么是线程安全的了。在LinkedBlockingQueue的所有共享的全局变量中，final声明的capacity在构造器生成实例时就成了不变量了。而final声明的count由于是AtomicInteger类型的，所以能够保证其操作的原子性。剩下的final的变量都是初始化成了不变量，并且不包含可变属性，所以都是访问安全的。那么剩下的就是Node类型的head和last两个可变量。所以要保证LinkedBlockingQueue是线程安全的就是要保证对head和last的访问是线程安全的。

首先从上面的源码可以看到insert(E x),extract()是真正的操作head,last来入队和出对的方法，但是由于是私有的，所以不能被直接访问，不用担心线程的问题。实际入队的公开的方法是put(E
e)，offer(E e)和offer(E e, long timeout, TimeUnit unit)。put(...)方法与offer(...)都是把新元素加入到队尾，所不同的是如果不满足条件put会把当前执行的线程扔到等待集中等待被唤醒继续执行，而offer则是直接退出，所以如果是需要使用它的阻塞特性的话，不能直接使用poll(...)。

put(...)方法中加入元素的操作使用this.putLock来限制多线程的访问，并且使用了可中断的方式：

``` java
public void put(E e) throws InterruptedException {
        if (e == null) throw new NullPointerException();
        int c = -1;
        final ReentrantLock putLock = this.putLock;
        final AtomicInteger count = this.count; //----------------a
        putLock.lockInterruptibly();//随时保证响应中断 //--------b
        try {
            //*****************************(1)*********************************
            try {
                while (count.get() == capacity)
                    notFull.await();
            } catch (InterruptedException ie) {
                notFull.signal(); // propagate to a non-interrupted thread
                throw ie;
            }
           //*****************************end*********************************
            insert(e);//真正的入队操作
           //********************(2)**********************
            c = count.getAndIncrement();
            if (c + 1 < capacity)
                notFull.signal();
            //******************end**********************
        } finally {
            putLock.unlock();
        } //-------------------------c
        if (c == 0) //---------------d
            signalNotEmpty();
}
```

代码段(1)是阻塞操作，代码段(2)是count递增和唤醒等待的操作。两者之间的insert(e)才是入队操作，其实际是操作的队尾引用last，并且没有牵涉到head。所以设计两个锁的原因就在这里！因为出队操作take()，poll()实际是执行extract()仅仅操作队首引用head。增加了this.takeLock这个锁，就实现了多个不同任务的线程入队的同时可以进行出对的操作，并且由于两个操作所共同使用的count是AtomicInteger类型的，所以完全不用考虑计数器递增递减的问题。假设count换成int，则相应的putLock内的count++和takeLock内的count--有可能相互覆盖，最终造成count的值被腐蚀，故这种设计必须使用原子操作类。

保证类的线程安全只要保证head和last的操作的线程安全，也就是保证insert(E
x)和extract()线程安全即可。那么上面的put方法中的代码段(1)放在a,b之间，代码段(2)放在c,d之间不是更好？毕竟锁的粒度越小越好。单纯的考虑count的话这样的改变是正确的，但是await()和singal()这两个方法执行时都会检查当前线程是否是独占锁的那个线程，如果不是则抛出java.lang.IllegalMonitorStateException异常。而这两段代码中包含notFull.await()和notFull.signal()这两句使得(1),(2)必须放在lock保护块内。这里说明主要是count本身并不需要putLock或者takeLock的保护，从

```
public int size() {
        return count.get();
}
```

可以看出count的访问是不需要任何锁的。而在put等方法中，其与锁机制的混用很容易造成迷惑。最后put中的代码d的作用主要是一个低位及时通知的作用，也就是队列刚有值试图获得takeLock去通知等待集中的出队线程。因为c==0意味着count.getAndIncrement()原子递增成功，所以count
> 0成立。类似作用的代码：

```
if (c == capacity)
       signalNotFull();
```

在take和poll中也有出现，实现了高位及时通知。

分析完了put，对应的offer，take，poll方法都是类似的实现。下面看看遍历队列的操作：

```
public Object[] toArray() {
        fullyLock();
        try {
            int size = count.get();
            Object[] a = new Object[size];
            int k = 0;
            for (Node p = head.next; p != null; p = p.next)
                a[k++] = p.item;
            return a;
        } finally {
            fullyUnlock();
        }
}
```

这个方法很简单主要是要清楚一点：这个操作执行时不允许其他线程再修改队首和队尾，所以使用了fullyLock去获取putLock和takeLock，只要成功则可以保证不会再有修改队列的操作。然后就是安心的遍历到最后一个元素为止了。

另外在offer(E e, long timeout, TimeUnit
unit)这个方法中提供了带有超时的入队操作，如果一直不成功的话，它会尝试在timeout的时间内入队：

```
for (;;) {
     ...//入队操作
     if (nanos <= 0)
         return false;
     try {
          nanos = notFull.awaitNanos(nanos);
     } catch (InterruptedException ie) {
           notFull.signal(); // propagate to a non-interrupted thread
           throw ie;
     }
}
```

其内部循环使用notFull.awaitNanos(nanos)方法反复的计算剩余时间的大概值用于实现延时功能。nanos<=0则放弃尝试，直接退出。

整体而言，LinkedBlockingQueue的实现还是很清晰的。相对于后面要介绍的ConcurrentLinkedQueue来说，它属于简单的实现。这些看似复杂的数据结构的实现实质都是多线程的基础的综合应用。就好像数学中千变万化的难题其实都是基础公式的组合一样，如果有清晰的基础认知，还是能找到自己分析的思路的。本来是想从mina中找找类似的实现，不过很遗憾的是它好像仅仅实现了一个非线程安全的循环队列，然后在其基础上使用synchronized进行封装成线程安全的Queue。
