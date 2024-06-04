# ConcurrentLinkedQueue

ConcurrentLinkedQueue充分使用了atomic包的实现打造了一个无锁得并发线程安全的队列。对比锁机制的实现，个人认为使用无锁机制的难点在于要充分考虑线程间的协调。简单的说就是多个线程对内部数据结构进行访问时，如果其中一个线程执行的中途因为一些原因出现故障，其他的线程能够检测并帮助完成剩下的操作。这就需要把对数据结构的操作过程精细的划分成多个状态或阶段，考虑每个阶段或状态多线程访问会出现的情况。上述的难点在此次分析的并发Queue的实现中有很好的说明。首先看看其部分源码：

``` java
public class ConcurrentLinkedQueue< E> extends AbstractQueue< E>
        implements Queue< E>, java.io.Serializable {
    private static final long serialVersionUID = 196745693267521676L;

    private static class Node< E> {
        private volatile E item;
        private volatile Node< E> next;
        private static final
            AtomicReferenceFieldUpdater< node node="">
            nextUpdater =
            AtomicReferenceFieldUpdater.newUpdater
            (Node.class, Node.class, "next");
        private static final
            AtomicReferenceFieldUpdater< node object="">
            itemUpdater =
            AtomicReferenceFieldUpdater.newUpdater
            (Node.class, Object.class, "item");

        Node(E x) { item = x; }

        Node(E x, Node< E> n) { item = x; next = n; }

        E getItem() {
            return item;
        }

        boolean casItem(E cmp, E val) {
            return itemUpdater.compareAndSet(this, cmp, val);
        }

        void setItem(E val) {
            itemUpdater.set(this, val);
        }

        Node< e> getNext() {
            return next;
        }

        boolean casNext(Node< E> cmp, Node< E> val) {
            return nextUpdater.compareAndSet(this, cmp, val);
        }

        void setNext(Node< E> val) {
            nextUpdater.set(this, val);
        }

    }

    private static final
        AtomicReferenceFieldUpdater< concurrentlinkedqueue node="">
        tailUpdater =
        AtomicReferenceFieldUpdater.newUpdater
        (ConcurrentLinkedQueue.class, Node.class, "tail");
    private static final
        AtomicReferenceFieldUpdater< concurrentlinkedqueue node="">
        headUpdater =
        AtomicReferenceFieldUpdater.newUpdater
        (ConcurrentLinkedQueue.class,  Node.class, "head");

    private boolean casTail(Node< E> cmp, Node< E> val) {
        return tailUpdater.compareAndSet(this, cmp, val);
    }

    private boolean casHead(Node< E> cmp, Node < e> val) {
        return headUpdater.compareAndSet(this, cmp, val);
    }

    private transient volatile Node < e> head = new Node< E>(null, null);

    private transient volatile Node< E> tail = head;
    ...
}
```

先看看其内部数据结构Node的实现。由于使用了原子字段更新器AtomicReferenceFieldUpdater<t
v>（其中T表示持有字段的类的类型，V表示字段的类型），所以其对应的需要更新的字段要使用volatile进行声明。其newUpdater(Class<u>
tclass, Class< w> vclass, String
fieldName)方法实例化一个指定字段的更新器，参数分别表示：持有需要更新字段的类，字段的类，要更新的字段的名称。Node的内部变量item，next分别有对应自己的字段更新器，并且包含了对其原子性操作的方法compareAndSet(T
obj, V expect, V
update)，其中T是持有被设置字段的对象，后两者分别是期望值和新值。

对于ConcurrentLinkedQueue自身也有两个volatile的线程共享变量：head，tail分别对应队列的头指针和尾指针。要保证这个队列的线程安全就是保证对这两个Node的引用的访问（更新，查看）的原子性和可见性，由于volatile本身能够保证可见性，所以就是对其修改的原子性要被保证。下面看看其对应的方法是如何完成的。

``` java
    public boolean offer(E e) {
        if (e == null) throw new NullPointerException();
        Node< e> n = new Node< e>(e, null);
        for (;;) {
            Node< e> t = tail;
            Node< e> s = t.getNext();
            if (t == tail) { //------------------------------a
                if (s == null) { //---------------------------b
                    if (t.casNext(s, n)) { //-------------------c
                        casTail(t, n); //------------------------d
                        return true;
                    }
                } else {
                    casTail(t, s); //----------------------------e
                }
            }
        }
    }
```

offer()方法都很熟悉了，就是入队的操作。涉及到改变尾指针的操作，所以要看这个方法实现是否保证了原子性。CAS操作配合循环是原子性操作的保证，这里也不例外。此方法的循环内首先获得尾指针和其next指向的对象，由于tail和Node的next均是volatile的，所以保证了获得的分别都是最新的值。

代码a：t==tail是最上层的协调，如果其他线程改变了tail的引用，则说明现在获得不是最新的尾指针需要重新循环获得最新的值。

代码b：s==null的判断。静止状态下tail的next一定是指向null的，但是多线程下的另一个状态就是中间态：tail的指向没有改变，但是其next已经指向新的结点，即完成tail引用改变前的状态，这时候s!=null。这里就是协调的典型应用，直接进入代码e去协调参与中间态的线程去完成最后的更新，然后重新循环获得新的tail开始自己的新一次的入队尝试。另外值得注意的是a,b之间，其他的线程可能会改变tail的指向，使得协调的操作失败。从这个步骤可以看到无锁实现的复杂性。

代码c：t.casNext(s,
n)是入队的第一步，因为入队需要两步：更新Node的next，改变tail的指向。代码c之前可能发生tail引用指向的改变或者进入更新的中间态，这两种情况均会使得t指向的元素的next属性被原子的改变，不再指向null。这时代码c操作失败，重新进入循环。

代码d：这是完成更新的最后一步了，就是更新tail的指向，最有意思的协调在这儿又有了体现。从代码看casTail(t,
n)不管是否成功都会接着返回true标志着更新的成功。首先如果成功则表明本线程完成了两步的更新，返回true是理所当然的；如果casTail(t,n)不成功呢？要清楚的是完成代码c则代表着更新进入了中间态，代码d不成功则是tail的指向被其他线程改变。意味着对于其他的线程而言：它们得到的是中间态的更新，s!=null，进入代码e帮助本线程执行最后一步并且先于本线程成功。这样本线程虽然代码d失败了，但是是由于别的线程的协助先完成了，所以返回true也就理所当然了。

通过分析这个入队的操作，可以清晰的看到无锁实现的每个步骤和状态下多线程之间的协调和工作。理解了入队的整个过程，出队的操作poll()的实现也就变得简单了。基本上是大同小异的，无非就是同时牵涉到了head和tail的状态，在改变head的同时照顾到tail的协调，在此不多赘述。下面介绍一下其无锁下的查看访问，其内部不单单是查看更包含了线程间的协调，这是无锁实现的一个特点。不管是contains()，size()还是isEmpty()，只要获得了head后面第一个最新的Node就可以很轻松的实现，毕竟Node的getNext()和getItem()返回的都是对应的最新值。所以先看看这些方法内部的first()如何获得最新的第一个Node：

``` java
    Node< e> first() {
        for (;;) {
            Node< e> h = head;
            Node< e> t = tail;
            Node< e> first = h.getNext();
            if (h == head) { //---------------------------------------a
                if (h == t) { //-----------------------------------------b
                    if (first == null) //----------------------------------c
                        return null;
                    else
                        casTail(t, first); //--------------------------------d
                } else {
                    if (first.getItem() != null) //------------------------e
                        return first;
                    else // remove deleted node and continue
                        casHead(h, first); //------------------------------f
                }
            }
        }
    }
```

此方法在尝试获得最新的第一个非head结点的时候，在不同的阶段同样在协调着head和tail的更新任务，让人感觉无锁的世界没有纯粹的工作，呵呵。

代码a：还是最上层的协调，head指向没改变的情况下才继续下面的操作。这时侯head只可能是静止的，因为poll()出队操作的步骤是反着的：首先更新head的指向进入中间态，然后更新原head的next的item为null。

代码b：之所以h==t的情况独立于其他的情况(在出队poll()方法中同样)，主要是因为first!=null时可能对应着某一个更新的中间态，而产生中间态的的必要条件就是代码b成立。如果h==t则表示当前线程获得的首尾指针指向同一个结点，当然代码b执行之后可能其他线程会进行head或者tail的更新。

代码c：first==null表明tail并没有进入更新的中间态而是处于静止状态，并且由于tail指向的是head的指向，所以返回null是唯一的选择。但是这美好的一切都是建立在代码b和代码c之间没有其他的线程更新tail。一旦有其他的线程执行了入队的操作并至少进入中间态的话，h==t和first==null都遗憾的成立，这就造成了取得幻象值，而实际上h.getNext()已经不再为null。个人认为代码c改成if((first
= h.getNext()) == null)更能提高命中率。

代码d：只要first!=null(不管是本人修改的代码还是源码)本线程则去尝试协调其他的线程先完成tail的更新，等待循环再次获取最新的head和tail。

代码e：此处first一定不为null，tail更新与否不影响first的item的获取，但是head的更新会有影响。如果head正在被另一个线程更新并进入中间态，既是poll()内的else
if (casHead(h, first))
成功，但是并没有执行first.setItem(null)之前。此时代码e是满足的，返回的也是当前的first的，但是随后head全部更新成功则first的item为null。所以此处返回的first的item并不一定是item!=null的结点，在使用此方法获得的结点的item时一定要再次的进行判断，这点在contains(...)等方法内都有体现。

代码f：如果first的item==null，则更新head的指向。直观上看似乎多余，因为出队的操作是先更新head的指向再更新item为null的。但是另一个方法remove(...)则仅仅更新item的值而不改变head的指向，所以针对这样的多线程调用，代码f变得非常的必需了。

这样通过这两个方法的分析可以推及对ConcurrentLinkedQueue共享变量的其他操作的实现，这样的无锁的实现印象最深的就是要考虑线程间的协调。不像锁机制的实现虽然牺牲了一定的性能，但是至少操作这些非线程安全的共享变量时不用过多的考虑其他线程的操作。至此才算体会到无锁实现的复杂性，这或许就是有得必有失吧，呵呵。
