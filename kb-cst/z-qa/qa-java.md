## Java基础

1、什么是面向对象？

2、JDK JRE JVM

3、==和equals比较

4、hashCode与equals

5、final

6、String、StringBuffer、StringBuilder

7、重载和重写的区别

8、接口和抽象类的区别

9、List和Set的区别

10、ArrayList和LinkedList区别

11、HashMap和HashTable有什么区别？其底层实现是什么？

12、ConcurrentHashMap原理，jdk7和jdk8版本的区别？

13、什么是字节码？采用字节码的好处是什么？

14、Java中的异常体系

15、Java类加载器

16、双亲委托模型

17、GC如何判断对象可以被回收

## 线程、并发相关

1、线程的生命周期？线程有几种状态

2、sleep()、wait()、join()、yield()的区别

3、对线程安全的理解

4、Thread、Runable的区别

5、对守护线程的理解

6、ThreadLocal的原理和使用场景

7、ThreadLocal内存泄露原因，如何避免

8、并发、并行、串行的区别

9、并发的三大特性

10、volatile

11、为什么用线程池？解释下线程池参数？

12、简述线程池处理流程

13、线程池中阻塞队列的作用？为什么是先添加列队而不是先创建最大线程？

14、线程池中线程复用原理

## JVM

jvm 的主要组成部分？及其作用？
什么是双亲委派机制
jvm 运行时数据区？
类加载的执行过程？
怎么判断对象是否可以被回收？
jvm 有哪些垃圾回收算法？
jvm 有哪些垃圾回收器？

- Serial：最早的单线程串行垃圾回收器。
- Serial Old：Serial 垃圾回收器的老年版本，同样也是单线程的，可以作为 CMS 垃圾回收器的备选预案。
- ParNew：是 Serial 的多线程版本。
- Parallel 和 ParNew 收集器类似是多线程的，但 Parallel 是吞吐量优先的收集器，可以牺牲等待时间换取系统的吞吐量。
- Parallel Old 是 Parallel 老生代版本，Parallel 使用的是复制的内存回收算法，Parallel Old 使用的是标记-整理的内存回收算法。
- CMS：一种以获得最短停顿时间为目标的收集器，非常适用 B/S 系统。
- G1：一种兼顾吞吐量和停顿时间的 GC 实现，是 JDK 9 以后的默认 GC 选项。

详细介绍一下 CMS 垃圾回收器？
新生代垃圾回收器和老生代垃圾回收器都有哪些？有什么区别？

- 新生代回收器：Serial、ParNew、Parallel Scavenge
- 老年代回收器：Serial Old、Parallel Old、CMS
- 整堆回收器：G1

简述分代垃圾回收器是怎么工作的？
说一下 jvm 调优的工具？
常用的 jvm 调优的参数都有哪些？

