# threadlocal

当多个线程可以访问一个对象的实例变量时，这个实例变量是线程不安全的。如果我们想保证变量的原子性可以用Synchonized来解决。如果我们想让每个线程都有自己的实例变量怎么办？

比如一个Servlet中众多方法要使用数据库的查询内容，但每个请求的查询内容是不同的，这就要求方法在获取数据库查询内容时是指定线程查出来的。这时就要用到ThreadLocal。

ThreadLocal的实现原理很简单，每个Thread都有一个MAP，叫做threadLocals。

``` java
public
class Thread implements Runnable {
    ...
    ThreadLocal.ThreadLocalMap threadLocals = null;
    ...
}
```

ThreadLocal主要用两个方法：

void set(Object obj) 设置当前线程的变量的副本的值。

Object get() 返回当前线程的变量副本

看一下具体实现：

``` java 
    public T get() {
        Thread t = Thread.currentThread();
        ThreadLocalMap map = getMap(t);
        if (map != null) {
            ThreadLocalMap.Entry e = map.getEntry(this);
            if (e != null)
                return (T)e.value;
        }
        return setInitialValue();
    }
    public void set(T value) {
        Thread t = Thread.currentThread();
        ThreadLocalMap map = getMap(t);
        if (map != null)
            map.set(this, value);
        else
            createMap(t, value);
    }
```

它设置的是当前线程中的threadLocals这个MAP，Map的key是ThreadLocal对象本身的实例。

来一个例子：

``` java 
    public class ThreadLocalDemo implements Runnable {
       private final static  ThreadLocal studentLocal = new ThreadLocal();  //ThreadLocal对象在这

       public static void main(String[] agrs) {
           TreadLocalDemo td = new TreadLocalDemo();
             Thread t1 = new Thread(td,"a");
             Thread t2 = new Thread(td,"b");
            t1.start();
            t2.start();
          }

        public void run() {
             accessStudent();
        }

        public  void  accessStudent() {
            String currentThreadName = Thread.currentThread().getName();
            System.out.println(currentThreadName+" is running!");
            Random random = new Random();
            int age = random.nextInt(100);
            System.out.println("thread "+currentThreadName +" set age to:"+age);
            Student student = getStudent();  //每个线程都独立维护一个Student变量
            student.setAge(age);
            System.out.println("thread "+currentThreadName+" first  read age is:"+student.getAge());
            try {
            Thread.sleep(5000);
            }
            catch(InterruptedException ex) {
                ex.printStackTrace();
            }
            System.out.println("thread "+currentThreadName +" second read age is:"+student.getAge());

        }

        protected Student getStudent() {
            Student student = (Student)studentLocal.get();  //从ThreadLocal对象中取
            if(student == null) {
                student = new Student();
                studentLocal.set(student);  //如果没有就创建一个
            }
            return student;
        }

        protected void setStudent(Student student) {
            studentLocal.set(student);  //放入ThreadLocal对象中
        }
    }
```

1. ThreadLocal只是对需要存储的对象的管理，而存储实际是由当前Thread负责。个人理解为ThreadLocal是一个操作Thread.threadLocals 的工具。
2. 使用ThreadLocal可以使对象达到线程隔离的目的。同一个ThreadLocal操作不同的Thread，实质是各个Thread对自己的变量操作。
3. 为什么要使用ThreadLocal，个人感觉有两个原因，1是与其它线程的隔离，2是可以在一个线程的生命周期中使用同一个对象，达到对象传递的作用。这样的好处是可以减少dal访问或者ws调用。

这里列出一个用到ThreadLocal的例子，主要的作用是使用ThreadLocal记录用户信息以及记录用户的执行时间。这在实际应用中，可以映射为全局记录用户的权限，以及使用Threadlocal对系统的性能做一些分析等。。

首先有两个对象，一个是用户对象

``` java
// 简单记录用户是否可以访问，可以用于全局权限控制等
class User {
    private String name;
    private boolean isAllow;
    public User(String name, boolean isAllow) {
        this.name = name;
        this.isAllow = isAllow;
    }
    public String getName() {
        return name;
    }
    public boolean isAllow() {
        return isAllow;
    }
    @Override
    public String toString() {
        return "用户名：" + name + "\t 是否允许访问：" + isAllow;
    }
}

// 用于记录每一步骤耗时…,可以用于每一步的性能分析
class TimeConsumer {
    // 名称
    private String name;
    // 耗时数据列表
    private List steps;
    public TimeConsumer(String name, long start) {
        this.name = name;
        steps = new ArrayList();
        steps.add(start);
    }
    public void andStep(long step) {
        steps.add(step);
    }
    @Override
    public String toString() {
        StringBuffer br = new StringBuffer("操作[" + name + "]共有"
                + (steps.size() - 1) + "步\n");
        for (int i = 1; i < steps.size(); i++) {
            br.append("\t|--耗时[" + (steps.get(i) - steps.get(0))
                    + "ms]\n");
        }
        br.append("\n");
        return br.toString();
    }
}

// threadlocal 管理类
class MyThreadLocal {
    // 用于全局记录user访问权限
    private ThreadLocal userLocal;
    // 用于全局记录用户每一步的耗时
    private ThreadLocal timeLocal;
    private static MyThreadLocal local = new MyThreadLocal();
    private MyThreadLocal() {
        userLocal = new ThreadLocal();
        timeLocal = new ThreadLocal();
    }
    public static MyThreadLocal getInstanse() {
        return local;
    }
    public void addUser(User user) {
        userLocal.set(user);
    }
    public User getUser() {
        return userLocal.get();
    }
    public void addTime(TimeConsumer timeConsumer) {
        timeLocal.set(timeConsumer);
    }
    public void addTime(long l) {
        TimeConsumer time = timeLocal.get();
        timeLocal.remove();
        time.andStep(l);
        timeLocal.set(time);
    }
    public TimeConsumer getTime() {
        return timeLocal.get();
    }
}

public class CoreThreadLocal {
    public static void main(String[] args) {
        new Thread(new TestRunnable("name1", 1000L, true)).start();
        new Thread(new TestRunnable("name2", 700L, true)).start();
        new Thread(new TestRunnable("name3", 888, false)).start();
    }
}

// 用于测试，多线程实现
class TestRunnable implements Runnable {
    String name;
    long l;
    boolean isAllow;
  TestRunnable(String name, long l, boolean isAllow) {
        this.name = name;
        this.l = l;
        this.isAllow = isAllow;
    }
    public void run() {
        MyThreadLocal local = MyThreadLocal.getInstanse();
        local.addUser(new User(name, isAllow));
        local.addTime(new TimeConsumer(name, System.currentTimeMillis()));
        // 做某个业务，并记录时间
        doThings(l);
        local.addTime(System.currentTimeMillis());
        // 做某个业务，并记录时间
        doThings(l);
        local.addTime(System.currentTimeMillis());
        // 业务做完，打印日志
        System.out.println(local.getUser());
        System.out.println(local.getTime());
    }
    // 模拟具体业务的处理步骤
    private void doThings(long l) {
        try {
            Thread.sleep(l);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
```

运行上面的程序得到结果如下：

```
用户名：name2   是否允许访问：true
操作[name2]共有2步
    |--耗时[703ms]
    |--耗时[1406ms]


用户名：name3    是否允许访问：false
操作[name3]共有2步
    |--耗时[891ms]
    |--耗时[1781ms]


用户名：name1    是否允许访问：true
操作[name1]共有2步
    |--耗时[1000ms]
    |--耗时[2000ms]
```
