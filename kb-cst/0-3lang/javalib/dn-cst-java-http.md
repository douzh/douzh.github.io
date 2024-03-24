# java http 

## HttpClient 

1.  connectionRequestTimout：指从连接池获取连接的timeout
2.  connetionTimeout：指客户端和服务器建立连接的timeout，就是http请求的三个阶段，一：建立连接；二：数据传送；三，断开连接。超时后会ConnectionTimeOutException
3.  socketTimeout：指客户端和服务器建立连接后，客户端从服务器读取数据的timeout，超出后会抛出SocketTimeOutException
4.  max-total：连接池里的最大连接数
5.  default-max-per-route：某一个/每服务每次能并行接收的请求数量，这里route指的是域名

### per-route

核心在AbstractConnPool

```java
public abstract class AbstractConnPool<T, C, E extends PoolEntry<T, C>>
                                               implements ConnPool<T, E>, ConnPoolControl<T> {

    private final Lock lock;
    private final Condition condition;
    private final ConnFactory<T, C> connFactory;
    private final Map<T, RouteSpecificPool<T, C, E>> routeToPool;
    private final Set<E> leased;
    private final LinkedList<E> available;
    private final LinkedList<Future<E>> pending;
    private final Map<T, Integer> maxPerRoute;

    private volatile boolean isShutDown;
    private volatile int defaultMaxPerRoute;
    private volatile int maxTotal;
    private volatile int validateAfterInactivity;
```

配制文件设置的defaultMaxPerRoute，最终每个route会保存在routeToPool，route以域名加端口标识。
