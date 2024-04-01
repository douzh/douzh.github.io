
# kafka 

<https://kafka.apache.org/>

-   没有消息过滤的语义，可以采取多个Topic的方式达到过滤的目的。
-   没有消息广播的语义，可以通过创建不同的Group来模拟实现。
-   没有列信队列，可以将失败消息转到一个TOPIC模拟实现。
-   可以接入logstash 和 filebeat

## 开始

从官网下载

启动zk

    # Start the ZooKeeper service
    $ bin/zookeeper-server-start.sh config/zookeeper.properties

另一窗口启动服务

    # Start the Kafka broker service
    $ bin/kafka-server-start.sh config/server.properties

创建topic

    $ bin/kafka-topics.sh --create --topic quickstart-events --bootstrap-server localhost:9092

查询topic

    $ bin/kafka-topics.sh --describe --topic quickstart-events --bootstrap-server localhost:9092
    Topic: quickstart-events        TopicId: NPmZHyhbR9y00wMglMH2sg PartitionCount: 1       ReplicationFactor: 1    Configs:
        Topic: quickstart-events Partition: 0    Leader: 0   Replicas: 0 Isr: 0

发消息

    $ bin/kafka-console-producer.sh --topic quickstart-events --bootstrap-server localhost:9092
    This is my first event
    This is my second event

收消息

    $ bin/kafka-console-consumer.sh --topic quickstart-events --from-beginning --bootstrap-server localhost:9092
    This is my first event
    This is my second event

## kafka best 

### 发布者

发送消息的示例代码如下：

``` java
Future<RecordMetadata> metadataFuture = producer.send(new ProducerRecord<String, String>(
        topic,   //消息主题。
        null,   //分区编号。建议为null，由Producer分配。
        System.currentTimeMillis(),   //时间戳。
        String.valueOf(value.hashCode()),   //消息键。
        value   //消息值。
));
```

为了便于追踪，请为消息设置一个唯一的Key。您可以通过Key追踪某消息，打印发送日志和消费日志，了解该消息的发送和消费情况。

如果消息发送量较大，建议不要设置Key，并使用黏性分区策略。

1.  失败重试

    分布式环境下，由于网络等原因偶尔发送失败是常见的。导致这种失败的原因可能是消息已经发送成功，但是ACK失败，也有可能是确实没发送成功。

    云消息队列 Kafka
    版是VIP网络架构，长时间不进行通信连接会被主动断开，因此，不是一直活跃的客户端会经常收到connection
    reset by peer错误，建议重试消息发送。

    您可以根据业务需求，设置以下重试参数：

    -   retries：消息发送失败时的重试次数。
    -   retry.backoff.ms，消息发送失败的重试间隔，建议设置为1000，单位：毫秒。

2.  异步发送

    发送接口是异步的，如果您想接收发送的结果，可以调用metadataFuture.get(timeout,
    TimeUnit.MILLISECONDS)。

3.  线程安全

    Producer是线程安全的，且可以往任何Topic发送消息。通常情况下，一个应用对应一个Producer。

4.  Acks

    Acks的说明如下：

    acks=0：无需服务端的Response、性能较高、丢数据风险较大。

    acks=1：服务端主节点写成功即返回Response、性能中等、丢数据风险中等、主节点宕机可能导致数据丢失。

    acks=all：服务端主节点写成功且备节点同步成功才返回Response、性能较差、数据较为安全、主节点和备节点都宕机才会导致数据丢失。

    为了提升发送性能， 建议设置为acks=1。

5.  提升发送性能

    Batch机制，Kafka 版Producer端主要通过两个参数进行控制：

    -   batch.size :
        发往每个分区（Partition）的消息缓存量（消息内容的字节数之和，不是条数）。达到设置的数值时，就会触发一次网络请求，然后Producer客户端把消息批量发往服务器。如果batch.size设置过小，有可能影响发送性能和稳定性。建议保持默认值16384。单位：字节。
    -   linger.ms :
        每条消息在缓存中的最长时间。若超过这个时间，Producer客户端就会忽略batch.size的限制，立即把消息发往服务器。建议根据业务场景，
        设置linger.ms在100\~1000之间。单位：毫秒。

    因此，Kafka 版Producer客户端什么时候把消息批量发送至服务器是由batch.size和linger.ms共同决定的。您可以根据具体业务需求进行调整。为了提升发送的性能，保障服务的稳定性， 建议您设置batch.size=16384和linger.ms=1000。

6.  黏性分区策略

    只有发送到相同分区的消息，才会被放到同一个Batch中，因此决定一个Batch如何形成的一个因素是云消息队列 Kafka 版Producer端设置的分区策略。云消息队列 Kafka 版Producer允许通过设置Partitioner的实现类来选择适合自己业务的分区。在消息指定Key的情况下，云消息队列 Kafka 版Producer的默认策略是对消息的Key进行哈希，然后根据哈希结果选择分区，保证相同Key的消息会发送到同一个分区。

    黏性分区策略主要解决无Key消息分散到不同分区，造成小Batch问题。其主要策略是如果一个分区的Batch完成后，就随机选择另一个分区，然后后续的消息尽可能地使用该分区。这种策略在短时间内看，会将消息发送到同一个分区，如果拉长整个运行时间，消息还是可以均匀地发布到各个分区上的。这样可以避免消息出现分区倾斜，同时还可以降低延迟，提升服务整体性能。

    如果您使用的云消息队列 Kafka 版Producer客户端是2.4及以上版本，默认的分区策略就采用黏性分区策略。如果您使用的Producer客户端版本小于2.4，可以根据黏性分区策略原理，自行实现分区策略，然后通过参数partitioner.class设置指定的分区策略。

7.  OOM

    结合云消息队列 Kafka 版的Batch设计思路，云消息队列 Kafka 版会缓存消息并打包发送，如果缓存太多，则有可能造成OOM（Out of Memory）。

    -   buffer.memory : 发送的内存池大小。如果内存池设置过小，则有可能导致申请内存耗时过长，从而影响发送性能，甚至导致发送超时。建议buffer.memory ≧ batch.size \* 分区数 \* 2。单位：字节。
    -   buffer.memory的默认数值是32 MB，对于单个Producer而言，可以保证足够的性能。

    如果您在同一个JVM中启动多个Producer，那么每个Producer都有可能占用32 MB缓存空间，此时便有可能触发OOM。

8.  分区顺序

    单个分区（Partition）内，消息是按照发送顺序储存的，是基本有序的。

    默认情况下，云消息队列 Kafka 版为了提升可用性，并不保证单个分区内绝对有序，在升级或者宕机时，会发生少量消息乱序（某个分区挂掉后把消息Failover到其它分区）。

    如果业务要求分区保证严格有序，请在创建Topic时选择使用Local存储。

### 订阅者

1.  负载均衡

    每个Group可以包含多个消费实例，即可以启动多个云消息队列 Kafka 版Consumer，并把参数group.id设置成相同的值。属于同一个Group的消费实例会负载消费订阅的Topic。

    云消息队列 Kafka 版负载均衡消费的内部原理是，把订阅的Topic的分区，平均分配给各个消费实例。因此，消费实例的个数不要大于分区的数量，否则会有消费实例分配不到任何分区而处于空跑状态。这个负载均衡发生的时间，除了第一次启动上线之外，后续消费实例发生重启、增加、减少等变更时，都会触发一次负载均衡。

2.  分区个数

    分区个数主要影响的是消费者的并发数量。

    控制台的默认分区个数是12，可以满足绝大部分场景的需求。您可以根据业务使用量进行增加。不建议分区数小于12，否则可能影响消费发送性能；也不建议超过100个，否则易引发消费端Rebalance。

    分区增加后，将不能减少，请小幅度调整。

3.  多个订阅

    一个Group可以订阅多个Topic，多个Topic的消息被Group中的Consumer均匀消费。例如Group A订阅了Topic A、Topic B、Topic C，则这三个Topic中的消息，被Group中的Consumer均匀消费。

    一个Topic可以被多个Group订阅，且各个Group独立消费Topic下的所有消息。例如Group A订阅了Topic A，Group B也订阅了Topic A，则发送到Topic A的每条消息，不仅会传一份给Group A的消费实例，也会传一份给Group B的消费实例，且这两个过程相互独立，相互没有任何影响。

4.  一个Group对应一个应用

    建议一个Group对应一个应用，即不同的应用对应不同的代码。如果您需要将不同的代码写在同一个应用中，请准备多份不同的kafka.properties。例如kafka1.properties、kafka2.properties。

5.  消费位点提交

    每个Topic会有多个分区，每个分区会统计当前消息的总条数，这个称为最大位点MaxOffset。

    云消息队列 Kafka 版Consumer会按顺序依次消费分区内的每条消息，记录已经消费了的消息条数，称为消费位点ConsumerOffset。

    剩余的未消费的条数（也称为消息堆积量）=MaxOffset-ConsumerOffset。

    云消息队列 Kafka 版消费者有两个相关参数：

    -   enable.auto.commit：是否采用自动提交位点机制。默认值为true，表示默认采用自动提交机制。
    -   auto.commit.interval.ms： 自动提交位点时间间隔。默认值为1000，即1s。

    这两个参数组合的结果就是，每次poll数据前会先检查上次提交位点的时间，如果距离当前时间已经超过参数auto.commit.interval.ms规定的时长，则客户端会启动位点提交动作。

    因此，如果将enable.auto.commit设置为true，则需要在每次poll数据时，确保前一次poll出来的数据已经消费完毕，否则可能导致位点跳跃。

    如果想自己控制位点提交，请把enable.auto.commit设为false，并调用commit(offsets)函数自行控制位点提交。

6.  消费位点重置

    以下两种情况，会发生消费位点重置：

    -   当服务端不存在曾经提交过的位点时（例如客户端第一次上线）。
    -   当从非法位点拉取消息时（例如某个分区最大位点是10，但客户端却从11开始拉取消息）。

    Java客户端可以通过auto.offset.reset来配置重置策略，主要有三种策略：

    -   latest：从最大位点开始消费。
    -   earliest：从最小位点开始消费。
    -   none：不做任何操作，即不重置。

7.  拉取大消息

    消费过程是由客户端主动去服务端拉取消息的，在拉取大消息时，需要注意控制拉取速度，注意修改配置：

    -   max.poll.records：每次Poll获取的最大消息数量。如果单条消息超过1
        MB，建议设置为1。
    -   fetch.max.bytes：设置比单条消息的大小略大一点。
    -   max.partition.fetch.bytes：设置比单条消息的大小略大一点。

    拉取大消息的核心是逐条拉取的。

    通过公网消费消息时，通常会因为公网带宽的限制导致连接被断开，此时需要注意控制拉取速度，修改配置：

    -   fetch.max.bytes：建议设置成公网带宽的一半（注意该参数的单位是bytes，公网带宽的单位是bits）
    -   max.partition.fetch.bytes：建议设置成fetch.max.bytes的三分之一或者四分之一。

8.  消息重复和消费幂等

    云消息队列 Kafka 版消费的语义是at least once， 也就是至少投递一次，保证消息不丢失，但是无法保证消息不重复。在出现网络问题、客户端重启时均有可能造成少量重复消息，此时应用消费端如果对消息重复比较敏感（例如订单交易类），则应该做消息幂等。

    以数据库类应用为例，常用做法是：

    -   发送消息时，传入key作为唯一流水号ID。
    -   消费消息时，判断key是否已经消费过，如果已经被消费，则忽略，如果没消费过，则消费一次。

    如果应用本身对少量消息重复不敏感，则不需要做此类幂等检查。

9.  消费失败

    云消息队列 Kafka 版是按分区逐条消息顺序向前推进消费的，如果消费端拿到某条消息后执行消费逻辑失败，例如应用服务器出现了脏数据，导致某条消息处理失败，等待人工干预，那么有以下两种处理方式：

    -   失败后一直尝试再次执行消费逻辑。这种方式有可能造成消费线程阻塞在当前消息，无法向前推进，造成消息堆积。
    -   云消息队列 Kafka 版没有处理失败消息的设计，实践中通常会打印失败的消息或者存储到某个服务（例如创建一个Topic专门用来放失败的消息），然后定时检查失败消息的情况，分析失败原因，根据情况处理。

10. 提高消费速度

    提高消费速度有以下两个办法：

    -   增加Consumer实例个数。

    可以在进程内直接增加（需要保证每个实例对应一个线程，否则没有太大意义），也可以部署多个消费实例进程；需要注意的是，实例个数超过分区数量后就不再能提高速度，将会有消费实例不工作。

    -   增加消费线程。 增加Consumer实例本质上也是增加线程的方式来提升速度，因此更加重要的性能提升方式是增加消费线程，最基本的步骤如下：
        -   定义一个线程池。
        -   Poll数据。
        -   把数据提交到线程池进行并发处理。
        -   等并发结果返回成功后，再次poll数据执行。
