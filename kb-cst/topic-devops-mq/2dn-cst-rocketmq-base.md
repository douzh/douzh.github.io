# Rocketmq介绍

## 介绍

开源官网：http://rocketmq.apache.org/

中文文档：https://github.com/apache/rocketmq/tree/master/docs/cn

可以直接下载程序包，也可以自己编译。

## 使用逻辑

消息流转：生产者-\>Topic-\>消费者

生产者：

1.  分组：生产组
2.  目标：Topic:Tag

Topic:

1.  所属：集群、broker
2.  队列：每个broker上的读写队列
3.  订阅组：绑定的消费组

消费者：

1.  消费组：实现同一组不重复消费
2.  Topic：订阅Topic
3.  Selector: 过滤同一Topic的消息

**一个消息只能输出到一个Topic，可以被多个消费组订阅**

一个Topic在多个broker上分布有多个queue，消费者订阅最终订阅到queue上。

特殊场景：

用同一个topic向1000个店分发任务，不同的店订阅自己的任务。

方式一：使用广播方式，每个店会收到所有任务，通过自己的ID进行过滤
方式二：使用普通消息，每个店创建自己的group，订阅自己的tag

## rocketmq best

## install

broker三种模式：

-   单组节点单副：开发测试，不高可用
-   多组节点（集群）单副本：无slave，配置简单，raid保障消息不丢，异步刷盘丢失少量消息，同步刷盘一条不丢；宕机期间，这台机器上未被消费的消息在机器恢复之前不可订阅
-   多节点（集群）多副本模式-异步复制：磁盘损坏，消息丢失的非常少Master宕机后，消费者仍然可以从Slave消费；Master宕机，磁盘损坏情况下会丢失少量消息。
-   多节点（集群）多副本模式-同步双写：数据与服务都无单点故障，Master宕机情况下，消息无延迟；性能比异步复制模式略低（大约低10%左右）

### 启动

启动Name Server

``` bash
> nohup sh bin/mqnamesrv &
> tail -f ~/logs/rocketmqlogs/namesrv.log
The Name Server boot success...
```

启动Broker

    > nohup sh bin/mqbroker -n localhost:9876 &
    > tail -f ~/logs/rocketmqlogs/broker.log
    The broker[%s, 172.30.30.233:10911] boot success...

### 关闭

    > sh bin/mqshutdown broker
    The mqbroker(36695) is running...
    Send shutdown request to mqbroker(36695) OK

    > sh bin/mqshutdown namesrv
    The mqnamesrv(36664) is running...
    Send shutdown request to mqnamesrv(36664) OK

### 测试发送和接收消息

在发送或接收消息之前，我们需要通知客户端name servers的位置。RocketMQ提供多种实现方式。为简单起见，我们现在展示环境变量NAMESRV\\~ADDR的用法~

    > export NAMESRV_ADDR=localhost:9876
    > sh bin/tools.sh org.apache.rocketmq.example.quickstart.Producer
    SendResult [sendStatus=SEND_OK, msgId= ...

    > sh bin/tools.sh org.apache.rocketmq.example.quickstart.Consumer
    ConsumeMessageThread_%d Receive New Messages: [MessageExt...

    /mqadmin updateTopic -n localhost:9876  -b localhost:10911  -t tx-mq-TOPIC

## docker安装方式

namesvr和broker用同一镜像，只是启动命令不同

docker pull rocketmqinc/rocketmq:4.3.2

控制台

docker pull styletang/rocketmq-console-ng

### 启动

启动namesvr

    docker run -d -p 9876:9876 \
    --name rmqnamesrv \
    rocketmqinc/rocketmq:4.3.2 sh mqnamesrv

配制broker.conf

broker-a.conf

    brokerClusterName = DefaultCluster
    brokerName = broker-a
    brokerId = 0
    deleteWhen = 04
    fileReservedTime = 48
    brokerRole = ASYNC_MASTER
    flushDiskType = ASYNC_FLUSH
    # 个人配制
    # namesrvAddr=192.168.31.200:9876
    brokerIP1=192.168.31.200
    # 监听端口，默认是10911
    listenPort=10911
    # 是否允许 Broker 自动创建Topic，建议线下开启，线上关闭
    autoCreateTopicEnable=false
    # 是否允许 Broker 自动创建订阅组，建议线下开启，线上关闭
    autoCreateSubscriptionGroup=false

broker-a.conf个性brokerName和listenPort两个配制，其它一样就可以。

namesrvAddr如果有docker的link设置了就用不设置了，没有link需要设置一下。

brokerIP1是broker对外提供的ip地址，生产者和消费者从namesvr获取broker的地址就是这一个。

brokerIP1必须要设置，因为docker默认提供的是\`172.17.x.x\`的地址，在非docker容器环境不能调用。

启动broker

两个broker区别在于映射配制文件、端口和容器名称。

    docker run -d -p 10911:10911 \
    -v /Users/zihuidou/devtools/docker/rocketmq/conf/broker-a.conf:/opt/rocketmq-4.3.2/conf/broker.conf \
    --name rmqbroker-a \
    --link rmqnamesrv:namesrv \
    -e "NAMESRV_ADDR=namesrv:9876" \
    rocketmqinc/rocketmq:4.3.2 sh mqbroker  -c /opt/rocketmq-4.3.2/conf/broker.conf

    docker run -d -p 10912:10912 \
    -v /Users/zihuidou/devtools/docker/rocketmq/conf/broker-b.conf:/opt/rocketmq-4.3.2/conf/broker.conf \
    --name rmqbroker-b \
    --link rmqnamesrv:namesrv \
    -e "NAMESRV_ADDR=namesrv:9876" \
    rocketmqinc/rocketmq:4.3.2 sh mqbroker  -c /opt/rocketmq-4.3.2/conf/broker.conf

link是用于broker确定namesvr用的，不果不设置，需要在broker.conf里设置namesrvAddr配制。

**必须指定broker.conf，不指定配制不会生效。**

启动控制台

    docker run --name rmqadmin -p 8080:8080 \
    --link rmqnamesrv:namesrv \
    -e "JAVA_OPTS=-Drocketmq.namesrv.addr=namesrv:9876 \
    -Dcom.rocketmq.sendMessageWithVIPChannel=false" \
    -t styletang/rocketmq-console-ng

访问地址：http://localhost:8080/#/

## springboot

新建两个springboot项目，添加web依赖，一个做producer一个做consumer。

添加依赖

``` example
<dependency>
    <groupId>org.apache.rocketmq</groupId>
    <artifactId>rocketmq-spring-boot-starter</artifactId>
    <version>2.0.4</version>
</dependency>
```

### 生产者

添加配制

``` example
server:
  port: 18083

rocketmq:
  name-server: localhost:9876
  producer:
    group: boot_producer_group
    sendMessageTimeout: 10000
```

添加生产者调用示例

``` java
@RestController
public class ProducerController {

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @RequestMapping("/hellomq")
    public String hellomq()  {
        // 如下两种方式等价
        // rocketMQTemplate.send("test-topic-1", MessageBuilder.withPayload("Hello, World!").build());
        rocketMQTemplate.convertAndSend("test-topic-1", "Hello, World!");
        System.out.println("hellomq send msg");
        return "hellomq";
    }

    @RequestMapping("/hellotag")
    public String hellotag()  {
        // topic:tag
        rocketMQTemplate.convertAndSend("test-topic-2:tag1", "hello topic 2 tag 1!");
        rocketMQTemplate.convertAndSend("test-topic-2:tag2", "hello topic 2 tag 2!");
        System.out.println("hellotag send msg");
        return "hellotag";
    }

    @RequestMapping("/hellodelay")
    public String hellodelay()  {
        // 延迟级别（18个等级）: 1到18分别对应1s 5s 10s 30s 1m 2m 3m 4m 5m 6m 7m 8m 9m 10m 20m 30m 1h 2h
        rocketMQTemplate.syncSend("test-topic-3:delaytag",MessageBuilder.withPayload("I'm delayed message").build(),5000, 2);
        System.out.println("hellodelay send msg");
        return "hellodelay";
    }

    @RequestMapping("/helloobject")
    public String helloobject()  {
        rocketMQTemplate.convertAndSend("test-topic-4",new User("测试",22));
        System.out.println("helloobject send msg");
        return "helloobject";
    }
}
```

### 消费者

每个消费者consumerGroup不能相同。

``` java
@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-1-string", topic = "test-topic-1")
public class ConsumerString implements RocketMQListener<String> {

    @Override
    public void onMessage(String message) {

        System.out.println("group-test-topic-1-string: "+message);
    }
}

@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-2-tag1", topic = "test-topic-2",selectorExpression = "tag1")
public class ConsumerTag1 implements RocketMQListener<String> {
    @Override
    public void onMessage(String message) {
        System.out.println("group-test-topic-2-tag1 select tag1: "+message);
    }
}

@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-2-tag2", topic = "test-topic-2",selectorExpression = "tag2")
public class ConsumerTag2 implements RocketMQListener<String> {
    @Override
    public void onMessage(String message) {
        System.out.println("group-test-topic-2-tag2 select tag2: "+message);
    }
}

@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-3-delay", topic = "test-topic-3")
public class ConsumerDelay implements RocketMQListener<String> {
    @Override
    public void onMessage(String message) {
        System.out.println("group-test-topic-3-delay: "+message);
    }
}

@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-1-messgext", topic = "test-topic-1")
public class ConsumerMessageExt implements RocketMQListener<MessageExt> {
    @Override
    public void onMessage(MessageExt message) {
        System.out.println("group-test-topic-1-messgext: "+message);
        try {
            System.out.println("group-test-topic-1-messgext body: "+ new String(message.getBody(),"utf-8"));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
    }
}

// User必须有一个空构造器
@Service
@RocketMQMessageListener(consumerGroup = "group-test-topic-4-object", topic = "test-topic-4")
public class ConsumerObject implements RocketMQListener<User> {
    @Override
    public void onMessage(User message) {
        System.out.println("group-test-topic-1-messgext: "+message);
        System.out.println("group-test-topic-1-messgext body: "+ message.getName()+" "+message.getAge());
    }
}

public class User {

    private String name;
    private Integer age;

    public User() {
    }

    public User(String name, Integer age) {
        this.name = name;
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getAge() {
        return age;
    }

    public void setAge(Integer age) {
        this.age = age;
    }
}
```
