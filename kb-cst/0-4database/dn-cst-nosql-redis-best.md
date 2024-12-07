# redis-best 

云数据库Redis拥有极强的性能，阿里云结合多年的运维经验，从业务部署、Key的设计、SDK、命令、运维管理等维度展示云数据库Redis开发运维规范，为您设计高效的业务系统提供参考，帮助您充分发挥Redis的能力。

## 了解Redis性能边界

![](assets/p277459.png)

计算资源: 使用通配符、Lua并发、1对多的PUBSUB、热点Key等会大量消耗计算资源，[集群架构](https://help.aliyun.com/document_detail/52228.htm#concept-tds-4mm-tdb)下还会导致访问倾斜，无法有效利用所有数据分片。
存储资源: Streaming慢消费、大Key等会占用大量存储资源，[集群架构](https://help.aliyun.com/document_detail/52228.htm#concept-tds-4mm-tdb)下还会导致数据倾斜，无法有效利用所有数据分片。
网络资源: 扫描全库（*KEYS*命令）、大Value、大Key的范围查询（如*HGETALL*命令）等会消耗大量的网络资源，且极易引发线程阻塞。

**注意**

Redis的高并发能力不等同于高吞吐能力，例如将大Value存在Redis里以期望提升访问性能，此类场景往往不会有特别大的收益，反而会影响Redis整体的服务能力。

[集群架构](https://help.aliyun.com/document_detail/52228.htm#concept-tds-4mm-tdb)下，热点Key、大Key或大Value等还会引发存储或访问倾斜 ，在生产环境中，您应当避免触达Redis的性能边界。

下文从业务部署、Key的设计、SDK、命令、运维管理等维度展示云数据库Redis开发运维规范，为您设计高效的业务系统提供参考，帮助您充分发挥Redis的能力。

## 业务部署规范

确定使用场景为 高速缓存 或 内存数据库 。

高速缓存：建议关闭[AOF](https://help.aliyun.com/document_detail/147408.htm#task-2368275)以降低开销，同时，由于数据可能会被淘汰，业务设计上避免强依赖缓存中的数据。例如Redis被写满后，会触发[数据淘汰策略](https://help.aliyun.com/document_detail/38679.htm)以挪移出空间给新的数据写入，根据业务的写入量会相应地导致延迟升高。

**注意**

如需使用[通过数据闪回按时间点恢复数据](https://help.aliyun.com/document_detail/148479.htm#task-2337807)功能，AOF功能需保持开启状态。内存数据库：应选购企业版（[持久内存型](https://help.aliyun.com/document_detail/183956.htm#concept-1952913)），支持命令级持久化，同时应通过监控报警关注内存使用率。具体操作，请参见[报警设置](https://help.aliyun.com/document_detail/43884.htm#concept-sj5-m2z-5db)。

就近部署业务，例如将业务部署在同一个专有网络VPC下的ECS实例中。

Redis具备极强的性能，如果部署位置过远（例如业务服务器与Redis实例通过公网连接），网络延迟将极大影响读写性能。

*说明*

针对多地部署应用的场景，您可以通过全球多活功能，借助其提供的跨域复制（Geo-replication）能力，快速实现数据异地灾备和多活，降低网络延迟和业务设计的复杂度。更多信息，请参见[Redis全球分布式缓存简介](https://help.aliyun.com/document_detail/71881.htm#concept-qf1-mdk-zdb)。

为每个业务提供单独的Redis实例。

避免业务混用，尤其需要避免将同一Redis实例同时用作高速缓存和内存数据库业务。带来的影响例如针对某个业务淘汰策略设置、产生的慢请求或执行\*FLUSHDB\*命令影响将扩散至其他业务。

设置合理的过期淘汰策略。

云数据库Redis默认的默认逐出策略为 volatile-lru ，关于各逐出策略的说明，请参见[参数支持](https://help.aliyun.com/document_detail/259681.htm#concept-2087327)。

合理控制压测的数据和压测时间。

云数据库Redis不会对您压测的数据执行自动删除操作，您需要自行控制压测数据的数据量和压测时间，避免对业务造成影响。

## Key设计规范

设计合理的Key中Value的大小，推荐小于10 KB。

过大的Value会引发数据倾斜、热点Key、实例流量或CPU性能被占满等问题，应从设计源头上避免此类问题带来的影响

设计合理的Key名称与长度。

Key名称：使用可读字符串作为Key名，如果使用Key名拼接库、表和字段名时，推荐使用英文冒号（:）分隔。例如=project:user:001=。在能完整描述业务的前提下，尽量简化Key名的长度，例如=username=可简化为=u=。由于大括号（{}）为Redis的hash tag语义，如果使用的是[集群架构](https://help.aliyun.com/document_detail/52228.htm#concept-tds-4mm-tdb)的实例，Key名称需要正确地使用大括号避免 引发数据倾斜 ，更多信息，请参见[keys-hash-tags](https://redis.io/topics/cluster-spec#keys-hash-tags)。

**说明** 

集群架构下执行同时操作多个Key的命令时（例如\*RENAME\*命令），如果被操作的Key未使用hash tag让其处于相同的数据分片，则命令无法正常执行。长度：推荐Key名的长度不超过128字节（越短越好）。

对于支持子Key的复杂数据结构，应避免一个Key中包含过多的子Key（推荐低于1,000）。

*说明*

常见的复杂数据结构例如Hash、Set、Zset、Geo、Stream及企业版（[性能增强型](https://help.aliyun.com/document_detail/126164.htm#concept-1254543)）特有的[TairHash](https://help.aliyun.com/document_detail/145970.htm#concept-2353551)、[TairBloom](https://help.aliyun.com/document_detail/145972.htm#concept-2353553)[TairGIS](https://help.aliyun.com/document_detail/145971.htm#concept-2353552)等。

由于某些命令（例如*HGETALL*）的时间复杂度直接与Key中的子Key数量相关。如果频繁执行时间复杂度为*O(N)*及以上的命令，且Key中的子Key数量过多容易引发慢请求、数据倾斜或热点Key问题。

推荐使用串行化方法将Value转变为可读的结构。

由于编程语言的字节码随着版本可能会变化，如果存储裸对象（例如Java Object、C#对象）会导致整个软件栈升级困难，推荐使用串行化方法将Value变成可读的结构。

## SDK使用规范

推荐使用JedisPool或者JedisCluster连接实例。

*说明*

企业版（[性能增强型](https://help.aliyun.com/document_detail/126164.htm#concept-1254543)）实例推荐使用TairJedis客户端，支持新数据结构的封装类。使用方法，请参见[TairJedis客户端](https://help.aliyun.com/document_detail/43848.htm#section-5sn-12z-kjz)。

如果使用单连接的方式，一旦遇到单次超时则无法自动恢复。关于JedisPool的连接方法，请参见[Jedis客户端](https://help.aliyun.com/document_detail/43848.htm#section-bqv-lkc-5db)、[JedisPool资源池优化](https://help.aliyun.com/document_detail/98726.htm#concept-kfn-zzw-yfb)和[JedisCluster](https://javadoc.io/doc/redis.clients/jedis/2.9.0/redis/clients/jedis/JedisCluster.html)。

避免使用Lettuce客户端。

由于Lettuce客户端在请求多次超时后，不再发起自动重连，当云数据库Redis触发高可用导致代理或者数据分片发生切换时，可能出现连接超时导致无法重连。为避免此类风险，推荐您使用[Jedis客户端](https://help.aliyun.com/document_detail/43848.htm#section-bqv-lkc-5db)

程序客户端需要对超时和慢请求做容错处理。

由于Redis服务可能因网络波动或资源占满引发超时或慢请求，您需要在程序客户端上设计合理的容错机制。

程序客户端应设置相对宽松的超时重试时间。

如果超时重试时间设置的非常短（例如200毫秒以下），可能引发重试风暴，极易引发业务层雪崩。更多信息，请参见[Redis客户端重试指南](https://help.aliyun.com/document_detail/303129.htm#concept-2107101)。

## 命令使用规范

避免执行范围查询（例如*KEYS*），使用多次单点查询或*SCAN*命令来获取延迟优势。

执行范围查询可能导致服务发生抖动、引发慢请求或产生阻塞。

推荐使用扩展数据结构（[数据结构模块集成](https://help.aliyun.com/document_detail/126164.htm#section-n2s-gty-48a)）实现复杂功能，避免使用Lua脚本。

Lua脚本会占用较多的计算和内存资源，且无法被多线程加速，过于复杂或不合理的Lua脚本可能导致资源被占满的情况。

合理使用管道（pipeline）降低链路的往返时延RTT（Round-trip time）。

如果有多个操作命令需要被迅速提交至服务器端，且客户端不依赖每个操作返回的结果，那么可以通过管道来作为优化性能的批处理工具，注意事项如下：管道执行期间客户端将独占与服务器端的连接，推荐为管道单独建立一个连接，将其与常规操作分离。每个管道应包含合理的命令数量（不超过100个）。

正确使用[Transaction命令族](https://help.aliyun.com/document_detail/185969.htm#section-ccb-jwx-hnb)。

使用事务（Transaction）时，需要注意其限制：事务本身没有回滚条件。对于[集群架构](https://help.aliyun.com/document_detail/52228.htm#concept-tds-4mm-tdb)的实例，需要使用hash
tag确保命令所要操作的Key都分布在1个Hash槽中，同时还需要避免hash tag带来的存储倾斜问题。避免在Lua脚本中封装事务命令，可能因编译加载消耗较多的计算资源。

避免使用[Pub和Sub命令族](https://help.aliyun.com/document_detail/185969.htm#section-nmv-n5x-hnb)执行大量的消息分发工作。

由于Pub和Sub不支持数据持久化，且不支持ACK应答机制无法实现数据可靠性，当执行大量消息分发工作时（例如订阅客户端数量超过100且Value超过1 KB），订阅客户端可能因服务端资源被占满而无法接收到数据。

*说明* 

为提升性能和均衡性，云数据库Redis对Pub和Sub类命令进行了优化，集群架构下，代理节点会根据channel name进行Hash计算，并分配至对应数据节点。

## 运维管理规范

1.  充分了解不同的实例管理操作带来的影响。

在对Redis实例执行变更配置、重启等操作时，实例的状态将发生变化并产生某些影响（例如产生秒级的连接闪断），在操作前您需要充分了解。更多信息，请参见[实例状态与影响](https://help.aliyun.com/document_detail/200740.htm#concept-2036360)。

2.  验证客户端程序的差错处理能力或容灾逻辑。

云数据库Redis支持节点健康状态监测，当监测到实例中的主节点不可用时，会自动触发主备切换，保障实例的高可用性。在客户端程序正式上线前，推荐手动触发主备切换，可帮助您验证客户端程序的差错处理能力或容灾逻辑。具体操作，请参见[手动执行主备切换](https://help.aliyun.com/document_detail/164222.htm#task-2488873)。

3.  禁用高耗时或高风险的命令。

生产环境中，无限制地使用命令可能带来诸多问题，例如执行\*FLUSHALL\*会直接清空全部数据；执行\*KEYS\*会阻塞Redis服务。为保障业务稳定、高效率地运行，您可以根据实际情况禁用特定的命令，具体操作，请参见[禁用高风险命令](https://help.aliyun.com/document_detail/107695.htm#task-uzq-tgk-5gb)。

4.  及时处理阿里云发起的计划内运维操作（即待处理事件）

为提供更优质的服务，持续提升产品性能和稳定性，阿里云会不定期地发起计划内运维操作（即待处理事件），对部分实例所属的机器执行软硬件或网络换代升级（例如数据库小版本升级）。当您收到来自阿里云的事件通知后，您可以查看本次事件的影响，根据业务需求评估是否需要调整执行时间。更多信息，请参见[查看并管理待处理事件](https://help.aliyun.com/document_detail/187022.htm#task-1963135)。

5.  为核心指标配置监控报警，帮助掌握实例运行状态。

为CPU使用率、内存使用率、带宽使用率等核心指标配置监控报警，实时掌握实例运行状态。具体操作，请参见[报警设置](https://help.aliyun.com/document_detail/43884.htm#concept-sj5-m2z-5db)。

6. 通过云数据库Redis提供的丰富的运维功能，定期检查实例状态或辅助排查资源消耗异常问题。

[分析慢日志](https://help.aliyun.com/document_detail/182843.htm#task-1948179)：帮助您快速找到慢请求问题发生的位置，定位发出请求的客户端IP，为彻底解决超时问题提供可靠的依据。[查看监控数据](https://help.aliyun.com/document_detail/122091.htm#task-645669)：云数据库Redis支持丰富的性能监控指标，帮助您掌握Redis服务的运行状况和进行问题溯源。[发起实例诊断](https://help.aliyun.com/document_detail/207410.htm#concept-2045851)：帮助您从性能水位、访问倾斜情况、慢日志等多方面评估实例的健康状况，快速定位实例的异常情况。[发起缓存分析](https://help.aliyun.com/document_detail/102093.htm#concept-ufz-byl-jgb)：帮助您快速发现实例中的大Key，帮助您掌握Key在内存中的占用和分布、Key过期时间等信息。[查询实时和历史热点Key](https://help.aliyun.com/document_detail/160585.htm#task-2460922)：帮助您快速发现实例中的热点Key，为进一步的优化提供数据支持。

7.  评估并开启审计日志功能。

开通审计日志功能后，可记录写操作的审计信息，为您提供日志的查询、在线分析、导出等功能，助您时刻掌握产品安全及性能情况。更多信息，请参见[开通新版审计日志](https://help.aliyun.com/document_detail/102015.htm#concept-ddc-ydr-3gb)。

**注意**

开通审计日志后，视写入量或审计量可能会对Redis实例造成5%～15%的性能损失。如果业务对Redis实例的写入量非常大，建议仅在运维需要（例如故障排查）期间开通审计功能，以免带来性能损失。
