# 经典生产事故


## 阿里云DTS问题

阿里DTS是常用数据同步工具，主要有库到库同步，库到库迁移，数据订阅消费。DTS是基于BLOG做的实现，在使用时有很多注意事项，否则容易导致生产事故。

为了保障源到目标的数据一致性，猜测DTS内部数据是串行同步的，这导致DTS有下面几个不好的特性：
1. 如果一个DTS实例配置多张表，一张表修改大量数据会阻断其他表的数据同步
2. 消费端出现异常DTS会中断，需要人工重启消费服务（文档未明确说明，但有这现象）
3. DTS订阅只能使用阿里SDK的client消费，且多个消费服务只有一个有效，其他为备节点
4. 不支持表修改，需要目标端同步修改
5. 并发性的提高需要业务自己处理，比如发MQ再消费，但会引入其他问题

## 问题：DTS订阅ABA问题

问题原因：快速修改一条数据的一个值，DTS订阅到MQ没有顺序了，先修改成最新值再修改成旧值

解决方案：
1. 使用顺序消息，但有性能问题
2. 更新数据添加版本号，旧版本不能更新新版本数据，

## 问题：DTS堵塞

问题原因：大批量修改一张表数据，导致其他表同步堵塞

解决方案：
1. 不同业务，不同数据量的dts同步创新不同dts链路

注：DTS要保障修改的顺序，所以中间MQ没有并发，使用前要考虑好DTS和性能和规格

## 问题：DTS客户端配制问题

问题原因：DTS因为要保障同步的顺序性，消费端只有一个有效，服务集群部署最终也只有一个有效

解决方案：合理拆分消息服务，部署两个节点，一主一备即可

### 问题：DTS不支持表修改

问题原因：DTS不支持DDL同步，需要同步修改目标端表

解决方案：做好项目管理，做好预警

### 问题：DTS订阅中断

问题原因：DTS订阅消费端不能出现异常，有未捕获异常就就中断，必须捕获所有异常。

解决方法：
1. 捕获所有异常，但可能丢数据
2. 消费端直接转MQ，再消费MQ，MQ有重试机制

## 阿里Rocketmq问题

关注GROUP TOPIC TAG的订阅一致问题：
1. 生产者不需要group
2. 消费者用group划分，同一group的订阅信息要一致，包括TAG
3. 广播组消费节点不在线，在上线后不会收到历史广播消息，如果需要消息重试不要用广播模式
4. 配置好重试次数、重试时间、消费线程数，处理好消费端异常，需要重试的要抛出

### 问题：阿里云消息重复发送

问题原因：阿里云rocketmq消息重复发送，原因是升级

解决方案：rocketmq升级在阿里云最佳实践中要求消费端做幂等
1. 根据msgid做幂等
2. 根据业务ID做幂等
3. 数据库导致做数据幂等限制

### 问题：阿里云MQ网络不通

问题原因：阿里云rocketmq 4.0不支持内网，公网会有长时间连接不上的问题

解决方案：使用阿里云rocketmq 5.0，支持内网

### 问题：阿里云rocketmq5.0限流

问题原因：4.0不做限流，5.0做了限流，导致消费延时

解决方案：如果时间和资源上可以，中间件大版本升级要做好调研、灰度、试点，分批上线

### 问题：阿里云rocketmq流量大问题

问题原因：多个业务使用同一个Topic，1000多家门店使用一个广播组订阅任务，导致所有消息都会广播一下，产生大量广播消息

用一个TOPIC给1000家门店下发任务，如果用同一个group就不能不同门店订阅不同的TAG，因为订阅不一致，只能用一个GROUP广播门店再通过TAG过滤。

MQ5.0可以程序自建GROUP，这样门店服务自建自己的GROUP就可以订阅自己的TAG消息了。

解决方案：
1. 将不同业务拆分不同TOPIC，按最佳实践配制
2. rocketmq5.0支持自动建组，可以将广播改为独立组加TAG的单点订阅

## redis问题

### 问题：redis集群单节点内存不足

问题原因：因为使用redis集群，业务使用了hash结构，一个key存储了海量数据

解决方案：拆分hash结构为单个的key，分散到各节点

### 问题：redis堵塞

问题原因：因为redis配制连接池太小，在高并发秒杀情况下连接数不足导致程序功能堵塞

解决方案：一般业务不会考虑redis连接池问题，在高并发或秒杀情况要考虑redis配制

### 问题：redis导致程序卡顿

问题原因：系统一直卡顿导致千分之几的失败订单，经排查是系统使用LTS做调度器使用redis做选主功能，这个调度器使用keys命令，导致redis堵塞

解决方案：给LTS换一个redis独立使用

注：redis的keys会堵塞，可以使用scan命令，但数据结果可能会有不准的问题

### 问题：redis存储内容多个转译字符

问题原因：spring项目使用redis，有些功能存储数据多了转译字符，经排查是RedisTemplate使用问题

解决方案：使用StringRedisTemplate

### 问题：redis命令导致的redis查询超时

问题原因：开发人员代码中使用了keys命令，导致慢请求，并且是并发产生，导致redis偶现查询超时现象

解决方案：
1. 阿里云可以配置禁止redis的命令，对于高风险命令keys、flushdb、flushall、config
2. 开发人员查问题可以使用scan代替keys，虽然scan有时不准，但排查问题足够了

## 路由配制问题

### 请求头token丢失问题

问题：APP的h5界面经常有登录失效的情况

原因：因为nginx默认请求头的key不支持下划线，登录的信息cookie里有，当cookie失效时请求头里有token也无效

解决方案：配置nginx，添加对下划线的支持。但建议新请求头不要用下划线。

### gateway限流失败

问题：在gateway的route上配制限流，没有限制住

原因：gateway自来的限流filter限流参数不是针对route，而是请求path的，所以参数是限制每一个请求的QPS

解决方案：将需要限制接口地址单独配置route。

### 统一域名添加gateway网关的Cors问题

添加网关，添加登录验证，代理多个后端服务

问题一：上线时发现生产有cors问题
原因：测试环境nginx更换为了apisix，测试环境无问题，生产不兼容
解决方案：nginx添加cors配置

问题二：nginx添加cors配制后还是报错，提示Access-Control-Allow-Origin有多个
原因：经排查后发现服务端有cors配制，如果nginx开启会多次添加respond 响应头 Access-Control-Allow-Origin
解决方案：因为ngnix不配制cors请求会被限制，配置会有多个响应头问题，临时映射到同一域名解决

问题三：前端访问地址为域名加端口，路由在slb配制，无法映射同一域名
原因：历史配制不合理，前端调用一个域名不同端口的接口，无法映射同一域名
解决方案：因为nginx和后端微服务中有个gateway，通过删除请求头origin和响应头Access-Control-Allow-Origin，屏蔽了服务器上的cors配置

### 移动端免登录部分用户登录失败

流程：移动端免登录流程是调用协同办公平台通过飞书认证，之后跳转链接上带上加密的用户编号，业务系统解密做登录认证

问题原因：因为添加统一鉴权，路由添加了nginx和gateway，而加密用户信息编码有各种特殊字符，且是Get请求，中间被转码，导致各种解码失败

解决方案：对加密用户信息做hex十六进制编码解码，不在GET请求参数中传特殊字符

## 阿里云DRDS问题

会员DRDS数据库事故

连接数打满


## 数据库问题

### 问题修改数据库表字段导致系统功能崩溃

问题原因: 修改一张表字段类型为字符型，表数据量有12亿，数据变更了1.5小时，期间表不能做插入操作，导致相关业务整体崩溃

因为生产变更在很少人用的时间段操作，影响不大

解决方法：
1. 此类变更需发停服公告，变算无业务时间窗口也一样
2. 使用oceanbase数据库无此问题，mysql系列此问题没有好的解决办法
3. 使用更高版本mysql，支持无锁变更


## 环境问题

### InetAddress.getLocalHost线程卡死

现象: 因为门店服务要上报心跳，获取门店IP时总有门店的心跳线程被卡死，显示服务假死

原因: 用jstack pid 导出线程信息，发现是卡在了InetAddress.getLocalHost，获取服务器信息需要正确的hosts和dns配制，否则会有取不到或慢的现象

解决方法: 因为门店环境复杂，统一更新比较困难，所以程序不再获取服务器信息

```
Thread 266449: (state = BLOCKED)
 - java.net.InetAddress.getLocalHost() @bci=49, line=1465 (Interpreted frame)
 - com.epudge.store.service.impl.SysParamServiceImpl.sendServiceHeart() @bci=22, line=236 (Interpreted frame)
 - com.epudge.store.service.impl.SysParamServiceImpl$$FastClassBySpringCGLIB$$44023e56.invoke(int, java.lang.Object, java.lang.Object[]) @bci=109 (Compiled frame)
 - org.springframework.cglib.proxy.MethodProxy.invoke(java.lang.Object, java.lang.Object[]) @bci=19, line=204 (Compiled frame)
 - org.springframework.aop.framework.CglibAopProxy$DynamicAdvisedInterceptor.intercept(java.lang.Object, java.lang.reflect.Method, java.lang.Object[], org.springframework.cglib.proxy.MethodProxy) @bci=94, line=669 (Compiled frame)
 - com.epudge.store.service.impl.SysParamServiceImpl$$EnhancerBySpringCGLIB$$73988a3c.sendServiceHeart() @bci=31 (Interpreted frame)
 - com.epudge.store.mvc.controller.HomeController.sendInfo() @bci=33, line=414 (Interpreted frame)
 - com.epudge.store.mvc.controller.HomeController$$FastClassBySpringCGLIB$$de50fa3.invoke(int, java.lang.Object, java.lang.Object[]) @bci=280 (Compiled frame)
 - org.springframework.cglib.proxy.MethodProxy.invoke(java.lang.Object, java.lang.Object[]) @bci=19, line=204 (Compiled frame)
 - org.springframework.aop.framework.CglibAopProxy$CglibMethodInvocation.invokeJoinpoint() @bci=19, line=738 (Compiled frame)
 - org.springframework.aop.framework.ReflectiveMethodInvocation.proceed() @bci=19, line=157 (Compiled frame)
 - org.springframework.aop.interceptor.AsyncExecutionInterceptor$1.call() @bci=4, line=115 (Interpreted frame)
 - java.util.concurrent.FutureTask.run() @bci=42, line=266 (Compiled frame)
 - java.util.concurrent.ThreadPoolExecutor.runWorker(java.util.concurrent.ThreadPoolExecutor$Worker) @bci=95, line=1142 (Interpreted frame)
 - java.util.concurrent.ThreadPoolExecutor$Worker.run() @bci=5, line=617 (Interpreted frame)
 - java.lang.Thread.run() @bci=11, line=745 (Interpreted frame)
```

在系统hosts文件中增加本地ip地址与主机名的对应项

例如说你的ip是 10.1.4.166 主机名是centos1

那么配置： 10.1.4.166 centos1


## 安全问题

### SpringBoot Actuator未授权访问漏洞

springboot项目未关闭actuator，外网服务会泄露生产配置信息。

### nacos 暴露公网

nacos暴露公网，默认密码未修改或有弱密码，导致配置泄露。

Nacos权限绕过漏洞，nacos低版本可以通过设置请求头绕过权限访问。

最好的解决方案就是nacos不要放公网，如果放公网需要添加白名单限制访问。

