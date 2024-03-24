
# ribbon

主要功能是提供负载均衡算法和服务调用。

Ribbon就是在配置文件中根据你列出的Load Balancer 后面所有的机器基于某种规则，去连接这些机器。

说白了就是 负载均衡+RestTemplate调用

**getForObject方法和getForEntity方法**

推荐使用getForObject，能够获取json串。

getForEntity能获得更加详细的内容，比如状态码等。

IRule接口：根据特定算法从服务列表中选取一个要访问的服务。

 常用算法实现类如下 

| RoundRobinRule            | 轮询                                                         |
| ------------------------- | ------------------------------------------------------------ |
| RandomRule                | 随机                                                         |
| RetryRule                 | 先按照RoundRobinRule获取服务，如果获取失败就在指定实现内重试。 |
| WeightedResponseTimeRule  | 对RoundRobinRule的扩展，响应速度越快的实例选择权重越大。     |
| BestAvailableRule         | 会先过滤掉因为多次访问故障而处于断路器跳闸状态的服务，然后选择一个并发量最小的服务 |
| AvailabilityFilteringRule | 先过滤掉故障实例，再选择并发较小的实例                       |
| ZoneAvoidanceRule         | 默认规则，复合判断server所在区域的性能和server的可用性选择服务器。 |

负载均衡轮询算法原理

rest接口第几次请求数%服务器集群总数量=实际调用服务器位置下标，每次服务重启后rest接口计数从1开始。

如何替换？

注意配置细节：不能放在@ComponentScan所扫描的包及其子包下。

新建package，com.atguigu.myrule 因为配置要求，不能放在com.atguigu.springcloud下。

新建规则类

```java
package com.atguigu.myrule;

import com.netflix.loadbalancer.IRule;
import com.netflix.loadbalancer.RandomRule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @Author EiletXie
 * @Since 2020/3/11 10:01
 */
@Configuration
public class MySelfRule {

    @Bean
    public IRule myRule() {
        return new RandomRule(); // 定义为随机
    }

}
```

主启动类添加@RibbonClient

```java
@RibbonClient(name = "CLOUD-PAYMENT-SERVICE", configuration = MySelfRule.class)
```
