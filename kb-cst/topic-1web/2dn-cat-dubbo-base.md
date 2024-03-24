
# dubbo base 

<https://cn.dubbo.apache.org/zh-cn/>

## zookeeper
```
docker search zookeeper 
docker pull zookeeper
docker images
docker inspect zookeeper 

docker run -e TZ="Asia/Shanghai" -d -p 2181:2181 -v /Users/zihuidou/devtools/docker/zookeeper:/data --name zookeeper --restart always zookeeper
```
引入依赖

``` xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter</artifactId>
</dependency>

<dependency>
  <groupId>org.apache.dubbo</groupId>
  <artifactId>dubbo-spring-boot-starter</artifactId>
</dependency>
<dependency>
  <groupId>org.apache.dubbo</groupId>
  <artifactId>dubbo-dependencies-zookeeper-curator5</artifactId>
  <type>pom</type>
  <exclusions>
    <exclusion>
      <artifactId>slf4j-reload4j</artifactId>
      <groupId>org.slf4j</groupId>
    </exclusion>
  </exclusions>
</dependency>
```

## nacos

当Dubbo使用3.0.0及以上版本时，需要使用Nacos 2.0.0及以上版本。

引入依赖

``` xml
<dependency>
  <groupId>com.alibaba.nacos</groupId>
  <artifactId>nacos-client</artifactId>
  <version>2.1.0</version>
</dependency>
```
```
dubbo
  registry
    address: nacos://localhost:8848
```
## 示例

provider

``` java

public interface DemoService {

    String sayHello(String name);
}

@DubboService
public class DemoServiceImpl implements DemoService {

    @Override
    public String sayHello(String name) {
        return "Hello " + name;
    }
}

@SpringBootApplication
@EnableDubbo
public class ProviderApplication {
    public static void main(String[] args) {
        SpringApplication.run(ProviderApplication.class, args);
    }
}

```
```yaml
dubbo:
  application:
    name: dubbo-springboot-demo-provider
  protocol:
    name: dubbo
    port: -1
  registry:
    address: zookeeper://${zookeeper.address:127.0.0.1}:2181
```
consumer

``` java
@SpringBootApplication
@EnableDubbo
public class ConsumerApplication {

    public static void main(String[] args) {
        SpringApplication.run(ConsumerApplication.class, args);
    }
}

@Component
public class Task implements CommandLineRunner {
    @DubboReference
    private DemoService demoService;

    @Override
    public void run(String... args) throws Exception {
        String result = demoService.sayHello("world");
        System.out.println("Receive result ======> " + result);

        new Thread(()-> {
                while (true) {
                    try {
                        Thread.sleep(1000);
                        System.out.println(new Date() + " Receive result ======> " + demoService.sayHello("world"));
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        Thread.currentThread().interrupt();
                    }
                }
        }).start();
    }
}
```

```yaml
dubbo:
  application:
    name: dubbo-springboot-demo-consumer
  protocol:
    name: dubbo
    port: -1
  registry:
    address: zookeeper://${zookeeper.address:127.0.0.1}:2181
```
