在Spring Boot启动时，会在`refreshContext(context);`阶段完成配置类的解析、各种BeanFactoryPostProcessor和BeanPostProcessor的注册、国际化配置的初始化、web内置容器的构造等等，这时会读取pom中引入jar的配置文件`/META-INF/spring.factories`，所以这里`EnableAutoConfiguration`下的所有类都会被实例化并注入Spring容器。

## 最简demo

```xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter</artifactId>
  <version>2.3.3.RELEASE</version>
</dependency>
```

```java
package com.iteedu.springboot.d0;

import com.iteedu.springboot.d0.service.HelloService;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class SpringBootD0App {
    public static void main(String[] args) {

        ConfigurableApplicationContext cxt=SpringApplication.run(SpringBootD0App.class,args);
        HelloService s=cxt.getBean("helloService", HelloService.class);
        System.out.println(s.hello());
    }
}

package com.iteedu.springboot.d0.service;

import org.springframework.stereotype.Service;

@Service
public class HelloService {

    public String hello(){
        return "hello world";
    }
}

```

