## springboo-hello

注意：APP类必须在要注册类的父包中，因为spring扫描bean默认只扫Application的子包

```java
@SpringBootApplication
public class SpringBootD0App {
    public static void main(String[] args) {

        ConfigurableApplicationContext cxt=SpringApplication.run(SpringBootD0App.class,args);
        HelloService s=cxt.getBean("helloService", HelloService.class);
        System.out.println(s.hello());
    }
}

@Service
public class HelloService {

    public String hello(){
        return "hello world";
    }
}
```

```xml
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter</artifactId>
            <version>2.3.3.RELEASE</version>
        </dependency>
```

Springboot-log

```java
@SpringBootApplication
public class SpringbootD0LogbackApplication {

	public static void main(String[] args) {

	    SpringApplication.run(SpringbootD0LogbackApplication.class, args);
	    System.out.println("start log");
        TestService.log("bbb");
        UserActLog.logact("useridaaa","a","b","c");
        System.out.println("end log");
	}

}

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
public class TestService {
    static Logger log = LoggerFactory.getLogger(TestService.class);

    public static void log(String userId){
        log.info("log service {} args:{}",userId);
    }
}

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
public class UserActLog {
    static Logger log = LoggerFactory.getLogger(UserActLog.class);

    public static void logact(String userId,String... args){
        log.info("log act {} args:{}",userId, args);
    }
}
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- scan:当此属性设置为true时，配置文件如果发生改变，将会被重新加载，默认值为true -->
<!-- debug:当此属性设置为true时，将打印出logback内部日志信息，实时查看logback运行状态。默认值为false。 -->
<configuration  scan="true" scanPeriod="10 seconds" debug="false">
    <contextName>logback</contextName>
    <property name="LOG_HOME" value="${LOG_PATH:-.}" />
    <property name="LEVEL" value="INFO" />
    <property name="log.pattern" value=" %d{yyyy-MM-dd HH:mm:ss.SSS} [%thread] %-5level %logger{50} - [%method,%line] - %msg%n"/>

    <!--输出到控制台-->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <!--此日志appender是为开发使用，只配置最底级别，控制台输出的日志级别是大于或等于此级别的日志信息-->
        <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
            <level>info</level>
        </filter>
        <encoder>
            <Pattern>${log.pattern}</Pattern>
            <charset>UTF-8</charset>
        </encoder>
    </appender>

    <appender name="FILE_TEST" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>${LOG_HOME}/test.log</file>
        <encoder class="ch.qos.logback.classic.encoder.PatternLayoutEncoder">
            <Pattern>${log.pattern}</Pattern>
        </encoder>
        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>${logging.path}/%d{yyyy-MM-dd}-test.log.zip</fileNamePattern>
            <maxHistory>15</maxHistory>
        </rollingPolicy>
    </appender>

    <appender name="USERACT" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>${LOG_HOME}/user-act.log</file>
        <encoder class="ch.qos.logback.classic.encoder.PatternLayoutEncoder">
            <Pattern>%msg%n</Pattern>
        </encoder>
        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>${logging.path}/%d{yyyy-MM-dd}-user-act.log.zip</fileNamePattern>
            <maxHistory>15</maxHistory>
        </rollingPolicy>
    </appender>


    <logger name="com.iteedu.demo" level="${LEVEL}" additivity="true">
        <appender-ref ref="FILE_TEST"/>
    </logger>
    <logger name="com.iteedu.demo.utils.UserActLog" level="${LEVEL}" additivity="false">
        <appender-ref ref="USERACT"/>
    </logger>

    <root level="${LEVEL}">
        <appender-ref ref="CONSOLE"/>
        <appender-ref ref="FILE_TEST"/>
    </root>
</configuration>

```
application.properties
```properties
spring.application.name= logbacktest
logging.config=classpath:logback.xml
logging.file.path= /Users/zihuidou/logs/logback/

```

## springboot-SpringContextUtils

```java
@Component
public class SpringContextUtils implements ApplicationContextAware {
	public static ApplicationContext applicationContext;

	public void setApplicationContext(ApplicationContext applicationContext)
			throws BeansException {
		SpringContextUtils.applicationContext = applicationContext;
	}

	public static Object getBean(String name) {
		return applicationContext.getBean(name);
	}

	/**
	 * 从静态变量applicationContext中取得Bean, 自动转型为所赋值对象的类型.
	 */
	public static <T> T getBean(Class<T> requiredType) {
		return applicationContext.getBean(requiredType);
	}


	public static <T> T getBean(String name, Class<T> requiredType) {
		return applicationContext.getBean(name, requiredType);
	}

	public static boolean containsBean(String name) {
		return applicationContext.containsBean(name);
	}

	public static boolean isSingleton(String name) {
		return applicationContext.isSingleton(name);
	}

	public static Class<? extends Object> getType(String name) {
		return applicationContext.getType(name);
	}

}
```

## Springboot-web

只需要引入一个依赖

```xml
<dependency>
	<groupId>org.springframework.boot</groupId>
	<artifactId>spring-boot-starter-web</artifactId>
</dependency>
```

```java
@RestController
public class HelloController {

    @Autowired
    HelloService hs;

    @GetMapping("hello")
    public String hello(){
        hs.hello();
        return "hello";
    }
}
```

