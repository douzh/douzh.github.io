### 引入依赖

```
<dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-starter-sleuth</artifactId>
    <version>2.2.3.RELEASE</version>
</dependency>
```

### 配制logback

sleuth将跟踪信息添加到logback的MDC中，需要配制pattern打印出来。

```
<springProperty scope="context" name="applicationName" source="spring.application.name"/>
<property name="log.pattern" value="[${applicationName},%X{X-B3-TraceId},%X{X-B3-SpanId},%X{X-Span-Export}] %d{yyyy-MM-dd HH:mm:ss.SSS} [%thread] %-5level %logger{50} - [%method,%line] - %msg%n"/>

```

