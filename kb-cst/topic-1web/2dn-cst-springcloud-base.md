
# springcloud基础 

## springcloud微服务套件

[Spring Cloud Alibaba](https://sca.aliyun.com/)

网关：gateway 支持限流

配制中心：nacos config

注册中心：nacos discovery

RPC: openfeign Feign通过接口标注方式，将Ribbon的实现用接口代理。

限流: sentinel

日志追踪：sleuth

链路监控：pinpoint

## springcloud项目搭建

新建springboot项目。

Spring boot 引入

```xml
<parent>
      <groupId>org.springframework.boot</groupId>
      <artifactId>spring-boot-starter-parent</artifactId>
      <version>xxx</version>
</parent>
```
spring-web引入

```xml
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>
```
spring cloud 引入, 在dependencyManagement添加版本管理

```xml
<dependency>
      <groupId>org.springframework.cloud</groupId>
      <artifactId>spring-cloud-dependencies</artifactId>
      <version>${spring.cloud.version}</version>
      <type>pom</type>
      <scope>import</scope>
</dependency>
```
Spring cloud alibaba引入, 在dependencyManagement添加版本管理

```xml
<dependency>
      <groupId>com.alibaba.cloud</groupId>
      <artifactId>spring-cloud-alibaba-dependencies</artifactId>
      <version>${spring.cloud.alibaba.version}</version>
      <type>pom</type>
      <scope>import</scope>
</dependency>
```
引入cloud相关包

```xml
        <dependency>
            <groupId>com.alibaba.cloud</groupId>
            <artifactId>spring-cloud-starter-alibaba-nacos-discovery</artifactId>
        </dependency>
        <dependency>
            <groupId>com.alibaba.cloud</groupId>
            <artifactId>spring-cloud-starter-alibaba-nacos-config</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.cloud</groupId>
            <artifactId>spring-cloud-starter-openfeign</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.cloud</groupId>
            <artifactId>spring-cloud-starter-sleuth</artifactId>
        </dependency>
```

gateway依赖，需要先引入cloud和cloud alibaba，再引入gateway和注册中心

``` xml
<dependency>
    <groupId>com.alibaba.cloud</groupId>
    <artifactId>spring-cloud-starter-alibaba-nacos-discovery</artifactId>
</dependency>
<dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-starter-gateway</artifactId>
</dependency>
```

## bootstrap.yml配制

```yaml
server:
  servlet:
    context-path: /

spring:
  application:
    name: edu-cloud-center
  cloud:
    nacos:
      discovery:
        enabled: true
        service: ${spring.application.name}
      config:
        enabled: true
        encode: UTF-8
        file-extension: yaml
```

### 禁用问题

```
09:58:18.046 [main] DEBUG org.springframework.boot.diagnostics.LoggingFailureAnalysisReporter - Application failed to start due to an exception
org.springframework.cloud.commons.ConfigDataMissingEnvironmentPostProcessor$ImportException: No spring.config.import set
```

产生问题的原因是bootstrap.properties比application.properties的优先级要高，由于bootstrap.properties是系统级的资源配置文件，是用在程序引导执行时更加早期配置信息读取；而application.properties是用户级的资源配置文件，是用来后续的一些配置所需要的公共参数。但是在SpringCloud 2020.* 版本把bootstrap禁用了，导致在读取文件的时候读取不到而报错，所以我们只要把bootstrap重新导入进来就会生效了。

方式一：添加配置

```yaml
spring:
    cloud:
        nacos:
            config:
                import-check:
                    enabled: false
```

方式二：添加pom

```xml
<!--开启Spring Cloud 应用程序启动时加载bootstrap配置文件-->
<dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-starter-bootstrap</artifactId>
    <version>3.1.4</version>
</dependency>
```

方式三：配置加到application.properties



## Ribbon

restTemplate用注解@LoadBalance标后，会将服务名解析成ip:port生成调用地址。
