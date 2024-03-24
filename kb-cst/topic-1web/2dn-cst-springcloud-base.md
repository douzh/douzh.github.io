
# springcloud基础 

## springcloud微服务套件

[Spring Cloud Alibaba wiki](https://github.com/alibaba/spring-cloud-alibaba/wiki)

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

bootstrap.yml配制

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

## Ribbon

restTemplate用注解@LoadBalance标后，会将服务名解析成ip:port生成调用地址。
