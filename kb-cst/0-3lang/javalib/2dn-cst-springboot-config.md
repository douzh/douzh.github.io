# springboot配置方式

在Spring Boot中，可以通过多种方式来实现配置文件的相互读取和组合。如果你想要在一个Spring Boot应用中使用多个YAML（.yml）配置文件，并且希望这些配置文件可以相互读取或者互相覆盖某些配置，你可以采用以下几种方法：

## spring.config.import

使用spring.config.import属性（Spring Boot 2.4及以上版本）

从Spring Boot 2.4开始，引入了spring.config.import属性，允许你导入其他配置文件。例如，在你的application.yml中，你可以这样做：

```yaml
spring:
  config:
    import: classpath:additional-config.yml
```

这将会导入同一类路径下的additional-config.yml文件。

## @PropertySource

使用@PropertySource或@PropertySources注解

虽然@PropertySource注解不支持YAML格式的文件，但你可以使用它来加载.properties格式的文件。如果你的配置可以转换为.properties格式，这是一个可行的选择。

```yaml
@Configuration
@PropertySource("classpath:additional-config.properties")
public class AppConfig {
    // ...
}
```
对于多个配置文件，可以使用@PropertySources注解。

## spring.profiles.include

使用spring.profiles.include

在你的主配置文件中，你可以指定要包含的其他配置文件的profiles。例如，在application.yml中：

```yaml
spring:
  profiles:
    include: 
      - additional
```

然后，你可以有一个名为application-additional.yml的文件，它将被自动加载。

## 使用profiles

你可以在一个YAML文件中定义多个profiles，每个profile有不同的配置。然后，你可以在运行时通过设置spring.profiles.active来指定哪个profile是活跃的。

```yaml
spring:
  profiles: dev
---
spring:
  profiles: prod
```

然后通过命令行参数来激活特定的profile：

```java
java -jar yourapp.jar --spring.profiles.active=prod
```

注意事项

- 当 spring.profiles.active 被设置时，Spring Boot 会合并 application.yml（或 application.properties）和对应 profile 的配置文件（如 application-local.yml）。如果有任何冲突的配置，profile 特定的配置将覆盖主配置文件中的设置。
- 你可以同时激活多个 profiles，只需在设置时用逗号分隔它们，例如 local,dev。
- 使用 profile 时，确保你的配置文件命名遵循 application-{profile}.yml 的格式。例如，对于 local profile，文件应该命名为 application-local.yml。

如，各个模块都有自己的yml文件，可以通过spring.profiles.active属性来激活多个配置文件。

```yaml
spring:
  profiles:
    active: provider,quarstz,consumer
```

## 使用不同的配置文件名

Spring Boot默认会读取application.yml或application.properties文件。你可以通过在启动时指定不同的配置文件名来加载不同的配置文件。例如：

``` java
java -jar yourapp.jar --spring.config.name=another-application
```

这将会加载another-application.yml而不是application.yml。

