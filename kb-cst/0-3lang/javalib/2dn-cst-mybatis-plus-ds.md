
# dynamic-datasource动态数据源使用
  
``` xml
  <dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>dynamic-datasource-spring-boot-starter</artifactId>
    <version>3.5.2</version>
  </dependency>
```

```yml
spring:
  datasource:
    dynamic:
      primary: master
      strict: false #严格匹配数据源,默认false. true未匹配到指定数据源时抛异常,false使用默认数据源
      datasource:
        master:
          url: jdbc:mysql://localhost:3306/test
          username: root
          password: 123456
          driver-class-name: com.mysql.cj.jdbc.Driver
        slave_1:
          url: jdbc:mysql://localhost:3306/test2
          username: root
          password: 123456
          driver-class-name: com.mysql.cj.jdbc.Driver
```

``` sql
CREATE TABLE `user` (
  `id` bigint NOT NULL AUTO_INCREMENT,
  `name` varchar(100) NOT NULL,
  `age` int NOT NULL,
  `email` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
```

``` java
  public interface UserMapper extends BaseMapper<User> {
  }

  public class User {
    private Long id;
    private String name;
    private Integer age;
    private String email;
  }
```


## 手工切换数据源

DynamicDataSourceContextHolder 用于动态切换数据源的上下文工具类。在使用多数据源的情况下，它可以帮助在运行时选择要使用的数据源。

存储当前线程的数据源标识：DynamicDataSourceContextHolder使用线程本地变量（ThreadLocal）来存储当前线程所使用的数据源标识。这允许在同一应用程序中的不同线程使用不同的数据源。

``` java
@SpringBootApplication
public class MybatisApplication {
    public static void main(String[] args) {
        ConfigurableApplicationContext cxt = SpringApplication.run(MybatisApplication.class, args);
        DynamicDataSourceContextHolder.push("master");
        UserMapper userMapper = cxt.getBean("userMapper", UserMapper.class);
        List<User> userList = userMapper.selectList(null);
        DynamicDataSourceContextHolder.poll();
        userList.forEach(System.out::println);
        DynamicDataSourceContextHolder.push("slave_1");
        userList = userMapper.selectList(null);
        DynamicDataSourceContextHolder.poll();
        userList.forEach(System.out::println);
    }
}
```
