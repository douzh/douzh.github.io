
# mybatis-plus 源码分析

[MyBatis-Plus](https://baomidou.com/)

## 特性

- 无侵入：只做增强不做改变，引入它不会对现有工程产生影响，如丝般顺滑
- 损耗小：启动即会自动注入基本 CURD，性能基本无损耗，直接面向对象操作
- 强大的 CRUD 操作：内置通用 Mapper、通用 Service，仅仅通过少量配置即可实现单表大部分 CRUD 操作，更有强大的条件构造器，满足各类使用需求
- 支持 Lambda 形式调用：通过 Lambda 表达式，方便的编写各类查询条件，无需再担心字段写错
- 支持主键自动生成：支持多达 4 种主键策略（内含分布式唯一 ID 生成器 - Sequence），可自由配置，完美解决主键问题
- 支持 ActiveRecord 模式：支持 ActiveRecord 形式调用，实体类只需继承 Model 类即可进行强大的 CRUD 操作
- 支持自定义全局通用操作：支持全局通用方法注入（ Write once, use anywhere ）
- 内置代码生成器：采用代码或者 Maven 插件可快速生成 Mapper 、 Model 、 Service 、 Controller 层代码，支持模板引擎，更有超多自定义配置等您来使用
- 内置分页插件：基于 MyBatis 物理分页，开发者无需关心具体操作，配置好插件之后，写分页等同于普通 List 查询
- 分页插件支持多种数据库：支持 MySQL、MariaDB、Oracle、DB2、H2、HSQL、SQLite、Postgre、SQLServer 等多种数据库
- 内置性能分析插件：可输出 SQL 语句以及其执行时间，建议开发测试时启用该功能，能快速揪出慢查询
- 内置全局拦截插件：提供全表 delete 、 update 操作智能分析阻断，也可自定义拦截规则，预防误操作

## springboot 示例

引入依赖

``` xml
<dependency>
<groupId>com.baomidou</groupId>
<artifactId>mybatis-plus-boot-starter</artifactId>
<version>3.5.3.1</version>
</dependency>
```

添加类文件

``` java
@Data
public class User {
    private Long id;
    private String name;
    private Integer age;
    private String email;
}

public interface UserMapper extends BaseMapper<User> {

}
@Service
public class UserService extends ServiceImpl<UserMapper, User> {

}

@Configuration
@MapperScan("com.onekbase.demo.mybatis.mapper")
public class MybatisConfig {

}
```

调用测试

``` java
  public static void main(String[] args) {
        ConfigurableApplicationContext cxt = SpringApplication.run(MybatisApplication.class, args);
        System.out.println("------------------- mapper test");
        UserMapper userMapper = cxt.getBean("userMapper", UserMapper.class);
        List<User> userList = userMapper.selectList(null);
        userList.forEach(System.out::println);
        System.out.println("------------------- server test");
        UserService us =cxt.getBean("userService", UserService.class);
        List<User> userList2 = us.list();
        userList2.forEach(System.out::println);
        System.out.println("------------------- wrapper test");
//        List<User> userList3 = us.list(Wrappers.<User>query()
//                .eq("name","Tom")
//                .eq("age","28"));
        List<User> userList3 = us.list(Wrappers.<User>lambdaQuery()
                .eq(User::getName,"Tom")
                .eq(User::getAge,"28"));
        userList3.forEach(System.out::println);
        System.out.println("------------------- page test");
        Page<User> page=new Page<>(1,2);
        Page<User> userPage =userMapper.selectPage(page,
                Wrappers.<User>lambdaQuery()
                .lt(User::getAge,28));
        System.out.println("当前页:"+ userPage.getCurrent());
        System.out.println("每页记录数:"+userPage.getSize());
        System.out.println("总记录数:"+userPage.getTotal());
        System.out.println("总页数:"+userPage.getPages());
        List<User> ul = userPage.getRecords();
        ul.forEach(System.out::println);
  }
```

## spring 示例

引入依赖

``` xml
<dependency>
<groupId>com.baomidou</groupId>
<artifactId>mybatis-plus</artifactId>
<version>3.5.3</version>
</dependency>
```

配置

``` java
@Configuration  // 相当于 <bean>  </beans>
@ComponentScan("com.onekbase.demo.mybatis")   // context:component-scan
public class SpringConfig {
}

@Configuration
@PropertySource({"jdbc.properties"})    // 配置加载jdbc.properties文件 使用$符获取数据
public class JdbcConfig {

    @Value("${jdbc.driver}")
    private String driver;
    @Value("${jdbc.url}")
    private String url;
    @Value("${jdbc.username}")
    private String username;
    @Value("${jdbc.password}")
    private String password;

    @Bean
    public DataSource dataSource(){
        DriverManagerDataSource dataSource =new DriverManagerDataSource();
        dataSource.setDriverClassName(driver);
        dataSource.setUrl(url);
        dataSource.setUsername(username);
        dataSource.setPassword(password);
        return dataSource;
    }
}

@Configuration
@MapperScan("com.onekbase.demo.mybatis.mapper")
public class MybatisConfig {
    @Bean
    public MybatisSqlSessionFactoryBean sqlSessionFactory(DataSource dataSource){
        MybatisSqlSessionFactoryBean sqfb =new MybatisSqlSessionFactoryBean();
        sqfb.setDataSource(dataSource);
        Properties prop=new Properties();
        prop.put("logImpl","org.apache.ibatis.logging.stdout.StdOutImpl");
        sqfb.setConfigurationProperties(prop);
        return sqfb;
    }
}
```

调用测试同springboot

## 源码分析

plus的封装逻辑：

1.  SqlSessionTemplate核心未动
2.  Builder、Configuration和相关装配类复制mybatis代码添加了解析内容
3.  核心是解析出BaseMapper方法对应的MappedStatement

### 修改的核心类

-   SqlSessionFactoryBean: MybatisSqlSessionFactoryBean
-   SqlSessionFactoryBuilder: MybatisSqlSessionFactoryBuilder 继承
-   Configuration: MybatisConfiguration 继承

### BaseMapper基本方法的注入

基本逻辑:

1.  对BaseMapper每个方法开发注入器
2.  注入器用model对象生成对应方法的sql，MybatisPlus 注解在这里启作用
3.  languageDriver.createSqlSource(configuration, sql, modelClass)
4.  addMappedStatement

MybatisMapperAnnotationBuilder的parse方法

``` java
// TODO 注入 CURD 动态 SQL , 放在在最后, because 可能会有人会用注解重写sql
try {
  // https://github.com/baomidou/mybatis-plus/issues/3038
  if (GlobalConfigUtils.isSupperMapperChildren(configuration, type)) {
    parserInjector();
  }
} catch (IncompleteElementException e) {
  configuration.addIncompleteMethod(new InjectorResolver(this));
}
```

DefaultSqlInjector.getMethodList 获取所有自动注入的Inject

``` java
public List<AbstractMethod> getMethodList(Class<?> mapperClass, TableInfo tableInfo) {
        Stream.Builder<AbstractMethod> builder = Stream.<AbstractMethod>builder()
            .add(new Insert())
            .add(new Delete())
            .add(new DeleteByMap())
            .add(new Update())
            .add(new SelectByMap())
            .add(new SelectCount())
            .add(new SelectMaps())
            .add(new SelectMapsPage())
            .add(new SelectObjs())
            .add(new SelectList())
            .add(new SelectPage());
        if (tableInfo.havePK()) {
            builder.add(new DeleteById())
                .add(new DeleteBatchByIds())
                .add(new UpdateById())
                .add(new SelectById())
                .add(new SelectBatchByIds());
        } else {
            logger.warn(String.format("%s ,Not found @TableId annotation, Cannot use Mybatis-Plus 'xxById' Method.",
                tableInfo.getEntityType()));
        }
        return builder.build().collect(toList());
    }
```

AbstractSqlInjector.inspectInject注入MappedStatement

-   AbstractMethod.inject注入单个方法

### IService基本方法注入

Service继承ServiceImpl，ServiceImpl中自动Autowired了baseMapper，所有调用都是基于BaseMapper做了封装

``` java
public class ServiceImpl<M extends BaseMapper<T>, T> implements IService<T> {
    protected Log log = LogFactory.getLog(this.getClass());
    @Autowired
    protected M baseMapper;
```
