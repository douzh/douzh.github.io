
# mybatis源码分析

[mybatis官网中文文档](https://mybatis.org/mybatis-3/zh/)


## 相关设计模式总结

-   builer 配合 解释器解析配置 是很好的可扩展方案
-   配置主要是配合策略和模板指定构建的实现
-   建造者模式
    -   SqlSessionFactoryBuilder 构建SqlSessionFactory
-   工厂方法
    -   MapperProxyFactory 配合代理模式的工厂方法
-   解释器模式 配置和注解的解释
    -   XMLConfigBuilder 解析Configuration
    -   XMLMapperBuilder 解析mapper
-   策略模式 很多接口
    -   SqlSessionFactory
    -   SqlSession
    -   Interceptor
    -   Executor 模板
    -   StatementHandler
    -   ResultSetHandler
    -   SqlSource
    -   SqlNode
    -   TypeHandler
    -   Transaction
    -   ResultHandler
-   模板模式
    -   Executor 模板为BaseExecutor
-   代理模式
    -   插件 mapper
    -   RoutingStatementHandler代理StatementHandler
-   适配器模式
    -   SqlSession insert delete是对Executor.update的包装

## 常见问题

### 打印sql

在application.yml配置文件中增加MyBatis的configuration配置：

``` yaml
mybatis:
  configuration:
    log-impl: org.apache.ibatis.logging.stdout.StdOutImpl
```

### 一级缓存和二级缓存

一级缓存：存在于单个会话之中，当会话提交或者关闭，会清空缓存，缓存数据存在会话中的executor的localcache中，缓存sql的数据被修改也会清空缓存，但当我们使用分布式部署时，每个机器上的每个会话独享自己的缓存，且每个机器无法共享一级缓存上数据是否被修改，这就导致一种情况，A，B服务器上A、B会话之前都缓存了某条数据，现在A服务上修改了该条数据，A服务A会话的缓存被清空了，但B服务器的B会话的缓存仍然是之前的数据，不知道该数据已经被更改，导致在B服务器上执行的脏读。

调整一级缓存的作用范围，设置 LocalCache 级别为 statement 或者在语句上设置 flushCache=“true” 都可以。目的就是使一级缓存失效。

开启二级缓存后，会使用CachingExecutor装饰Executor，进入一级缓存的查询流程前，先在CachingExecutor进行二级缓存的查询，二级缓存没找到再去一级缓存找，一级缓存也没有就直接查数据库； 但二级缓存对于多表查询容易产生脏读，因为他的缓存是基于namaspace的，如果跨越了namespace其他命名空间去修改了数据，我们是监测不到的，就导致当前的命名空间查询出来的缓存数据是存在问题的； 分布式环境下，由于默认的MyBatis Cache实现都是基于本地的，分布式环境下必然会出现读取到脏数据，可以通过Cache接口实现，但那玩意想想都复杂，成本太大，不如直接用分布式的redis做数据缓存；

一级缓存用处不大，默认开启，分布式下不能使用； 二级缓存使用联表查询可能出问题，条件比较苛刻，所以还是不用比较好；但如果对数据一致要求不是那么高，倒也无所谓了； 单体架构推荐spring提供的ehcache做数据缓存； 分布式架构直接使用redis做数据缓存；

## springboot样例搭建

pom.xml引入依赖

```xml
<dependency>
    <groupId>org.mybatis.spring.boot</groupId>
    <artifactId>mybatis-spring-boot-starter</artifactId>
    <version>2.1.0</version>
</dependency>
```

## java样例搭建

建表

``` sql
CREATE TABLE `sys_config` (
  `ckey` varchar(100) NOT NULL COMMENT '配制键',
  `cvalue` varchar(1000) NOT NULL COMMENT '配制值',
  `remark` varchar(1000) DEFAULT NULL COMMENT '备注',
  PRIMARY KEY (`ckey`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='小饭卡配制表';
```

建接口和xml

``` java

public class SysConfig {

    private String ckey;
    private String cValue;
    private String remark;

    // getter setter
    ......

    @Override
    public String toString() {
        return "SysConfig{" +
                "ckey='" + ckey + '\'' +
                ", cValue='" + cValue + '\'' +
                ", remark='" + remark + '\'' +
                '}';
    }
}

public interface SysConfigMapper {

    public SysConfig selectConfig(String id);
}
```

``` xml
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE mapper
        PUBLIC "-//mybatis.org//DTD Mapper 3.0//EN"
        "https://mybatis.org/dtd/mybatis-3-mapper.dtd">
<mapper namespace="com.onekbase.demo.mybatis.mapper.SysConfigMapper">
    <select id="selectConfig" resultType="com.onekbase.demo.mybatis.model.SysConfig">
        select * from sys_config where ckey = #{id}
    </select>
</mapper>
```

mybatis-config.xml

``` xml
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE configuration
        PUBLIC "-//mybatis.org//DTD Config 3.0//EN"
        "http://mybatis.org/dtd/mybatis-3-config.dtd">
<configuration>
    <environments default="development">
        <environment id="development">
            <transactionManager type="JDBC"/>
            <dataSource type="POOLED">
                <property name="driver" value="com.mysql.jdbc.Driver"/>
                <property name="url" value="jdbc:mysql://localhost:3306/test"/>
                <property name="username" value="root"/>
                <property name="password" value="123456"/>
            </dataSource>
        </environment>
    </environments>
    <mappers>
        <mapper resource="com/onekbase/demo/mybatis/mapper/SysConfigMapper.xml"/>
    </mappers>
</configuration>
```

pom.xml

``` xml
<dependencies>
    <dependency>
        <groupId>org.mybatis</groupId>
        <artifactId>mybatis</artifactId>
        <version>3.5.13</version>
    </dependency>
    <dependency>
        <groupId>mysql</groupId>
        <artifactId>mysql-connector-java</artifactId>
        <version>8.0.33</version>
    </dependency>

</dependencies>
<build>
    <resources>
        <resource>
            <directory>src/main/java/</directory>
            <includes>
                <include>**/*.xml</include>
            </includes>
        </resource>
        <resource>
            <directory>src/main/resources/</directory>
            <includes>
                <include>**/*.xml</include>
            </includes>
        </resource>
    </resources>
</build>
```

D01BuildFromConfigXmlMain

``` java
public class D01BuildFromConfigXmlMain {
    public static void main(String[] args) throws IOException {
        String resource = "mybatis-config.xml";
        InputStream inputStream = Resources.getResourceAsStream(resource);
        SqlSessionFactoryBuilder builder = new SqlSessionFactoryBuilder();
        SqlSessionFactory factory = builder.build(inputStream);

        try (SqlSession session = factory.openSession()) {
            Object o = session.selectOne("com.onekbase.demo.mybatis.mapper.SysConfigMapper.selectConfig", "key.hello");
            System.out.println(o);
        }
    }
}
```

### 拦截器

``` java
@Intercepts({@Signature(
        type= Executor.class,
        method = "query",
        args = {MappedStatement.class,Object.class, RowBounds.class, ResultHandler.class})})
public class ExamplePlugin implements Interceptor {
    private Properties properties = new Properties();

    @Override
    public Object intercept(Invocation invocation) throws Throwable {
        // implement pre processing if need
        System.out.println("ExamplePlugin starter");
        Object returnObject = invocation.proceed();
        System.out.println("ExamplePlugin end");
        // implement post processing if need
        return returnObject;
    }

    @Override
    public void setProperties(Properties properties) {
        this.properties = properties;
    }
}
```

``` xml
<plugins>
  <plugin interceptor="com.onekbase.demo.mybatis.intercept.ExamplePlugin">
    <property name="someProperty" value="100"/>
  </plugin>
  <plugin interceptor="com.onekbase.demo.mybatis.intercept.ExamplePlugin2">
    <property name="someProperty" value="200"/>
  </plugin>
</plugins>
```

## mybatis源码分析

### 代码初始化SqlSessionFactory

``` java
public class D02BuildFromCodeMain {

    public static void main(String[] args) {
        Properties props = new Properties();
        props.put("driver","com.mysql.jdbc.Driver");
        props.put("url","jdbc:mysql://localhost:3306/test");
        props.put("username","root");
        props.put("password","123456");
        DataSourceFactory dsFactory = new PooledDataSourceFactory();
//        DataSourceFactory dsFactory = new UnpooledDataSourceFactory();
        dsFactory.setProperties(props);
        DataSource dataSource = dsFactory.getDataSource();
        TransactionFactory txFactory = new JdbcTransactionFactory();
        Environment environment = new Environment("development", txFactory, dataSource);
        Configuration configuration = new Configuration(environment);
        configuration.addMapper(SysConfigMapper.class);
        SqlSessionFactoryBuilder builder = new SqlSessionFactoryBuilder();
        SqlSessionFactory factory = builder.build(configuration);

        try (SqlSession session = factory.openSession()) {
            Object o = session.selectOne("com.onekbase.demo.mybatis.mapper.SysConfigMapper.selectConfig", "key.hello");
            System.out.println(o);
        }
    }
}
```

### mapper形式调用

``` java
public class D03MapperMain {
    public static void main(String[] args) throws IOException {
        String resource = "mybatis-config.xml";
        InputStream inputStream = Resources.getResourceAsStream(resource);
        SqlSessionFactoryBuilder builder = new SqlSessionFactoryBuilder();
        SqlSessionFactory factory = builder.build(inputStream);
        try (SqlSession session = factory.openSession()) {
            SysConfigMapper mapper = session.getMapper(SysConfigMapper.class);
            SysConfig sc=mapper.selectConfig("key.hello");
            System.out.println(sc);
        }
    }
}
```

### SqlSessionFactory的创建

SqlSessionFactoryBuilder通过配置文件创建Configuration对象，再build出SqlSessionFactory

加载的配置：

-   configuration（配置）
    -   properties（属性）
    -   settings（设置）
    -   typeAliases（类型别名）
    -   typeHandlers（类型处理器）
    -   objectFactory（对象工厂）
    -   plugins（插件）
    -   environments（环境配置）
        -   environment（环境变量）
            -   transactionManager（事务管理器）
            -   dataSource（数据源）
    -   databaseIdProvider（数据库厂商标识）
    -   mappers（映射器）

XMLConfigBuilder 解析配置文件，构建Configuration对象

``` java
private void parseConfiguration(XNode root) {
    try {
        // issue #117 read properties first
        propertiesElement(root.evalNode("properties"));
        Properties settings = settingsAsProperties(root.evalNode("settings"));
        loadCustomVfs(settings);
        loadCustomLogImpl(settings);
        typeAliasesElement(root.evalNode("typeAliases"));
        pluginElement(root.evalNode("plugins"));
        objectFactoryElement(root.evalNode("objectFactory"));
        objectWrapperFactoryElement(root.evalNode("objectWrapperFactory"));
        reflectorFactoryElement(root.evalNode("reflectorFactory"));
        settingsElement(settings);
        // read it after objectFactory and objectWrapperFactory issue #631
        environmentsElement(root.evalNode("environments"));
        databaseIdProviderElement(root.evalNode("databaseIdProvider"));
        typeHandlerElement(root.evalNode("typeHandlers"));
        mapperElement(root.evalNode("mappers"));
    } catch (Exception e) {
        throw new BuilderException("Error parsing SQL Mapper Configuration. Cause: " + e, e);
    }
}
```

最复杂的为mapper的解析，主要向Configuration注入了两个信息：

-   Configuration: Map\<String, MappedStatement\> mappedStatements
    方法名和具体Statement信息
-   Configuration: MapperRegistry对象 Map\<Class\<?\>,
    MapperProxyFactory\<?\>\> 接口名和代理工厂

注入流程：

-   package: 描到接口，按接口方式添加
-   resource: mapper的xml配置添加，实现类XMLMapperBuilder
    -   XMLMapperBuilder
    -   parse
    -   buildStatementFromContext
        -   XMLStatementBuilder.parseStatementNode
            -   MapperBuilderAssistant.addMappedStatement
                -   Configuration.addMappedStatement
-   url: 通过url加载xml配置添加，实现类XMLMapperBuilder
-   class: 通过configuration.addMapper接口添加
    -   MapperAnnotationBuilder
        -   parse
        -   loadXmlResource 通过接口类查询对应的xml文件
            -   XMLMapperBuilder.parse

``` java
if ("package".equals(child.getName())) {
    String mapperPackage = child.getStringAttribute("name");
    configuration.addMappers(mapperPackage);
}
......
String resource = child.getStringAttribute("resource");
String url = child.getStringAttribute("url");
String mapperClass = child.getStringAttribute("class");
if (resource != null && url == null && mapperClass == null) {
    ErrorContext.instance().resource(resource);
    try (InputStream inputStream = Resources.getResourceAsStream(resource)) {
        XMLMapperBuilder mapperParser = new XMLMapperBuilder(inputStream, configuration, resource,
                                                             configuration.getSqlFragments());
        mapperParser.parse();
    }
} else if (resource == null && url != null && mapperClass == null) {
    ErrorContext.instance().resource(url);
    try (InputStream inputStream = Resources.getUrlAsStream(url)) {
        XMLMapperBuilder mapperParser = new XMLMapperBuilder(inputStream, configuration, url,
                                                             configuration.getSqlFragments());
        mapperParser.parse();
    }
} else if (resource == null && url == null && mapperClass != null) {
    Class<?> mapperInterface = Resources.classForName(mapperClass);
    configuration.addMapper(mapperInterface);
} 
```

### 动态sql

sql载体:MappedStatement里的SqlSource

-   MappedStatement.getBoundSql返回BoundSql，这过程解析动态标签
-   SqlSource有多个实现：
    -   RawSqlSource: 没有动态标签的sql，有变量可以
    -   DynamicSqlSource：有if foreach之类标签
        -   SqlNode 接口
        -   ChooseSqlNode choose标签的实现
        -   ForEachSqlNode foreach标签的实现
        -   IfSqlNode if标签的实现
        -   TrimSqlNode trim标签的实现
            -   WhereSqlNode where标签的实现
            -   SetSqlNode set标签的实现

构建过程：

-   XMLStatementBuilder 和
    MapperAnnotationBuilder在构建MappedStatement过程中
    -   XMLLanguageDriver.createSqlSource
        -   XMLScriptBuilder.parseScriptNode
            -   parseDynamicTags 解析出MixedSqlNode
            -   根据节点sql类型生成DynamicSqlSource或RawSqlSource

构建结果：

-   MappedStatement
    -   SqlSource 分两类
        -   DynamicSqlSource 动态sql的元数据
            -   MixedSqlNode 对sql node列表的一个包装
                -   List\<SqlNode\> contents 一个sql中所有node的列表
        -   RawSqlSource

``` java
private void initNodeHandlerMap() {
  nodeHandlerMap.put("trim", new TrimHandler());
  nodeHandlerMap.put("where", new WhereHandler());
  nodeHandlerMap.put("set", new SetHandler());
  nodeHandlerMap.put("foreach", new ForEachHandler());
  nodeHandlerMap.put("if", new IfHandler());
  nodeHandlerMap.put("choose", new ChooseHandler());
  nodeHandlerMap.put("when", new IfHandler());
  nodeHandlerMap.put("otherwise", new OtherwiseHandler());
  nodeHandlerMap.put("bind", new BindHandler());
}
```

查询解析过程：

-   BaseExecutor在执行查询前选获取BoundSql
    -   DynamicSqlSource在getBoundSql过程中解析动态标签
        -   创建一个DynamicContext
        -   调用SqlNode的apply解析自己拼接到DynamicContext

### openSession创建SqlSession

DefaultSqlSessionFactory

``` java
private SqlSession openSessionFromDataSource(ExecutorType execType, TransactionIsolationLevel level,
    boolean autoCommit) {
  Transaction tx = null;
  try {
    final Environment environment = configuration.getEnvironment();
    final TransactionFactory transactionFactory = getTransactionFactoryFromEnvironment(environment);
    tx = transactionFactory.newTransaction(environment.getDataSource(), level, autoCommit);
    final Executor executor = configuration.newExecutor(tx, execType);
    return new DefaultSqlSession(configuration, executor, autoCommit);
  } catch (Exception e) {
    closeTransaction(tx); // may have fetched a connection so lets call close()
    throw ExceptionFactory.wrapException("Error opening session.  Cause: " + e, e);
  } finally {
    ErrorContext.instance().reset();
  }
}
```

Configuration execType默认为Simple类型

``` java
public Executor newExecutor(Transaction transaction, ExecutorType executorType) {
  executorType = executorType == null ? defaultExecutorType : executorType;
  Executor executor;
  if (ExecutorType.BATCH == executorType) {
    executor = new BatchExecutor(this, transaction);
  } else if (ExecutorType.REUSE == executorType) {
    executor = new ReuseExecutor(this, transaction);
  } else {
    executor = new SimpleExecutor(this, transaction);
  }
  if (cacheEnabled) {
    executor = new CachingExecutor(executor);
  }
  return (Executor) interceptorChain.pluginAll(executor);
}
```

### SqlSession

SqlSession的默认实现为DefaultSqlSession

-   selectOne: 最终调用的selectList选第一个
-   selectMap: 最终调用的selectList
-   select: : 最终调用的selectList
-   selectList: 可以定义ResultHandler处理结果
-   selectCursor: 返回游标处理数据
-   insert: 最终调用的update
-   delete: 最终调用的update
-   update
-   commit
-   rollback
-   getMapper

SqlSession的所有操作最终都是调用创建时设置的SimpleExecutor的具体方法

-   DefaultSqlSession最终执行的是Executor接口的实现
    -   BaseExecutor模板实现
    -   SimpleExecutor为具体实现类
    -   CachingExecutor最终调用的SimpleExecutor加载数据

通过传入的方法全地址定位查询到MappedStatement

MappedStatement ms = configuration.getMappedStatement(statement);

### 拦截器

-   在初始化时在InterceptorChain添加Interceptor的实现
-   Configuration中可以创建拦截器的对象
    -   (Executor) interceptorChain.pluginAll(executor);
    -   (StatementHandler) interceptorChain.pluginAll(statementHandler);
    -   (ParameterHandler) interceptorChain.pluginAll(parameterHandler);
    -   (ResultSetHandler) interceptorChain.pluginAll(resultSetHandler);
-   最后通过interceptorChain.pluginAll(executor)包装成代理

这里没有用责任链模式，而是用Proxy层层包装实现

拦截器最终的实现类为：Plugin

``` java
private final Object target; // 代理对象
private final Interceptor interceptor; // 开发的拦截器对象
private final Map<Class<?>, Set<Method>> signatureMap; // 要拦截的类和方法
```

中间的注册过程

``` java
public class InterceptorChain {

    private final List<Interceptor> interceptors = new ArrayList<>();

    public Object pluginAll(Object target) {
        for (Interceptor interceptor : interceptors) {
            target = interceptor.plugin(target);
        }
        return target;
    }

    public void addInterceptor(Interceptor interceptor) {
        interceptors.add(interceptor);
    }

    public List<Interceptor> getInterceptors() {
        return Collections.unmodifiableList(interceptors);
    }

}

public interface Interceptor {

    Object intercept(Invocation invocation) throws Throwable;

    default Object plugin(Object target) {
        return Plugin.wrap(target, this);
    }

    default void setProperties(Properties properties) {
        // NOP
    }

}

public static Object wrap(Object target, Interceptor interceptor) {
    Map<Class<?>, Set<Method>> signatureMap = getSignatureMap(interceptor);
    Class<?> type = target.getClass();
    Class<?>[] interfaces = getAllInterfaces(type, signatureMap);
    if (interfaces.length > 0) {
        return Proxy.newProxyInstance(type.getClassLoader(), interfaces, new Plugin(target, interceptor, signatureMap));
    }
    return target;
}
```

### Executor分析

Executor的默认实现为SimpleExecutor

-   doQuery
-   doUpdate
-   doQueryCursor
-   query: 最终调用的doQuery
-   queryCursor: 最终调用的doQueryCursor
-   update: 最终调用的doUpdate

Executor最终调用的是StatementHandler实现类

在调用之前会统一对handler做预处理

-   设置数据库Connection
-   设置Statement
-   对PreparedStatement绑定参数，这里就是sql中变量替换的地方

``` java
private Statement prepareStatement(StatementHandler handler, Log statementLog) throws SQLException {
    Statement stmt;
    Connection connection = getConnection(statementLog);
    stmt = handler.prepare(connection, transaction.getTimeout());
    handler.parameterize(stmt);
    return stmt;
}
```

Configuration.newStatementHandler

-   RoutingStatementHandler代理StatementHandler的具体实现，用StatementType分类创建
-   BaseStatementHandler：StatementHandler的模板类，从connect取到Statement，并设置参数
    -   PreparedStatementHandler
    -   SimpleStatementHandler
    -   CallableStatementHandler

### StatementHandler分析

StatementHandler负责

-   设置SQL参数: ParameterHandler.setParameters
-   执行Statement
-   处理返回结果: ResultSetHandler

StatementHandler的查询结果最终由ResultSetHandler的实现来解析，只有一个实现类 DefaultResultSetHandler

``` java
@Override
public <E> List<E> query(Statement statement, ResultHandler resultHandler) throws SQLException {
  PreparedStatement ps = (PreparedStatement) statement;
  ps.execute();
  return resultSetHandler.handleResultSets(ps);
}

@Override
public <E> Cursor<E> queryCursor(Statement statement) throws SQLException {
  PreparedStatement ps = (PreparedStatement) statement;
  ps.execute();
  return resultSetHandler.handleCursorResultSets(ps);
}
```

ResultSetHandler的实现为DefaultResultSetHandler，我们调用的主要是handleResultSets方法

``` java
public interface ResultSetHandler {

  <E> List<E> handleResultSets(Statement stmt) throws SQLException;

  <E> Cursor<E> handleCursorResultSets(Statement stmt) throws SQLException;

  void handleOutputParameters(CallableStatement cs) throws SQLException;

}
```

### mapper调用过程

1.  mapper的注册

    -   Configuration有个mapperRegistry属性
        -   key为接口的class，值为MapperProxyFactory
    -   SqlSessionFactory创建过程中会通过Configuration的addMapper添加接口类

2.  mapper获取

    -   获取mapper的代理：session.getMapper
        -   MapperProxyFactory会创建基于MapperProxy的代理类(实现了java的InvocationHandler接口)
        -   MapperProxy实际invoke调用的是MapperMethodInvoker

    ``` java
    protected T newInstance(MapperProxy<T> mapperProxy) {
      return (T) Proxy.newProxyInstance(mapperInterface.getClassLoader(), new Class[] { mapperInterface }, mapperProxy);
    }

    public T newInstance(SqlSession sqlSession) {
      final MapperProxy<T> mapperProxy = new MapperProxy<>(sqlSession, mapperInterface, methodCache);
      return newInstance(mapperProxy);
    }
    ```

3.  mapper调用

    -   mapper调用实际调用的是MapperProxy的invoke接口
    -   MapperProxy实际invoke调用的是MapperMethodInvoker的invoke接口
    -   MapperMethodInvoker实际调用的是MapperMethod.execute(SqlSession
        sqlSession, Object\[\] args)
    -   MapperMethod.execute实际调用的是SqlSession的对应方法，实际为DefaultSqlSession的实现

    ``` java
    if (!m.isDefault()) {
         return new PlainMethodInvoker(new MapperMethod(mapperInterface, method, sqlSession.getConfiguration()));
    }
    ```

    ``` java
    public Object execute(SqlSession sqlSession, Object[] args) {
        Object result;
        switch (command.getType()) {
          case INSERT: {
            Object param = method.convertArgsToSqlCommandParam(args);
            result = rowCountResult(sqlSession.insert(command.getName(), param));
            break;
          }
          case UPDATE: {
            Object param = method.convertArgsToSqlCommandParam(args);
            result = rowCountResult(sqlSession.update(command.getName(), param));
            break;
          }
          case DELETE: {
            Object param = method.convertArgsToSqlCommandParam(args);
            result = rowCountResult(sqlSession.delete(command.getName(), param));
            break;
          }
          case SELECT:
            if (method.returnsVoid() && method.hasResultHandler()) {
              executeWithResultHandler(sqlSession, args);
              result = null;
            } else if (method.returnsMany()) {
              result = executeForMany(sqlSession, args);
            } else if (method.returnsMap()) {
              result = executeForMap(sqlSession, args);
            } else if (method.returnsCursor()) {
              result = executeForCursor(sqlSession, args);
            } else {
              Object param = method.convertArgsToSqlCommandParam(args);
              result = sqlSession.selectOne(command.getName(), param);
              if (method.returnsOptional() && (result == null || !method.getReturnType().equals(result.getClass()))) {
                result = Optional.ofNullable(result);
              }
            }
            break;
          case FLUSH:
            result = sqlSession.flushStatements();
            break;
          default:
            throw new BindingException("Unknown execution method for: " + command.getName());
        }
        return result;
      }
    ```

### 数据源的创建

最终调用的是UnpooledDataSource.doGetConnection

``` java
private Connection doGetConnection(Properties properties) throws SQLException {
  initializeDriver();
  Connection connection = DriverManager.getConnection(url, properties);
  configureConnection(connection);
  return connection;
}
```

# mybatis-spring

## 项目创建

引入依赖

``` xml
<dependency>
  <groupId>org.springframework</groupId>
  <artifactId>spring-context</artifactId>
  <version>5.2.8.RELEASE</version> 
</dependency>
<dependency>
  <groupId>org.springframework</groupId>
  <artifactId>spring-jdbc</artifactId>
  <version>5.2.8.RELEASE</version>
</dependency>
<dependency>
  <groupId>org.mybatis</groupId>
  <artifactId>mybatis-spring</artifactId>
  <version>2.0.7</version>
</dependency>
<dependency>
  <groupId>org.mybatis</groupId>
  <artifactId>mybatis</artifactId>
  <version>3.5.13</version>
</dependency>
<dependency>
  <groupId>mysql</groupId>
  <artifactId>mysql-connector-java</artifactId>
  <version>8.0.33</version>
</dependency>
```

配置spring配置文件

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:context="http://www.springframework.org/schema/context"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:mybatis="http://mybatis.org/schema/mybatis-spring"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
    http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
    http://www.springframework.org/schema/context
    http://www.springframework.org/schema/context/spring-context-3.0.xsd
    http://mybatis.org/schema/mybatis-spring http://mybatis.org/schema/mybatis-spring.xsd">

<!--    <context:annotation-config />-->
    <context:component-scan base-package="com.onekbase" />
<!--    <mybatis:scan base-package="com.onekbase.demo.mybatis.mapper" />-->

    <bean id="dataSource" class="org.springframework.jdbc.datasource.DriverManagerDataSource">
        <property name="driverClassName" value="com.mysql.jdbc.Driver"/>
        <property name="url" value="jdbc:mysql://localhost:3306/test"/>
        <property name="username" value="root"/>
        <property name="password" value="123456"/>
    </bean>
    <bean id="sqlSessionFactory" class="org.mybatis.spring.SqlSessionFactoryBean">
        <property name="dataSource" ref="dataSource" />
        <property name="mapperLocations" value="classpath*:com/onekbase/**/mapper/*.xml" />
    </bean>
    <bean id="sqlSession" class="org.mybatis.spring.SqlSessionTemplate">
        <constructor-arg index="0" ref="sqlSessionFactory" />
    </bean>
    <bean id="demoService" class="com.onekbase.demo.mybatis.service.DemoService">
        <property name="sqlSession" ref="sqlSession"></property>
    </bean>

</beans>
```

添加配置类

``` java
@Configuration
@MapperScan("com.onekbase.demo.mybatis.mapper")
public class MybatisConfig {
}
```

添加功能类

``` java
public class DemoService {
    private SqlSession sqlSession;
    @Resource
    SysConfigMapper mapper;
    public void setSqlSession(SqlSession sqlSession) {
        this.sqlSession = sqlSession;
    }

    public void hello(){
        System.out.println("hello from demo service");
        Object o = sqlSession.selectOne("com.onekbase.demo.mybatis.mapper.SysConfigMapper.selectConfig", "key.hello");
        System.out.println(o);
        Object oo = mapper.selectConfig("key.hello");
        System.out.println(oo);
    }
}

public class SpringXmlMain {
    public static void main(String[] args) {
        ApplicationContext context = new ClassPathXmlApplicationContext("spring-ioc.xml");
        DemoService s = context.getBean("demoService", DemoService.class);
        s.hello();
    }
}
```

### SqlSessionTemplate

SqlSessionTemplate 是 MyBatis-Spring 的核心。作为 SqlSession
的一个实现，这意味着可以使用它无缝代替你代码中已经在使用的 SqlSession。
SqlSessionTemplate 是线程安全的，可以被多个 DAO 或映射器所共享使用。

``` xml
<bean id="sqlSession" class="org.mybatis.spring.SqlSessionTemplate">
  <constructor-arg index="0" ref="sqlSessionFactory" />
</bean>
```

``` java
@Configuration
public class MyBatisConfig {
  @Bean
  public SqlSessionTemplate sqlSession() throws Exception {
    return new SqlSessionTemplate(sqlSessionFactory());
  }
}
```

## mybatis-spring源码分析

### SqlSessionFactory

SqlSessionFactoryBean实现了InitializingBean接口，在bean初始化时会调用afterPropertiesSet

-   afterPropertiesSet
    -   buildSqlSessionFactory
        -   创建配置Configuration
        -   XMLMapperBuilder.parse : 有xml配置则加载，可以多个
        -   sqlSessionFactoryBuilder.build :
            用Configuration构建SqlSessionFactory

### SqlSessionTemplate

SqlSessionTemplate中有一个SqlSession的代理实现sqlSessionProxy

代理类为SqlSessionInterceptor，所有的session调用都会由代理实现

最关键的是使用SqlSessionUtils.getSqlSession获取真正的SqlSession

-   TransactionSynchronizationManager获取SqlSessionHolder
-   SqlSessionHolder获取SqlSession
-   没有就sessionFactory.openSession一个
-   TransactionSynchronizationManager注册SqlSessionHolder

TransactionSynchronizationManager为spring的实现类，使用ThreadLocal保证线程变量的安全

### mapper映射

知识点：

-   NamespaceHandlerSupport是spring配置文件的空间解析抽象
-   BeanDefinitionRegistryPostProcessor是spring的注册拦截器

mybatis:scan

``` xml
<mybatis:scan base-package="com.onekbase.demo.mybatis.mapper" />
```

通过实现spring的NamespaceHandlerSupport，注册了一个scan元素和一个解析器

``` java
public class NamespaceHandler extends NamespaceHandlerSupport {

  @Override
  public void init() {
    registerBeanDefinitionParser("scan", new MapperScannerBeanDefinitionParser());
  }
```

注解MapperScan

``` java
@Configuration
@MapperScan("com.onekbase.demo.mybatis.mapper")
public class MybatisConfig {
}
```

MapperScannerRegistrar最终注入了一个MapperScannerConfigurer

MapperScannerConfigurer

-   实现了BeanDefinitionRegistryPostProcessor
-   会在postProcessBeanDefinitionRegistry注册mapper的BeanDefinition
-   创建ClassPathMapperScanner，实现了spring的ClassPathBeanDefinitionScanner
-   扫描到mapper创建BeanDefinition后，mapper的工厂类设置为MapperFactoryBean
-   MapperFactoryBean经过多层包装，最后还是调用的MapperProxyFactory创建MapperProxy
-   因为注入的SqlSessionTemplate为线程安全的，所以mapper代理也为安全的

``` java
// MapperFactoryBean
public T getObject() throws Exception {
    return getSqlSession().getMapper(this.mapperInterface);
}

//SqlSessionTemplate
public <T> T getMapper(Class<T> type) {
    return getConfiguration().getMapper(type, this);
}

// Configuration
public <T> T getMapper(Class<T> type, SqlSession sqlSession) {
    return mapperRegistry.getMapper(type, sqlSession);
}

// MapperRegistry
public <T> T getMapper(Class<T> type, SqlSession sqlSession) {
    final MapperProxyFactory<T> mapperProxyFactory = (MapperProxyFactory<T>) knownMappers.get(type);
    if (mapperProxyFactory == null) {
        throw new BindingException("Type " + type + " is not known to the MapperRegistry.");
    }
    try {
        return mapperProxyFactory.newInstance(sqlSession);
    } catch (Exception e) {
        throw new BindingException("Error getting mapper instance. Cause: " + e, e);
    }
}
```

# mybatis-springboot [mybatis-springboot]

## 项目引入

引入依赖

``` xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter</artifactId>
</dependency>
<dependency>
  <groupId>org.mybatis.spring.boot</groupId>
  <artifactId>mybatis-spring-boot-starter</artifactId>
  <version>2.1.0</version>
</dependency>
<dependency>
  <groupId>mysql</groupId>
  <artifactId>mysql-connector-java</artifactId>
  <version>8.0.33</version>
</dependency>
```

资源文件

``` xml
<resources>
  <resource>
    <directory>src/main/java/</directory>
    <includes>
      <include>**/*.xml</include>
    </includes>
  </resource>
  <resource>
    <directory>src/main/resources/</directory>
    <includes>
      <include>**/*.xml</include>
      <include>**/*.yml</include>
    </includes>
  </resource>
</resources>
```

application.yml

    spring:
      datasource:
        # type: com.alibaba.druid.pool.DruidDataSource
        driver-class-name: com.mysql.cj.jdbc.Driver
        url: jdbc:mysql://localhost:3306/test
        username: root
        password: 123456

    mybatis:
      configuration:
        log-impl: org.apache.ibatis.logging.stdout.StdOutImpl
    #  configLocation: mybatis-config.xml

mybatis配置类

``` java
@Configuration
@MapperScan("com.onekbase.demo.mybatis.mapper")
public class MybatisConfig {
    @Bean
    ConfigurationCustomizer mybatisConfigurationCustomizer() {
        return configuration -> {
        };
    }

    @Bean
    ExamplePlugin examplePlugin(){
        return new ExamplePlugin();
    }

    @Bean
    ExamplePlugin2 examplePlugin2(){
        return new ExamplePlugin2();
    }

```

启动器，其他文件相同

``` java
@SpringBootApplication
public class MybatisApplication {
    public static void main(String[] args) {
        ConfigurableApplicationContext cxt = SpringApplication.run(MybatisApplication.class, args);
        DemoService s = cxt.getBean("demoService", DemoService.class);
        s.hello();
    }
}
```

## 源码解析

starter的自动注入：org.mybatis.spring.boot.autoconfigure.MybatisAutoConfiguration

# mybatis 多数据源

## 样例

添加两个数据源配置

mysql.properties

    jdbc.mysql.driver=com.mysql.cj.jdbc.Driver
    jdbc.mysql.url=jdbc:mysql://localhost:3306/test
    jdbc.mysql.username=root
    jdbc.mysql.password=123456

sqlite.properties

    jdbc.sqlite.driver=org.sqlite.JDBC
    jdbc.sqlite.url=jdbc:sqlite:/Users/zihuidou/0docbase/gitee-edu/edu-orm/test.db

``` java
@Configuration
@PropertySource({"mysql.properties"})
public class MysqlConfig {

    @Value("${jdbc.mysql.driver}")
    private String driver;
    @Value("${jdbc.mysql.url}")
    private String url;
    @Value("${jdbc.mysql.username}")
    private String username;
    @Value("${jdbc.mysql.password}")
    private String password;

    @Bean
    public DataSource mysqlDataSource(){
        DriverManagerDataSource dataSource =new DriverManagerDataSource();
        dataSource.setDriverClassName(driver);
        dataSource.setUrl(url);
        dataSource.setUsername(username);
        dataSource.setPassword(password);
        return dataSource;
    }
}

@Configuration
@PropertySource({"sqlite.properties"})
public class SqliteConfig {

    @Value("${jdbc.sqlite.driver}")
    private String driver;
    @Value("${jdbc.sqlite.url}")
    private String url;

    @Bean
    public DataSource sqliteDataSource(){
        DriverManagerDataSource dataSource =new DriverManagerDataSource();
        dataSource.setDriverClassName(driver);
        dataSource.setUrl(url);
        return dataSource;
    }
}
```

添加不同的mybatis配置

``` java

@Configuration
@MapperScan(basePackages = {"com.onekbase.demo.mybatis.mapper.mysql"}
        ,sqlSessionFactoryRef = "mysqlSqlSessionFactory"
        ,sqlSessionTemplateRef = "mysqlSqlSessionTemplate"
)
public class MysqlMybatisConfig {
    @Bean
    public SqlSessionFactory mysqlSqlSessionFactory(@Qualifier("mysqlDataSource") DataSource dataSource) {
        try {
            SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
            sqlSessionFactoryBean.setDataSource(dataSource);
            return sqlSessionFactoryBean.getObject();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Bean
    public SqlSessionTemplate mysqlSqlSessionTemplate(@Qualifier("mysqlSqlSessionFactory") SqlSessionFactory sqlSessionFactory) {
        return new SqlSessionTemplate(sqlSessionFactory);
    }

}

@MapperScan(basePackages = {"com.onekbase.demo.mybatis.mapper.sqlite"}
        ,sqlSessionFactoryRef = "sqliteSqlSessionFactory"
        ,sqlSessionTemplateRef = "sqliteSqlSessionTemplate"
)
public class SqliteMybatisConfig {
    @Bean
    public SqlSessionFactory sqliteSqlSessionFactory(@Qualifier("sqliteDataSource") DataSource dataSource) {
        try {
            SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
            sqlSessionFactoryBean.setDataSource(dataSource);
            return sqlSessionFactoryBean.getObject();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Bean
    public SqlSessionTemplate sqliteSqlSessionTemplate(@Qualifier("sqliteSqlSessionFactory") SqlSessionFactory sqlSessionFactory) {
        return new SqlSessionTemplate(sqlSessionFactory);
    }
}
```

添加两个测试mapper

``` java
public interface MysqlMapper {

    @Select("select * from sys_config where ckey = #{id}")
    public SysConfig selectConfig(String id);
}

public interface SqliteMapper {

    @Select("select * from sys_config where ckey = #{id}")
    public SysConfig selectConfig(String id);
}
```

调用测试

``` java
@SpringBootApplication
public class MybatisApplication {
    public static void main(String[] args) {
        ConfigurableApplicationContext cxt = SpringApplication.run(MybatisApplication.class, args);
        MysqlMapper m1 = cxt.getBean( MysqlMapper.class);
        System.out.println(m1.selectConfig("key.hello"));
        SqliteMapper m2 = cxt.getBean(SqliteMapper.class);
        System.out.println(m2.selectConfig("key.hello"));
    }
}
```

## 原理分析

MapperScan有两个配置，可以指定生成的mapper代理使用哪个mybatis配置

``` java
String sqlSessionTemplateRef() default "";
String sqlSessionFactoryRef() default "";
Class<? extends MapperFactoryBean> factoryBean() default MapperFactoryBean.class;
```

在创建mapper的代理类时装配不同的SqlSessionTemplate
