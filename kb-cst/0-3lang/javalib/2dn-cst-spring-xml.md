
# spring xml定义 

xml可以定义spring初始化Bean的配置，具体语法可以看对应的xsd文件

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:context="http://www.springframework.org/schema/context"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
    http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
    http://www.springframework.org/schema/context
    http://www.springframework.org/schema/context/spring-context-3.0.xsd">

    ....

</beans>
```

## bean配置示例

``` xml
<!--配置User对象创建-->
<bean id="user" class="com.spring5.User"></bean>

<!--set方法注入属性-->
<bean id="book" class="com.spring5.Book">
  <property name="bname" value="WeiSanJin"></property>
  <property name="bauthor" value="WeiSanJin"></property>
</bean>

<bean id="orders" class="com.spring5.Orders">
  <constructor-arg name="oname" value="WeiSanJin"></constructor-arg>
  <constructor-arg name="address" value="WeiSanJin"></constructor-arg>
  <!--可以使用 index代替name-->
</bean>

<bean id="userService" class="com.spring5.service.UserService"> 
  <property name="userDao" ref="userDaoImpl"></property>
</bean>
// 配置dao对象
<bean id="userDaoImpl" class="com.spring5.dao.UserDaoImpl"></bean> 

<!--可以改用外部bean的方式来写 其它bean对象就不能调用。-->
<bean id="emp" class="com.spring5.bean.Emp">
  <property name="ename" value="WeiSanJin"></property>
  <property name="genfer" value="WeiSanJin"></property>
  <property name="dept">
    <bean id="dept" class="com.spring5.bean.Dept">
      <property name="dname" value="保安部"></property>
    </bean>
  </property>
</bean> 
```

## 转义字符

``` xml
//方法一：转义字符
<property name="address" value="&lt;北京&dt;"></property>
//方法二：CDATA
<property name="address">
    <value>
    <![CDATA[<北京>]]>
    </value>
</property>
```

## 集合类属性注入

``` xml
<bean id="stu" class="com.spring5.collectionytpe.Stu">
        <!--数组类型属性注入 -->
        <property name="courses">
            <array>
                <value>Java课程</value>
                <value>数据库课程</value>
            </array>
        </property>
        <!--list类型属性注入 -->
        <property name="list">
            <list>
                <value>张三</value>
                <value>小三</value>
            </list>
            <list>
                <ref bean="course1"></ref>
                <ref bean="course2"></ref>
            </list>
        </property>
        <!--map类型属性注入 -->
        <property name="maps">
            <map>
                <entry key="Java" value="java"></entry>
                <entry key="PHP" value="php"></entry>
            </map>
        </property>
        <!--set类型属性注入 -->
        <property name="sets">
            <set>
                <value>Mysql</value>
                <value>Redis</value>
            </set>
        </property>
    </bean>

<!-- 创建多个course对象-->
    <bean id="course1" class="com.spring5.collectionytpe.Course">
        <property name="cname" value="String"></property>
    </bean>
    <bean id="course2" class="com.spring5.collectionytpe.Course">
        <property name="cname" value="String"></property>
    </bean>
```

## util命名空间

beans只能存在bean对象，不能有list等集合标签。而util相当于将bean中的集合属性property抽出来单独当做一个bean对象操作，通过它就可以生成集合对象供其它bean引用。

spring配置文件中引入名称空间util

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:util="http://www.springframework.org/schema/util"
       xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
                           http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util.xsd">
```

``` xml
<util:list id="bookList">
     <value>三国演义</value>
     <value>水浒传</value>
     <value>西游记</value>
     <value>红楼梦</value>
</util:list>

<bean id="book" class="com.spring5.collectionytpe.Book">
     <property name="list" ref="bookList"></property>
</bean>
```

## xml自动装配

``` xml
<!--实现自动装配
    bean标签属性autowire，配置自动装配
    autowire属性常用两个值：
        byName根据属性名称注入，注入值bean的id值和类属性名称一样
        byType根据属性类型注入
-->
    <bean id="emp" class="com.spring5.autowire.Emp" autowire="byName">
    </bean>
    <bean id="dept" class="com.spring5.autowire.Dept"></bean>
```

## 外部属性文件

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:context="http://www.springframework.org/schema/context"
       xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
                           http://www.springframework.org/schema/util http://www.springframework.org/schema/util/spring-util.xsd
                           http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context.xsd">

<!--    引入外部属性文件-->
<context:property-placeholder location="classpath:jdbc.properties"></context:property-placeholder>

<!--    配置连接池-->
    <bean id="dataSource" class="com.alibaba.druid.pool.DruidDataSource">
        <property name="driverClassName" value="${prop.driverClass}"></property>
        <property name="url" value="${prop.url}"></property>
        <property name="username" value="${prop.userName}"></property>
        <property name="password" value="${prop.password}"></property>
    </bean>
```

## 扫描注解

``` xml
<context:component-scan base-package="com.zhh.service,com.zhh.DAO"></context:component-sacn>

<context:component-scan base-package="com.spring5"/>

    <!--示例1
      use-default-filters="false" 表示现在不使用默认filter，自己配置fillter
      context:include-filter,设置扫描哪些内容
  -->
  <context:component-scan base-package="com.spring5" use-default-filters="false">
      <context:include-filter type="annotation" expression="org.springframework.stereotype.Controller"/>
  </context:component-scan>

  <!--示例2
      下面配置扫描包所有内容
      context:exclude-filter：设置哪些内容不进行扫描
  -->
  <context:component-scan base-package="com.spring5">
      <context:exclude-filter type="annotation" expression="org.springframework.stereotype.Controller"/>
  </context:component-scan>
```
