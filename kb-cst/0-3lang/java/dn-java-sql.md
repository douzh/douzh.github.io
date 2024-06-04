# java sql

java实现数据库连接是通过厂商实现java的jdbc协议实现的，jdbc协议为java.sql包的数据模型

模型主要的类：
- DriverManager：获取Connection的管理类
  - DriverInfo：Driver接口的包装类
    - Driver：数据库驱动接口，厂商实现，主要是connect接口
- Connection：数据库连接类
  - 获取Statement
    - Statement：createStatement
    - PreparedStatement：prepareStatement
    - CallableStatement：prepareCall
  - 事务管理
    - setAutoCommit
    - commit
    - rollback 
- Statement
  - execute(String sql)：执行任何sql，可接收多个results
  - executeBatch(): 批量执行添加的sql
  - ResultSet executeQuery(String sql): 执行一个sql返回一个结果
  - int executeUpdate(String sql)：执行一个增删改sql
  - setFetchSize(int rows)
  - setQueryTimeout(int seconds) 
- PreparedStatement：预编译Statement，多了一批set参数的方法
- ResultSet：在创建时可以设置为可滚动、可更新
  - setFetchSize(int rows)
  - 位置跳转：需要设置为可滚动
    - absolute( int row )
    - first()
    - last()
    - afterLast()
    - beforeFirst()
    - relative( int rows )
    - previous()
  - get系列，获取指定列的值
  - next()：前移一行
  - update系列

## getConnection

``` java
  Class.forName("com.mysql.jdbc.Driver");
  String url = "jdbc:mysql://localhost:3306/test";
  String username = "root";
  String password = "123456";
  Connection conn = DriverManager.getConnection(url, username, password);
```

DriverManager.getConnection

从ArrayList<DriverInfo> registeredDrivers 列表里取出Driver，调用connect方法

Driver的注册

``` java
package com.mysql.cj.jdbc;

import java.sql.DriverManager;
import java.sql.SQLException;

public class Driver extends NonRegisteringDriver implements java.sql.Driver {
    public Driver() throws SQLException {
    }

    static {
        try {
            DriverManager.registerDriver(new Driver());
        } catch (SQLException var1) {
            throw new RuntimeException("Can't register driver!");
        }
    }
}
```

## 查询数据

``` java
  try(Connection conn = DbFactory.getConnection();
      Statement stmt = conn.createStatement();){
      String sql = "select * from sys_config where ckey = 'key.hello'";
      try(ResultSet rs = stmt.executeQuery(sql)) {
          while (rs.next()) {
              String ckey = rs.getString("ckey");
              String cvalue = rs.getString("cvalue");
              System.out.println("ckey:"+cvalue);
          }
      }
  } catch (Exception e) {
      e.printStackTrace();
  }
```
      
