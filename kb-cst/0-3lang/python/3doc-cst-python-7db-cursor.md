---
title: Python数据库游标
date: 2024-03-15
type: 计算机 
tags: 计算机/编程语言/python
---

# Python数据库游标

cursor 常用接口
1. description
如果 cursor 执行了查询的 sql 代码。那么读取 cursor.description 属性的时候，将返回一个列表，这个列表中装的是元组，元组中装的分别
是 (name,type_code,display_size,internal_size,precision,scale,null_ok) ，其中 name 代表的是查找出来的数据的字段名称，其他参数暂时用处不大。

2. rowcount
代表的是在执行了 sql 语句后受影响的行数。

3. close
关闭游标。关闭游标以后就再也不能使用了，否则会抛出异常。

4. execute(sql[,parameters])
执行某个 sql 语句。如果在执行 sql 语句的时候还需要传递参数，那么可以传给 parameters 参数。示例代码如下：

```
cursor.execute("select * from article where id=%s",(1,))
```

5. fetchone
在执行了查询操作以后，获取第一条数据。

6. fetchmany(size)
在执行查询操作以后，获取多条数据。具体是多少条要看传的 size 参数。如果不传 size 参数，那么默认是获取第一条数据。

7. fetchall
获取所有满足 sql 语句的数据。
