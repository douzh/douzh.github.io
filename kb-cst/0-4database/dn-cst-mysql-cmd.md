# mysql常用命令

## 生产问题排查

### 锁表问题

```
show processlist;

```
这时会看到有哪些线程正在执行，也可以查看锁表的线程。你会发现`alter table * add key **`那个线程状态是`Waiting for table metadata lock`，后面有个这个表的所有操作都是这个状态，很明显是这条加索引的语句把表给锁了。查看线程ID，执行

``` 
kill 线程ID
```

这样被锁住的表就能立即被使用了。

``` sql
show OPEN TABLES where In_use > 0;
```

## 元数据查询

### 查看所有数据库容量大小

``` sql
select 
table_schema as '数据库',
sum(table_rows) as '记录数',
sum(truncate(data_length/1024/1024, 2)) as '数据容量(MB)',
sum(truncate(index_length/1024/1024, 2)) as '索引容量(MB)'
from information_schema.tables
group by table_schema
order by sum(data_length) desc, sum(index_length) desc;
```

### 查看所有数据库各表容量大小

``` sql
select 
table_schema as '数据库',
table_name as '表名',
table_rows as '记录数',
truncate(data_length/1024/1024, 2) as '数据容量(MB)',
truncate(index_length/1024/1024, 2) as '索引容量(MB)'
from information_schema.tables
order by data_length desc, index_length desc;
```

### 查看指定数据库容量大小

例：查看mysql库容量大小

``` sql
select 
table_schema as '数据库',
sum(table_rows) as '记录数',
sum(truncate(data_length/1024/1024, 2)) as '数据容量(MB)',
sum(truncate(index_length/1024/1024, 2)) as '索引容量(MB)'
from information_schema.tables
where table_schema='mysql';
```

### 查看指定数据库各表容量大小

例：查看mysql库各表容量大小

``` sql
select 
table_schema as '数据库',
table_name as '表名',
table_rows as '记录数',
truncate(data_length/1024/1024, 2) as '数据容量(MB)',
truncate(index_length/1024/1024, 2) as '索引容量(MB)'
from information_schema.tables
where table_schema='mysql'
order by data_length desc, index_length desc;
```

### 列出所有表

查看所有表和表注释

``` sql
select
TABLE_NAME,
TABLE_COMMENT
from
INFORMATION_SCHEMA.Tables
where
table_schema = '某数据库名称'
```

``` sql
INFORMATION_SCHEMA.Tables:
TABLE_SCHEMA，TABLE_NAME，CREATE_TIME，UPDATE_TIME，CHECK_TIME，TABLE_COMMENT
```

### 列出所有字段

查看所有字段和字段注释

``` sql
select
COLUMN_NAME，
COLUMN_COMMENT
from
INFORMATION_SCHEMA.Columns
where
table_name = '表名'
and table_schema='数据库名'
```

``` sql
INFORMATION_SCHEMA.Columns:
TABLE_SCHEMA，TABLE_NAME，COLUMN_NAME, COLUMN_TYPE, COLUMN_KEY
```

## 常用SQL

### duplicate key update

``` sql
insert into table_a(column1,column2,...) values(v1,v2,...) on duplicate key update
column1 = v1,column2 = v2,....;
```

如果插入的记录导致一个UNIQUE索引或者primary key(主键)出现重复，那么就会认为该条记录存在，则执行update语句而不是insert语句，反之，则执行insert语句而不是更新语句。

注意点：

1.  一个唯一键，或者主键（insert into列中包含该键）
2.  不能跟where条件
3.  表中有多个唯一键时可能造成死锁，使用时注意

on duplicate key update后面跟全部更新的字段=值，也就是说insert into填写values()中的值，全部以key=value的形式填写在update后面,否则会出现不更新，或者更新某些字段的情况！！！
