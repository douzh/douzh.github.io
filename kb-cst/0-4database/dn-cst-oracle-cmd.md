# oracle常用SQL

## sqlplus

```
sqlplus / as sysdba
```
## 数据库连接

```sql
select t.USERNAME,t.STATUS,count(0) 
from SYS.V_$SESSION t 
group by t.USERNAME,t.STATUS

select machine,USERNAME,count(*)
from v$session 
where username='OAUSER'
GROUP BY machine,USERNAME
order by USERNAME
```

杀程语句

``` sql
select 'alter system kill session '''||sid||','||serial#||''' immediate;' cmd, username ,status
from v$session 
where status = 'INACTIVE' and username='xxx'
```

## 查询库中的表名和表中记录数

```sql
SELECT t.tablespace_name, T.TABLE_NAME, T.NUM_ROWS FROM USER_TABLES T order by T.NUM_ROWS desc;
```
查询库中记录总数：

```sql
SELECT SUM(A.NUM_ROWS) FROM (SELECT T.TABLE_NAME, T.NUM_ROWS FROM USER_TABLES T) A;
```

## 用户操作

```sql
select * from dba_users;
create user test identified by 123456;
alter user test identified by 123456;
grant connect,resource to test ;
alter user test quota unlimited on users;
drop user test cascade;
```

## 用户

```sql
select * from user_users;
```

查看当前用户的角色

```sql
select * from user_role_privs;
```

查看当前用户的系统权限和表级权限

```sql
select * from user_sys_privs;
select * from user_tab_privs;
```

## 权限

```sql
-- 只有dba可以用
select * from dba_tab_privs where grantee='xxx'
SELECT * from dba_users

select grantee,privilege from dba_sys_privs where grantee in (
  select t.username from dba_users t where t.account_status='OPEN' and t.username in ('YXUSER')
);

select * from dba_tab_privs where owner in (
  select t.username from dba_users t where t.account_status='OPEN' 
  -- and owner not in ('SYS','SYSTEM','DBSNMP','SYSADM')
  and owner in ('YXUSER')
);

select pri.grantee,u.account_status,count(*) 
from dba_tab_privs pri 
LEFT JOIN dba_users u on u.username=pri.grantee
where u.account_status='OPEN'
GROUP BY pri.grantee,u.account_status
order by count(*) desc

select pri.GRANTEE ,pri.PRIVILEGE ,pri.TABLE_NAME ,u.account_status
from dba_tab_privs pri
LEFT join dba_users u ON u.username=pri.GRANTEE 
 where u.username IS NOT NULL AND u.account_status='OPEN'
 ORDER BY pri.GRANTEE 
```

## 表

查看用户下所有的表
```
select * from user_tables;
```
查看某表的创建时间
```
select * from user_objects where object_name=upper('test');
```
查看某表的大小
```
select sum(bytes)/(1024*1024) as "size(M)" from user_segments where segment_name=upper('test');

select segment_name,ceil(sum(bytes)/(1024*1024)) as sizem from user_segments -- where segment_name=upper('test');
GROUP BY segment_name
order by sizem desc
```
## 索引

查看索引个数和类别
```
select index_name,index_type,table_name from user_indexes order by table_name;
```
查看索引被索引的字段
```
select * from user_ind_columns where index_name=upper('&index_name');
```
查看索引的大小
```
select sum(bytes)/(1024*1024) as "size(M)" from user_segments where segment_name=upper('&index_name');
```
## 序列号

查看序列号，last_number是当前值
```
select * from user_sequences;
```
## 视图
```
select * from user_views;
```
## 存储函数和过程

查看函数和过程的状态
```
select object_name,status from user_objects where object_type='FUNCTION'; select object_name,status from user_objects where object_type='PROCEDURE';
```
查看函数和过程的源代码
```
select text from all_source where owner=user and name=upper('&plsql_name');
```
## SQL时间计算
```
SELECT TO_CHAR(ADD_MONTHS(SYSDATE, -1), 'yyyyMMdd') SYY FROM DUAL;  --计算上一个月
```
查看表结构
```
SELECT DBMS_METADATA.GET_DDL('TABLE','PS_C_PYMNT_ORDER_L') FROM DUAL;
```
