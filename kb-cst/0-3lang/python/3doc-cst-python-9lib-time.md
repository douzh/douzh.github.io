---
title: Python time库
date: 2024-03-15
type: 计算机
tags: 计算机/编程语言/python
---
# Python time库

### 1、获取秒级时间戳与毫秒级时间戳、微秒级时间戳

```python
import time
import datetime

t = time.time()

print (t)                       #原始时间数据
print (int(t))                  #秒级时间戳
print (int(round(t * 1000)))    #毫秒级时间戳
print (int(round(t * 1000000))) #微秒级时间戳
```

返回

```
1499825149.257892    #原始时间数据
1499825149           #秒级时间戳，10位
1499825149257        #毫秒级时间戳，13位
1499825149257892     #微秒级时间戳，16位
```

### 2、获取当前日期时间

```python
dt    = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
dt_ms = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f') # 含微秒的日期时间
print(dt)
print(dt_ms)
```

返回

```
2018-09-06 21:54:46
2018-09-06 21:54:46.205213
```

### 3、将日期转为秒级时间戳

```python
dt = '2018-01-01 10:40:30'
ts = int(time.mktime(time.strptime(dt, "%Y-%m-%d %H:%M:%S")))
print (ts)
```

返回

```
1514774430
```

### 4、将秒级时间戳转为日期

```python
ts = 1515774430
dt = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(ts))
print(dt)
```

返回

```
2018-01-13 00:27:10
```

### 5、时间格式转成另一种时间格式

```python
dt = '08/02/2019 01:00'
dt_new = datetime.datetime.strptime(dt, '%m/%d/%Y %H:%M').strftime('%Y-%m-%d %H:%M:%S')
print(dt_new)
```

返回

```
2019-08-02 01:00:00
```

### 6、转结构体时间struct_time

```python
ta_dt = time.strptime("2018-09-06 21:54:46", '%Y-%m-%d %H:%M:%S')  #日期时间转结构体 
ta_ms = time.localtime(1486188476) #时间戳转结构体，注意时间戳要求为int
print(ta_dt)
print(ta_ms)
```

返回

```
time.struct_time(tm_year=2018, tm_mon=9, tm_mday=6, tm_hour=21, tm_min=54, tm_sec=46, tm_wday=3, tm_yday=249, tm_isdst=-1)
time.struct_time(tm_year=2017, tm_mon=2, tm_mday=4, tm_hour=14, tm_min=7, tm_sec=56, tm_wday=5, tm_yday=35, tm_isdst=0)
```
