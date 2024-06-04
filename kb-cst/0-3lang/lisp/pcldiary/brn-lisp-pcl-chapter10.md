---
title: 第10章 数字和字符串-实用Common Lisp编程笔记
date: 2017-04-08 22:00:38
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
lisp支持精确比值。

数字更接近于真正的数学数字。

common lisp整数可以任意大。

整数相除得到一个确切的比值，可以表示任意数度的分数。

common lisp支持复数。

## 字面数值

lisp读取器->lisp求值器

10、20/2、#xA读取器会转化为同一个对象。

    CL-USER> 10
    10
    CL-USER> 20/2
    10
    CL-USER> #xA
    10
    CL-USER> 

123

+123

-123

123.

2/3

-2/3

4/6->2/3

6/3->2

### 进制

\#b 二进制

\#b10101->21

\#b1010/1011->10/11

\#o 八进制

\#o777->511

\#x 十六进制

\#xDADA->56026

\#nR n进制

\#36rz-35

### 浮点数

数字总是以10进制表示，#x、#b、#o、#r只用于有理数。

1.0->1.0

1e0->1.0

1d0->1.0d0

123.0

123e0

0.123->0.123

.123->0.123

123e-3->0.123

123E-3->0.123

0.123e20->1.23e+19

123d23->1.23d+25

### 复数

\#c(实部 虚部)

### 初等数学

运算符：+ - * /

国为/不做截断处理，common lisp 提供了4种类型截断和舍入函数。

floor:向负无穷方向截断，返回小于等于实参的最大整数。

ceiling:向正无穷截断。

truncate:向零截断。

round:舍入，中间取偶。

### 数值比较

= < > <= >= /=：可以比较两个或多个值。

max min：可以取最大最小值。

## 字符

\#\跟想要的字符。

特殊字符跟名称，#\Space,#\Newline。

### 字符比较

CHAR= CHAR/= CHAR< CHAR> CHAR<= CHAR>= :多个参数，大小写相关。

CHAR-EQUAL:多个都相等，大小写无关。

## 字符串

REPL原样打印字符串，看真实内容用format。

CL-USER> "foo\"bar"
"foo\"bar"
CL-USER> (format t "foo\"bar")
foo"bar
NIL
CL-USER> 

比较

STRING= STRING/= STRING< STRING> STRING<= STRING>=

只能比较两个串，因为还有参数。

:start1 :end1 :start2 :end2

左闭右开，指定两个串的起始和结束位置。

所有序列函数都可用于字符串。