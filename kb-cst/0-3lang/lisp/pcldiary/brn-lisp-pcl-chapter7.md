---
title: 第7章标准控制结构-实用Common Lisp编程笔记
date: 2017-02-09 16:35:55
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
## if

    (if condtion then-form [else-form])

then-form和else-form只能是单一lisp表达式。

示例：

    CL-USER> (if (> 2 3) "then-form" "else-form")
    "else-form"
    CL-USER> (if (> 2 3) "then-form")
    NIL
    CL-USER> (if (> 3 2) "then-form" "else-form")
    "then-form"
    CL-USER>

## progn

顺序执行任意数量的宏，返回最后一个形式的值。

    (when (判断条件)
      (语句1)
      (语句2))

    CL-USER> (when T "a" "b")
    "b"
    CL-USER> (when NIL "a" "b")
    NIL

## WHEN宏

    (defmacro when (condtion &rest body)
        `(if ,condtion (progn ,@body)))

## UNLESS宏

    (defmacro when (condtion &rest body)
        `(if （not ,condtion) (progn ,@body)))
        
        

## COND宏

    (cond 
        (test-1 form*)
        ...
        (test-n form*))
    
    CL-USER> (COND (t "a" "b")
    	       (t "c" "d"))
    "b"

## AND OR NOT

    CL-USER> (not nil)
    T
    CL-USER> (not t)
    NIL
    CL-USER> (and t nil)
    NIL
    CL-USER> (or t nil)
    T

## 循环

lisp的25个操作符没有循环结构，所有循环结构都是基于TAGBODY和GO操作符上的宏。

低层是一个强大的DO宏，dolist和dotimes是基于DO宏上的简便宏。

非LISP语法的LOOP宏，类Algol语言。

## dolist

    (dolist (var list-form)
        body-form*)
        
    CL-USER> (dolist (x '(1 2 3)) (print x))
    
    1 
    2 
    3 
    NIL

中断循环

CL-USER> (dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))

1 
2 
NIL

## dotimes

    (dotimes (var count-form)
        body-form*)
        
    CL-USER> (dotimes (n 5)(print n) (prin1 (* n n)))
    
    0 0
    1 1
    2 4
    3 9
    4 16
    NIL
    
## do

    (do (variable-defination*)
        (end-test-form result-form*)
        statment*)
    
每个variable-defination的格式：

(var init-form step-form)

init-form:在循环开始时求值并赋给var，如果没有给出，则赋值为nil。

step-form:在后续迭代开始前求值并赋给var,可选，如果没有变量值不变，
可在循环体中做修改。

end-test-form:在每次迭代开始时以及所有所有循环变量指定新值后会被求值，只要值为nil，迭代继续。

result-form: 当end-test-form值为真时会被求值，最后一个结果形式的值被当作DO表达式的值返回。
在迭代的每一步里step-form将在分配任何值给变量之前被求值。这意味着可以在步长形式里引用其它循环变量。如在下列循环中：

    CL-USER> (do ((n 0 (+ 1 n))
    	      (cur 0 next)
    	      (next 1 (+ cur next)))
    	     ((= 10 n) cur))
    55

定义了三个variable-defination，n、cur、next，三个的步长(+ 1 n)、next、(+ cur next)都用旧值来求值。当所有步长被求值后变量才会赋新值。

由于可以同时推进多个循环变量，往往不需要循环体。

    CL-USER> (do ((i 0 (+ 1 i)))
    	     ((>= i 4))
    	   (print i))
    
    0 
    1 
    2 
    3 
    NIL

## LOOP





