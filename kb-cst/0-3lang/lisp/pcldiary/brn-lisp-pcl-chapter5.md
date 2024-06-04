---
title: 第5章函数-实用Common Lisp编程笔记
date: 2017-01-07 23:31:57
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
## 可选形参 &optional


    CL-USER> (defun foo (a b &optional c d) (list a b c d))
    FOO
    CL-USER> (foo 1 2)
    (1 2 NIL NIL)
    CL-USER> (foo 1 2 3)
    (1 2 3 NIL)
    CL-USER> (foo 1 2 3 4)
    (1 2 3 4)
    CL-USER> 


带参数的可选形参

    CL-USER> (foo 1 2)
    (1 2)
    CL-USER> (foo 1)
    (1 10)

带调用标识的可选形参

    CL-USER> (defun foo (a b &optional (c 3 c_supplied-p)) (list a b c c_supplied-p))
    FOO
    CL-USER> (foo 1 2)
    (1 2 3 NIL)
    CL-USER> (foo 1 2 3)
    (1 2 3 T)
    CL-USER> (foo 1 2 4)
    (1 2 4 T)
    CL-USER> 
    
## 剩余形参 &rest

    CL-USER> (defun add (&rest numbers) (format t "~a" numbers))
    ADD
    CL-USER> (add 1 2 3)
    (1 2 3)
    NIL

## 关键字形参 &key

    CL-USER> (defun foo (&key a b c)(list a b c))
    FOO
    CL-USER> (foo)
    (NIL NIL NIL)
    CL-USER> (foo :a 1)
    (1 NIL NIL)
    CL-USER> (foo :b 1)
    (NIL 1 NIL)
    CL-USER> (foo :c 1)
    (NIL NIL 1)
    CL-USER> (foo :a 1 :c 3)
    (1 NIL 3)
    CL-USER> (foo :c 3 :b 2 :a 1)
    (1 2 3)
    CL-USER> 

带标识的关键字参数

    CL-USER> (defun foo (&key (a 0) (b 0 b-supplied-p)(c (+ a b)))
    	   (list a b c b-supplied-p))
    FOO
    CL-USER> (foo )
    (0 0 0 NIL)
    CL-USER> (foo :a 1 :b 2)
    (1 2 3 T)
    
改变关键字

    CL-USER> (defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
    	   (list a b c c-supplied-p))
    FOO
    CL-USER> (foo :apple 10 :box 20 :charlie 30)
    (10 20 30 T)
    
    
## 组合

&optional &key

&rest &key

这两种组合会产生奇怪行为。

当&optiona不足时会吞掉&key的key和value。

&rest会收集&key的key和value。

    CL-USER> (defun foo (&rest rest &key a b c)(list rest a b c))
    FOO
    CL-USER> (foo :a 1 :b 2 :c 3)
    ((:A 1 :B 2 :C 3) 1 2 3)
    CL-USER> #'foo
    #<Compiled-function FOO #x2100910C5F>
    CL-USER> 


## return-from

## 高阶函数

### 操作符FUNCTION

用来获取函数对象，语法糖为#‘


    CL-USER> (defun foo () ())
    FOO
    CL-USER> (function foo)
    #<Compiled-function FOO #x2100910C5F>
    CL-USER> 

### 调用函数对象

funcall

    (foo 1 2 3)
    (funcall #'foo 1 2 3)

一个例子

    CL-USER> (defun plot (fn min max step)
    	   (loop for i from min to max by step do
    		(loop repeat (funcall fn i) do (format t "*"))
    		(format t "~%")))
    PLOT
    CL-USER> (plot #'exp 0 4 1/2)
    *
    *
    **
    ****
    *******
    ************
    ********************
    *********************************
    ******************************************************
    NIL
    CL-USER> 

apply

接收一个列表为函数参数

    CL-USER> (defun foo (a b c) (list a b c))
    FOO
    CL-USER> (apply #'foo '(1 2 3))
    (1 2 3)
    CL-USER> (apply #'foo 1 2 3 ())
    (1 2 3)
    CL-USER> (apply #'foo 1 2 '(3))
    (1 2 3)

## 匿名函数

CL-USER> (plot #'(lambda (x) (* 2 x)) 0 10 1)

    **
    ****
    ******
    ********
    **********
    ************
    **************
    ****************
    ******************
    ********************
    NIL
    CL-USER> 
