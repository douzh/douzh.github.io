---
title: 第8章如何自定义宏-实用Common Lisp编程笔记
date: 2017-02-09 16:36:44
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
## DEFMACRO

格式：

    (defmacro name (parameter*)
    	   body-form*)
	   
## do-primes

素数迭代宏的实现

两个工具函数

一个数是否是素数

    (defun primep (number)
    	   (when (> number 1)
    	     (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
    
    CL-USER> (primep 17)
    T
    CL-USER> (primep 10)
    NIL
    CL-USER> 

找一下数的下一个素数

     (defun next-primep (number)
    	   (loop for n from number when (primep n) return n))
    	   
    CL-USER> (next-primep 10)
    11
    CL-USER> (next-primep 20)
    23
    CL-USER> 

do-primes定义

    (defmacro do-primes (var-and-range &rest body)
    	   (let ((var (first var-and-range))
    	     (start (second var-and-range))
    	     (end (third var-and-range)))
    	     `(do ((,var (next-primep ,start) (next-primep (+ 1 ,var))))
    		  ((> ,var ,end))
    		,@body)))
    		
测试

    CL-USER> (do-primes (p 0 19) (format t "~d " p))
    2 3 5 7 11 13 17 19 
    NIL

宏展开式

    CL-USER> (macroexpand-1 `(do-primes (p 0 19) (format t "~d " p)))
    (DO ((P (NEXT-PRIMEP 0) (NEXT-PRIMEP (+ 1 P)))) ((> P 19)) (FORMAT T "~d " P))
    T
    
slime宏展开快捷键

光标移动到源码的开括号上，C-c RET，调用slime的slime-macroexpand-1，后者调用macroexpand-1，结果输出到一缓冲区

## 堵住漏洞

    (defmacro do-primes ((var start end) &rest body)
        	     `(do ((,var (next-primep ,start) (next-primep (+ 1 ,var))))
        		  ((> ,var ,end))
        		,@body))

    CL-USER> (do-primes (p 0 19) (format t "~d " p))
    2 3 5 7 11 13 17 19 
    NIL

多次求值漏洞

    (do-primes (p 0 (random 100))
    	   (format t "~d " p))
    	   
    (DO ((P (NEXT-PRIMEP 0) (NEXT-PRIMEP (+ 1 P))))
        ((> P (RANDOM 100)))
      (FORMAT T "~d " P))

循环结束条件每次迭代会随机一次，需求应该是在第一次进入迭代时产生一次做结束值。

    (defmacro do-primes ((var start end) &rest body)
        	     `(do ((,var (next-primep ,start) (next-primep (+ 1 ,var)))
        		    (ending-value ,end))
        		    ((> ,var ending-value))
        		,@body))

新定义变量代替入参解决多次求值问题。

参数漏洞

新定义变量引入新问题，宏参会宏体同名变量互相影响。

    (do-primes (ending-value 0 10)
    	   (print ending-value))
    	   
    (DO ((ENDING-VALUE (NEXT-PRIMEP 0) (NEXT-PRIMEP (+ 1 ENDING-VALUE)))
         (ENDING-VALUE 10))
        ((> ENDING-VALUE ENDING-VALUE))
      (PRINT ENDING-VALUE))

gensym解决变量名冲突问题

    (defmacro do-primes ((var start end) &rest body)
    	   (let ((ending-value-name (gensym)))
    	     `(do ((,var (next-primep ,start) (next-primep (+ 1 ,var)))
            		    (,ending-value-name ,end))
            		    ((> ,var ,ending-value-name))
            		,@body)))

宏展开

    (DO ((P (NEXT-PRIMEP 0) (NEXT-PRIMEP (+ 1 P)))
         (#:G780 19))
        ((> P #:G780))
      (FORMAT T "~d " P))

漏洞处理总结：

除非有特殊理由，否则需要将展开式中的任何子形式放在一个位置上，使其求值顺序与宏调用的子形式相同。

除非有特殊理由，否则需要确保子形式仅被求值一次，方法是在展开式中创建变量来持有求值参数形式所得到的值，然后在展开式中所有需要用到该值的地方使用这个变量。

在宏展开期使用GENSYM来创建展开式中用到的变量名。

## 用于编写宏的宏

简化GENSYM的宏

    (defmacro with-gensyms ((&rest names) &body body)
    	   `(let ,(loop for n in names collect `(,n (gensym)))
    	      ,@body))
	      
应用

    (defmacro do-primes ((var start end) &rest body)
        	   (with-gensyms (ending-value-name)
        	     `(do ((,var (next-primep ,start) (next-primep (+ 1 ,var)))
                		    (,ending-value-name ,end))
                		    ((> ,var ,ending-value-name))
                		,@body)))