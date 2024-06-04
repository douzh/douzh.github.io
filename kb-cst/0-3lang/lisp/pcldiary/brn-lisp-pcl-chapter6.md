---
title: 第6章变量-实用Common Lisp编程笔记
date: 2017-02-09 16:34:43
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
## let

绑定的变量只能在body-form中生效。

    (let (variable*)
        body-form*)
    
    (let ((x 10) (y 20) z)
        ...)
示例：

    CL-USER> (defun foo (x)
    	   (format t "Parameter: ~a~%" x)
    	   (let ((x 2))
    	     (format t "Outer LET: ~a~%" x)
    	     (let ((x 3))
    	       (format t "Inner LET: ~a~%" x))
    	     (format t "Outer LET: ~a~%" x))
    	  (format t "Parameter: ~a~%" x)) 
    FOO
    CL-USER> (foo 1)
    Parameter: 1
    Outer LET: 2
    Inner LET: 3
    Outer LET: 2
    Parameter: 1
    NIL
    CL-USER> 

## let*

每个变量的初始值都可以引用变量列表中早先引入的变量。

    CL-USER> (let* ((x 10)
    		(y (+ x 10)))
    	   (list x y ))
    (10 20)
    CL-USER> 

## 词法变量和闭包

词法变量一般在退出绑定代码块后会被释放，但如果被匿名函数携带返回，则不会被释放。

    CL-USER> (defparameter *fn* (let ((count 0))
        #'(lambda () (setf count (1+ count)))))
    *FN*
    CL-USER> (funcall *fn*)
    1
    CL-USER> (funcall *fn*)
    2
    CL-USER> (funcall *fn*)
    3
    CL-USER> 

## 动态变量（全局变量）

全局变量命名：
defvar和defparameter

defparameter总是将值赋给变量。

defvar只有当变量未定义时才有效。

    CL-USER> (defvar *count* 10)
    *COUNT*
    CL-USER> *COUNT*
    10
    CL-USER> (defvar *count* 11)
    *COUNT*
    CL-USER> *COUNT*
    10
    CL-USER> (defparameter *count* 11)
    *COUNT*
    CL-USER> *COUNT*
    11
    CL-USER> 

函数参数和LET

示例1：

    CL-USER> (defparameter *count* 10)
    *COUNT*
    CL-USER> *COUNT*
    10
    CL-USER> (defun foo (*count*)
    	   (format t "~a~%" *count*)
    	   (setf *count* (+ 1 *count*))
    	   (format t "~a~%" *count*)
    	   (let ((*count* 100))
    	     (format t "~a~%" *count*)
    	     (setf *count* (+ 1 *count*))
    	     (format t "~a~%" *count*))
    	   (format t "~a~%" *count*))
    		  
    FOO
    CL-USER> (foo *count*)
    10
    11
    100
    101
    11
    NIL
    CL-USER> *COUNT*
    10

示例2：

    CL-USER> *COUNT*
    10
    CL-USER> (defun foo ()
    	   (format t "~a~%" *count*)
    	   (setf *count* (+ 1 *count*))
    	   (format t "~a~%" *count*)
    	   (let ((*count* 100))
    	     (format t "~a~%" *count*)
    	     (setf *count* (+ 1 *count*))
    	     (format t "~a~%" *count*))
    	   (format t "~a~%" *count*))
    		  
    FOO
    CL-USER> (foo)
    10
    11
    100
    101
    11
    NIL
    CL-USER> *COUNT*
    11
    CL-USER> 

闭包与动态变量

    CL-USER> *COUNT*
    11
    CL-USER> (defparameter *fn* (let ((*count* 0))
        #'(lambda () (setf *count* (1+ *count*)))))
    *FN*
    CL-USER> (funcall *fn*)
    12
    CL-USER> (funcall *fn*)
    13
    CL-USER>

## 常量

defconstant

## 赋值

    (setf place value)

setf是宏，可以检查所赋值place上的形式，并展开成适当的低层操作修改那个位置。当位置是变量时，它展开成对SETQ的调用。

SETF一次也可以对多个位置赋值：

    (setf x 1 y 2)

setf返回最近被赋予的值，将x和y赋予同一个随机值：

    (setf x (setf y (random 10)))

## 广义赋值

对各种结构赋值为10

    (setf x 10) 
    (setf (aref a 0) 10) ;array
    (setf (gethash 'key hash) 10) ;hash table
    (setf (field o) 10) ;slot named 'field'

## 其它修改位置的方式

    (incf x) 
    (setf x (+ x 1))
    (decf x)
    (setf x (- x 1))
    (incf x 10)
    (setf x (+ x 10))

一个例子：

    (incf (aref *array* (random (length *array*))))
    
    (let ((tmp (ramdom (length *array*))))
        (setf (aref *array* tmp) (1+ (aref *array* tmp))))
    
rotatef

    (rotatef a b)
    (let ((tmp a)) (setf a b b tmp) nil)

shiftf

    (shiftf a b 10)
    (let ((tmp a)) (setf a b b 10) tmp)


