---
title: 第9章建立单元测试框架-实用Common Lisp编程笔记
date: 2017-03-06 21:45:14
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
测试框架设计目地是尽可能简单的增加新测试、运行多个测试套件、跟踪测试的失败。

    CL-USER> (defun test-+()
    	   (and
    	    (= (+ 1 2) 3)
    	    (= (+ 1 2 3) 6)
    	    (= (+ -1 -3) -4)))
    TEST-+
    CL-USER> (test-+)
    T
    CL-USER> 

不知道每个用例的运行情况。

## 改进

    CL-USER> (defun test-+()
    	    (format t "~:[fail~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
    	    (format t "~:[fail~;pass~] ... ~a~%"  (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    	    (format t "~:[fail~;pass~] ... ~a~%"  (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
    TEST-+
    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    NIL
    CL-USER> 

## 重构

重构去除重复

    CL-USER> (defun report-result (result form)
    	    (format t "~:[fail~;pass~] ... ~a~%" result form))
    REPORT-RESULT
    CL-USER> (defun test-+()
    	    (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
    	    (report-result  (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    	    (report-result  (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
    TEST-+
    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    NIL
    CL-USER> 

用宏再次去重

    CL-USER> (defmacro check (form)
    	   `(report-result ,form ',form))
    CHECK
    CL-USER> (defun test-+()
    	    (check (= (+ 1 2) 3))
    	    (check  (= (+ 1 2 3) 6))
    	    (check  (= (+ -1 -3) -4)))
    TEST-+
    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    NIL
    CL-USER> 

check去重

    CL-USER> (defmacro check (&body forms)
    	   `(progn
    	      ,@(loop for f in forms collect `(report-result ,f ',f))))
    CHECK
    CL-USER> (defun test-+()
    	       (check 
    	         (= (+ 1 2) 3)
    	         (= (+ 1 2 3) 6)
    	         (= (+ -1 -3) -4)))
    TEST-+
    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    NIL
    CL-USER>

## 修复返回值

默认返回值为nil，修改为全通过为t，只要有一个不通过为nil的返回形式。

第一步：每个单元测试返回测试结果

    (defun report-result (result form)
    	    (format t "~:[fail~;pass~] ... ~a~%" result form) result)

第二步：创建没有拦截的AND宏

    (defmacro combine-results (&body forms)
    	   (with-gensyms (result)
    	     `(let ((,result t))
    		,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
    		,result)))

第三步：替换progn

    (defmacro check (&body forms)
    	   `(combine-results
    	      ,@(loop for f in forms collect `(report-result ,f ',f))))

测试

    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    T
    CL-USER> (defun test-+()
    	    (check (= (+ 1 2) 3)
    	    (= (+ 1 2 3) 6)
    	     (= (+ -1 -3) -5)))
    TEST-+
    CL-USER> (test-+)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    fail ... (= (+ -1 -3) -5)
    NIL
    CL-USER> 

## 更好的结果输出

    CL-USER> (defun test-* ()
    	   (check
    	     (= (* 2 2) 4)
    	     (= (* 3 5) 15)))
    TEST-*
    CL-USER> (defun test-arithmetic ()
    	   (combine-results
    	     (test-+)
    	     (test-*)))
    TEST-ARITHMETIC
    CL-USER> (test-arithmetic)
    pass ... (= (+ 1 2) 3)
    pass ... (= (+ 1 2 3) 6)
    pass ... (= (+ -1 -3) -4)
    pass ... (= (* 2 2) 4)
    pass ... (= (* 3 5) 15)
    T
    CL-USER> 

输出测试方法名

    (defvar *test-name* nil)
    
    (defun report-result (result form)
    	    (format t "~:[fail~;pass~] ...~a: ~a~%" result *test-name* form))
    	    
    (defun test-* ()
    	   (let ((*test-name* 'test-*))
    	     (check
    	     (= (* 2 2) 4)
    	     (= (* 3 5) 15))))
    	     
    (defun test-+()
    	    (let ((*test-name* 'test-+))
    	      (check 
        	         (= (+ 1 2) 3)
        	         (= (+ 1 2 3) 6)
        	         (= (+ -1 -3) -4))))

测试

    CL-USER> (test-arithmetic)
    pass ...TEST-+: (= (+ 1 2) 3)
    pass ...TEST-+: (= (+ 1 2 3) 6)
    pass ...TEST-+: (= (+ -1 -3) -4)
    pass ...TEST-*: (= (* 2 2) 4)
    pass ...TEST-*: (= (* 3 5) 15)
    NIL

## 抽象诞生

抽象函数定义

    (defmacro deftest (name parameters &body body)
    	   `(defun ,name ,parameters
    	      (let ((*test-name* ',name))
    		,@body)))

新测试方法

    (deftest test-* ()
    	   (check
    	     (= (* 2 2) 4)
    	     (= (* 3 5) 15)))

## 测试层次体系

    (defmacro deftest (name parameters &body body)
    	   `(defun ,name ,parameters
    	      (let ((*test-name* (append *test-name* (list ',name))))
    		,@body)))
    		
    (deftest test-* ()
       (check
         (= (* 2 2) 4)
         (= (* 3 5) 15)))
        	     
    (deftest test-+()
        (check (= (+ 1 2) 3)
            (= (+ 1 2 3) 6)
            (= (+ -1 -3) -5)))
        	     
    (deftest test-arithmetic ()
       (combine-results
         (test-+)
         (test-*)))

所有用deftest定义的测试用例都会输出方法名
    	     
测试

    CL-USER> (test-arithmetic)
    pass ...(TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
    pass ...(TEST-ARITHMETIC TEST-+): (= (+ 1 2 3) 6)
    fail ...(TEST-ARITHMETIC TEST-+): (= (+ -1 -3) -5)
    pass ...(TEST-ARITHMETIC TEST-*): (= (* 2 2) 4)
    pass ...(TEST-ARITHMETIC TEST-*): (= (* 3 5) 15)
    NIL
    CL-USER> 

## 总结

    (defvar *test-name* nil)
    
    (defun report-result (result form)
        	    (format t "~:[fail~;pass~] ...~a: ~a~%" result *test-name* form))
        	    
    (defmacro deftest (name parameters &body body)
       `(defun ,name ,parameters
          (let ((*test-name* (append *test-name* (list ',name))))
    	,@body)))
    		
    (defmacro with-gensyms ((&rest names) &body body)
       `(let ,(loop for n in names collect `(,n (gensym)))
          ,@body))
    	      
    (defmacro combine-results (&body forms)
       (with-gensyms (result)
    	     `(let ((,result t))
    		,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
    		,result)))
        		
     (defmacro check (&body forms)
    	   `(combine-results
        	      ,@(loop for f in forms collect `(report-result ,f ',f))))

测试

```lisp
(deftest test-* ()
	   (check
	     (= (* 2 2) 4)
	     (= (* 3 5) 15)))
    	     
(deftest test-+()
    (check (= (+ 1 2) 3)
	    (= (+ 1 2 3) 6)
	     (= (+ -1 -3) -5)))
    	     
(deftest test-arithmetic ()
       (combine-results
         (test-+)
         (test-*)))
```

测试
	     
```lisp
CL-USER> (test-arithmetic)
pass ...(TEST-ARITHMETIC TEST-+): (= (+ 1 2) 3)
pass ...(TEST-ARITHMETIC TEST-+): (= (+ 1 2 3) 6)
fail ...(TEST-ARITHMETIC TEST-+): (= (+ -1 -3) -5)
pass ...(TEST-ARITHMETIC TEST-*): (= (* 2 2) 4)
pass ...(TEST-ARITHMETIC TEST-*): (= (* 3 5) 15)
NIL
```