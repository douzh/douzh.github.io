---
title: 第3章简单的数据库-实用Common Lisp编程笔记
date: 2017-01-07 22:54:13
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
### 属性表

    CL-USER> (getf (list :a 1 :b 2 :c 3) :a)
    1

### make-cd

     (defun make-cd (title artist rating ripped)
        (list :tilte title :artist artist :rating rating :ripped ripped))

### DEFVAR

定义全局变量

    (defvar *db* nil)

### add-record

    (defun add-record (cd) (push cd *db*))

### 录入

    CL-USER> (add-record (make-cd "roses" "kathy" 7 t))
    ((:TILTE "roses" :ARTIST "kathy" :RATING 7 :RIPPED T))
    CL-USER> (add-record (make-cd "roses2" "kathy2" 8 t))
    ((:TILTE "roses2" :ARTIST "kathy2" :RATING 8 :RIPPED T)
    (:TILTE "roses" :ARTIST "kathy" :RATING 7 :RIPPED T))
    CL-USER> (add-record (make-cd "roses3" "kathy3" 9 t))
    ((:TILTE "roses3" :ARTIST "kathy3" :RATING 9 :RIPPED T)
    (:TILTE "roses2" :ARTIST "kathy2" :RATING 8 :RIPPED T)
    (:TILTE "roses" :ARTIST "kathy" :RATING 7 :RIPPED T))
    CL-USER> 

### 查看

    CL-USER> *db*
    ((:TILTE "roses3" :ARTIST "kathy3" :RATING 9 :RIPPED T)
    (:TILTE "roses2" :ARTIST "kathy2" :RATING 8 :RIPPED T)
    (:TILTE "roses" :ARTIST "kathy" :RATING 7 :RIPPED T))
    CL-USER> 

### dump-db

    (defun dump-db()
        (dolist (cd *db*)
            (format t "~{~a: ~10t~a~%~}~%" cd)))


format

    ~a: ~10t~a~%~


处理一个键值对。

    ~{~a: ~10t~a~%~}

循环处理列表所有键值对。

    (defun dump-db ()
        (format t "~{~{~a: ~10t~a~%~}~%~}" *db*))

### format

    ~a

美化，关键字没有前导冒号，字段串没有双引号。

    ~10t

输出制表符。

    ~{~}

列表处理部分。

    ~%

换行。

### 交互读取

    (defun prompt-read (prompt)
        (format *query-io* "~a: " prompt)
        (force-output *query-io*)
        (read-line *query-io*))

*query-io*

当前终端输入流的全局变量。


    (defun prompt-for-cd () 
        (make-cd 
            (prompt-read "title")
            (prompt-read "artist")
            (prompt-read "rating")
            (prompt-read "ripped [y/n]")))
    	     
    (defun prompt-for-cd () 
        (make-cd 
            (prompt-read "title")
            (prompt-read "artist")
            (or (parse-integer (prompt-read "rating") :junk-allowed t) 0)
            (y-or-n-p "ripped [y/n]")))
    
    (defun add-cds ()
       (loop (add-record (prompt-for-cd))
          (if (not (y-or-n-p "another? [y/n]: ")) (return))))     

### 保存和加载数据库

    (defun save-db (filename)
      (with-open-file (out filename
    		       :direction :output
    		       :if-exists :supersede)
        (with-standard-io-syntax
          (print *db* out))))

:direction :output

用于写入的文件。

:if-exists :supersede

覆盖已经存在的文件。


    (defun load-db (filename)
      (with-open-file (in filename)
        (with-standard-io-syntax
          (setf *db* (read in)))))


### remove-if-not

    CL-USER> (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
    (2 4 6 8 10)


​    
​    CL-USER> (remove-if-not #'(lambda (x) (= 0 (mod x 2))) 
​    		'(1 2 3 4 5 6 7 8 9 10))
​    (2 4 6 8 10)


### 查询数据库


    (defun select (select-fn)
        (remove-if-not select-fn *db*))
      
    (defun select-by-artist (artist)
      (remove-if-not
       #'(lambda (cd) (equal (getf cd :artist) artist))
       *db*))
       
    (defun artist-selector (artist)
        #'(lambda (cd) (equal (getf cd :artist) artist)))

### where

    (defun where (&key title artist rating (ripped nil ripped-p))
      #'(lambda (cd)
          (and
           (if title (equal (getf cd :title) title) t)
           (if artist (equal (getf cd :artist) artist) t)
           (if rating (equal (getf cd :rating) rating) t)
           (if ripped-p (equal (getf cd :ripped) ripped) t))))

### update

    (defun update (selector-fn &key title artist rating (ripped nil ripped-p))
      (setf *db*
    	(mapcar
    	 #'(lambda (row)
    	     (when (funcall selector-fn row)
    	       (if title (setf (getf row :title) title))
    	       (if artist (setf (getf row :artist) artist))
    	       (if rating (setf (getf row :rating) rating))
    	       (if ripped-p (setf (getf row :ripped) ripped)))
    	     row) *db*)))

### macro

    CL-USER> (defun make-comparison-expr (field value)
    	   (list 'equal (list 'getf 'cd field) value))
    MAKE-COMPARISON-EXPR
    CL-USER> (make-comparison-expr :rating 10)
    (EQUAL (GETF CD :RATING) 10)

反引号

任何以逗号开始的子句会被求值

    CL-USER> `(1 2 (+ 1 2))
    (1 2 (+ 1 2))
    CL-USER> `(1 2 ,(+ 1 2))
    (1 2 3)
    CL-USER> 

where宏

    (defun make-comparison-expr (field value)
        `(equal (getf cd ,field) ,value))
    
    (defun make-comparisons-list (fields)
        (loop while fields
            collecting (make-comparison-expr (pop fields) (pop fields))))
    	      
    (defmacro where (&rest clauses)
        `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

### ,@

    CL-USER> `(and ,(list 1 2 3))
    (AND (1 2 3))
    CL-USER> `(and ,@(list 1 2 3))
    (AND 1 2 3)

### macroexpand-1

    CL-USER> (macroexpand-1 '(where :title "adafdsf" :ripped t))
    #'(LAMBDA (CD) (AND (EQUAL (GETF CD :TITLE) "adafdsf") (EQUAL (GETF CD :RIPPED) T)))
    T









​           