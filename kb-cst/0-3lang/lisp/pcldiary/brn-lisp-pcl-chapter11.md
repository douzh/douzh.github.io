---
title: 第11章 集合-实用Common Lisp编程笔记
date: 2017-04-08 22:03:02
tags: 
 - Common Lisp
categories: 
 - 编程语言
 - Lisp
 - Common Lisp
---
## 向量

### vector

    CL-USER> (vector)
    #()
    CL-USER> (vector 1)
    #(1)
    CL-USER> (vector 1 2)
    #(1 2)
    CL-USER> #(1 2)
    #(1 2)

`#(...)`是Lisp打印器和读取器使用向量的字面形式，可以用print和read打印和恢复重向量。

可以用`#(...)`添加向量，但更建议用vector创建要修改的向量。

### make-array 

make-array可以创建任何维度的数组及定长和变长向量。

    CL-USER> (make-array 5 :initial-element nil)
    #(NIL NIL NIL NIL NIL)

创建可填充的向量

    CL-USER> (defparameter *x* (make-array 5 :fill-pointer 0))
    *X*
    CL-USER> (vector-push 'a *x*)
    0
    CL-USER> *x*
    #(A)
    CL-USER> (vector-push 'b *x*)
    1
    CL-USER> (vector-push 'c *x*)
    2
    CL-USER> (vector-push 'd *x*)
    3
    CL-USER> (vector-push 'e *x*)
    4
    CL-USER> (vector-push 'f *x*)
    NIL
    CL-USER> *x*
    #(A B C D E)
    CL-USER> (vector-pop *x*)
    E
    CL-USER> (vector-pop *x*)
    D
    CL-USER> (vector-pop *x*)
    C
    CL-USER> *x*
    #(A B)
    CL-USER> 
    
创建可变长的向量

    (make-array 5 :fill-pointer 0 :adjustable t)

## 向量的子类型

字符串`“foo"`和`#()`写成的字面向量一样，大小固定不可修改。

创建变长字符串：

    CL-USER> (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
    ""

创建位向量：

    CL-USER> (make-array 5 :fill-pointer 0 :adjustable t :element-type 'bit)
    #*

## 序列操作

    CL-USER> (defparameter *x* (vector 1 2 3))
    *X*
    CL-USER> (length *x*)
    3
    CL-USER> (elt *x* 0)
    1
    CL-USER> (elt *x* 1)
    2
    CL-USER> (elt *x* 2)
    3
    CL-USER> (elt *x* 3)
    Invoking restart: Kill this thread
    ; Evaluation aborted on #<CCL::SEQUENCE-INDEX-TYPE-ERROR #x21008241DD>.
    CL-USER> (setf (elt *x* 0) 10)
    10
    CL-USER> *x*
    #(10 2 3)
    CL-USER> 

## 序列迭代函数

理论上所有的序列操作都可以归纳于length、elt、setf操作。

序列函数允许不用显示遍历列表就可以表达一定的序列操作。

    CL-USER> (count 1 #(1 2 3 4 1 2 3))
    2
    CL-USER> (remove 1 #(1 2 3 4 1 2 3))
    #(2 3 4 2 3)
    CL-USER> (remove 1 '(1 2 3 4 1 2 3))
    (2 3 4 2 3)
    CL-USER> (remove #\a "ababcdeft")
    "bbcdeft"
    CL-USER> (substitute 10 1 #(1 2 1 2))
    #(10 2 10 2)
    CL-USER> (substitute 10 1 '(1 2 1 2))
    (10 2 10 2)
    CL-USER> (substitute #\b #\a "abcabc")
    "bbcbbc"
    CL-USER> (find 3 #(1 2 3 4))
    3
    CL-USER> (find 5 #(1 2 3 4))
    NIL
    CL-USER> (position 2 #(1 1 1 2 3 ))
    3
    CL-USER> 

关键字参数：

:test 两参数函数用于比较元素和项

:key 单参数函数用于从实际序列元素中解出用于比较的关键字

:start 子序列的起始索引

:end 子序列的结束索引

:from-end 如果为真以相反顺序遍历

:count 需要移除或替换的元素个数

    CL-USER> (defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
    *V*
    CL-USER> (defun verbose-first (x) (format t "looking at ~s~%" x) (first x))
    VERBOSE-FIRST
    CL-USER> (count 'a *v* :key #'verbose-first)
    looking at (A 10)
    looking at (B 20)
    looking at (A 30)
    looking at (B 40)
    2
    CL-USER> (count 'a *v* :key #'verbose-first :from-end t)
    looking at (B 40)
    looking at (A 30)
    looking at (B 20)
    looking at (A 10)
    2
    CL-USER> 

## 高阶函数变体

对于每个序列迭代函数都有两种高阶函数变体，它们接受一个将在每个序列元素上调用的函数，以此来代替项参数。

第一类追加-IF，第二类追加-IF-NOT。

    CL-USER> (count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)
    2
    CL-USER> (count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)
    3
    CL-USER> (remove-if-not #'alpha-char-p #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))
    #("foo" "bar")
    CL-USER> (remove-duplicates #(1 2 1 2 3 1 2 3 4))
    #(1 2 3 4)
    CL-USER> 

## 排序与合并

	CL-USER> (sort #("foo" "bar" "baz") #'string<)
	\#("bar" "baz" "foo")
	CL-USER> (merge 'vector #(1 3 5) #(2 3 4) #'<)
	\#(1 2 3 3 4 5)
	CL-USER>

## 子序列操作

subseq取子序列，支持setf。

    CL-USER> (subseq "foobarbaz" 3)
    "barbaz"
    CL-USER> (subseq "foobarbaz" 3 6)
    "bar"
    CL-USER> (defparameter *x* (copy-seq "foobarbaz"))
    *X*
    CL-USER> (setf (subseq *x* 3 6) "xxx")
    "xxx"
    CL-USER> *x*
    "fooxxxbaz"
    CL-USER> (setf (subseq *x* 3 6) "abcd")
    "abcd"
    CL-USER> *x*
    "fooabcbaz"
    CL-USER> (setf (subseq *x* 3 6) "xx")
    "xx"
    CL-USER> *x*
    "fooxxcbaz"
    CL-USER> 

匹配串：

    CL-USER> (position #\b "foobarbaz")
    3
    CL-USER> (search "bar" "foobarbaz")
    3
    CL-USER> (mismatch "foobarbaz" "foom")
    3
    CL-USER> 

## 序列谓词

EVERY:每个都满足为真

SOME:有一个满足为真

NOTANY:从未满足返回真

NOTEVERY:总是满足返回假

    CL-USER> (every #'evenp #(1 2 3 4 5))
    NIL
    CL-USER> (some #'evenp #(1 2 3 4 5))
    T
    CL-USER> (notany #'evenp #(1 2 3 4 5))
    NIL
    CL-USER> (notevery #'evenp #(1 2 3 4 5))
    T
    CL-USER> (every #'> #(1 2 3 4) #(5 4 3 2))
    NIL
    CL-USER> (some #'> #(1 2 3 4) #(5 4 3 2))
    T
    CL-USER> (notany #'> #(1 2 3 4) #(5 4 3 2))
    NIL
    CL-USER> (notevery #'> #(1 2 3 4) #(5 4 3 2))
    T
    CL-USER> 

## 序列映射函数



    CL-USER> (map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
    #(10 18 24 28 30)
    CL-USER> (reduce #'+ #(1 2 3 4 5 6 7 8 9 10))
    55
    CL-USER> 

reduce可以接收的关键字参数:

:key

:from-end

:start

:end

:intial-value

## 哈希表

make-hash-table:创建哈希表

gethash:取值

setf:设置

remhash:移除

clrhash:清空

maphash:迭代


    CL-USER> (defparameter *h* (make-hash-table))
    *H*
    CL-USER> (gethash 'foo *h*)
    NIL
    NIL
    CL-USER> (setf (gethash 'foo *h*) 'quux)
    QUUX
    CL-USER> (gethash 'foo *h*)
    QUUX
    T
    CL-USER> 
    CL-USER> (maphash #'(lambda (k v) (format t "~a => ~a~%" k v )) *h*)
    FOO => QUUX
    NIL
    CL-USER> 




