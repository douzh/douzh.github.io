---
title: lisp开发环境之安装slime
date: 2016-07-26 15:38:29
tags: 
 - Lisp
categories: 
 - 编程语言
 - Lisp
---
## 简介

Slime 的意思是“Emacs 下优秀的 Lisp 交互式开发模式”。

通过支持 Common Lisp 的交互式编程，Slime 扩展了 Emacs。所以的特性都基于 slime-mode，一个 Emacs 的 minor-mode，它为标准的 lisp-mode 提供补充。lisp-mode 为编辑 Lisp 源文件提供支持，而 slime-mode 则提供了与一个 Lisp 进程进行交互的功能，包括编译、调试、文档查找等等。

Slime 由两部分组成：用 Emacs Lisp 写的用户界面，和用 Common Lisp 写的服务器端。这两部分通过套接字连接在一起，并且使用一个类似于 RPC 的协议通信。

服务器端的 Lisp 主要是可移植的 Commom Lisp。所需要的跟特定 Lisp 实现相关的特性都由一个接口定义好，然后由不同的 Lisp 实现提供。这使得 Slime 非常容易移植。

有名的LispBox集成开发环境也只是emacs+slime+ccl+quicklisp的集成。

## 安装

你可以选择使用发行版本的 Slime 或者直接通过 CVS 仓库使用 Slime。你可以从我们的网站下载最新发布版本： http://www.common-lisp.net/project/slime/ 。

如果你已经有了一个可以从命令行启动的 Lisp 实现，那么仅需在 .emacs 文件中添加几行即可安装成功：

```
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/hacking/lisp/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)
```

这是没有其它杂项的最小化配置。

在windows环境中inferior-lisp-program要指定到sbcl.exe的全路径

注：sbcl在window环境安装后要重新启动电脑，否则会报找不到sbcl.core的错误。

## 启动 Slime

Emacs 命令 M-x slime 可以启动 Slime。它使用 inferior-lisp 包来启动一个 Lisp 进程，加载并启动 Lisp 端服务器（叫做“Swank”），然后在 Emacs 和 Lisp 之间建立一个 socket 连接。最后会生成一个 REPL 缓冲区，你可以在这里输入 Lisp 表达式并求值。

## 自动加载

基本设置始终会加载 Slime，即使你不使用它。如果你只在需要的时候才加载 Slime，那么 Emacs 会启动的快一点。要这样，你需要稍微更改你的 .emacs 文件：

```
(setq inferior-lisp-program "the path to your Lisp system")
(add-to-list 'load-path "the path of your slime directory")
(require 'slime-autoloads)
(slime-setup)
;让slime变得更好看，比如把sbcl的*变成CL-USER>
(slime-setup '(slime-fancy))
```

跟基本配置相比，差别只在这一行 (require 'slime-autoloads)。它告诉 Emacs 当 M-x slime 或者 M-x slime-connect 命令第一次执行之后 Slime 的其它部分会被自动加载。

摘选自:http://www.open-open.com/lib/view/open1400054028504.html