---
title: Python安装
date: 2024-03-15
type: 计算机 
tags: 计算机/编程语言/python
---
# Mac同时安装Python2和Python3

## 背景：

最新版Mac（macOS Catalina 10.15.5）默认安装的是Python2.7.16.

Python有两个发行版，一个是Python2，一个是Python3。有很多老的软件使用的是Python2，而有很多新的软件使用的是Python3，所以最好是同时安装两个版本，使用起来比较方便。

不同的安装方式，安装的目录也是不一样的(实际上就算是相同的安装方式不同的版本也会通过版本号区分开)：

| 来源             | Python安装路径                                              |
| ---------------- | ----------------------------------------------------------- |
| **系统默认自带** | `/System/Library/Frameworks/Python.framework/Versions/2.7/` |
| **brew安装**     | `/usr/local/Cellar/`                                        |
| **官网pkg安装**  | `/Library/Frameworks/Python.framework/Versions/2.7/`        |

所以可以通过修改环境变量从而切换不同的Python版本

## 安装Python3:

Python2系统默认已经有了，就不再安装了，Python3为了方便，本次采用brew安装:

安装路径在这里/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/bin/python3

```shell
brew install python3
```

## 配置环境变量

```shell
vi ~/.bash_profile 
```

输入如下内容

```shell
# Setting PATH for Python 2.7
PATH="/System/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
# Setting PATH for Python 3.7.7
PATH="/usr/local/Cellar/python@3.9/3.9.1/Frameworks/Python.framework/Versions/3.9/bin:${PATH}"
export PATH
```
```
source ~/.bash_profile 
```

## 设置别名

如果是分别使用python2或者python3作为命令，是不需要设置别名的，但是很多场景是需要使用pyton作为命令，所以需要设置一下别名进行切换

```shell
vi ~/.bashrc 
```

添加一下内容

```shell
alias python2='/System/Library/Frameworks/Python.framework/Versions/2.7/bin/python2.7'
alias python3='/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/bin/python3.7'
#通过这两行进行切换
# alias python=python3  #设置python为python3
# alias python=python2  #设置python为python2
```
```
source ~/.bashrc
```
