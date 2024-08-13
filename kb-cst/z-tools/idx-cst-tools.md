
# cst tools 

-   图片
    -   snipaste 个人免费 截图贴图
-   文档
    -   pandoc 开源 文档转换
-   编辑器
    -   vim 开源
    -   emacs 开源
-   开发
    -   drawio 开源 画图
-   浏览器插件
    -   MaoXian web clipper 剪网页
    -   Markdown Viewer
    -   Feedbro 看rss

## 常用工具

### snipaste 

个人免费，支持mac和win

### pandoc 

[pandoc](id:dn-cst-tools-pandoc)

Pandoc
是一个命令行工具，用于将文件从一种标记语言转换为另一种标记语言。标记语言使用标签来标记文档的各个部分。常用的标记语言包括
Markdown、ReStructuredText、HTML、LaTex、ePub 和 Microsoft Word DOCX。

<https://pandoc.org/>

<https://zhuanlan.zhihu.com/p/49752930>

### ImageMagick 

ImageMagick是一个免费的创建、编辑、合成图片的开源软件。它可以读取、转换、写入多种格式的图片。图片切割、颜色替换、各种效果的应用，图片的旋转、组合，文本，直线，多边形，椭圆，曲线，附加到图片伸展旋转。

ImageMagick官网：http://www.imagemagick.org/

<http://www.imagemagick.com.cn/>

### GraphicsMagick 

GraphicsMagick号称图像处理领域的瑞士军刀。
短小精悍的代码却提供了一个鲁棒、高效的工具和库集合，来处理图像的读取、写入和操作，支持超过88种图像格式，包括重要的DPX、GIF、JPEG、JPEG-2000、PNG、PDF、PNM和TIFF。

GraphicsMagick官网：http://www.graphicsmagick.org/

### emacs

<https://emacsformacosx.com/>

### c-wine

<https://www.winehq.org/>

Wine （“Wine Is Not an Emulator” 的首字母缩写）是一个能够在多种
POSIX-compliant 操作系统（诸如 Linux，macOS 及 BSD 等）上运行 Windows
应用的兼容层。Wine 不是像虚拟机或者模拟器一样模仿内部的 Windows
逻辑，而是將 Windows API 调用翻译成为动态的 POSIX
调用，免除了性能和其他一些行为的内存占用，让你能够干净地集合 Windows
应用到你的桌面。

开源，可以在macos和linux上运行windows的软件

Winehq 使用homebrew安装

``` example
brew tap homebrew/cask-versions
brew install --cask --no-quarantine (selected wine package)
```

wine-stable, wine-devel or wine-staging packages can be installed using
the above example. The advantage of installing via homebrew means wine
is available from a standard terminal session The –no-quarantine line is
to avoid brew adding the quarantine flag.

Remove the source tree and binaries.

``` example
brew uninstall --cask (selected wine package)
```

首先下载并安装XQuartz支持库: brew install –cask xquartz 下载并安装wine
brew install –cask –no-quarantine wine-stable

### c-winetricks

### crossover 

wine的商业版本

### c-termux

Termux 是一个 Android 终端仿真应用程序，用于在 Android
手机上搭建一个完整的 Linux 环境。 不需要 root 权限 Termux
就可以正常运行。

从F-droid下载APK: <https://f-droid.org/packages/com.termux/>

termux-setup-storage pkg update pkg install proot-distro

在手机上安装完成后，可以使用linux部分命令，有些功能这个软件没有，如果你要编译运行c文件，你需要安装clang

pkg install clang

在安装过程中输入y等待下载完成

写好c语言文件后，就可以使用以下命令

clang hello.c

./a.out

运行c文件了

## jumpserver 开源堡垒机

jumpserver是使用python的django开发的开源跳板机系统，为互联网企业提供了认证、授权、审计和自动化运维等功能。jumpserver官网网址为：https://www.jumpserver.org/

jumpserver堡垒机由以下三个部分组成： 1、jumpserver
jumpserver是jumpserver的核心组件，是一个使用Python的django开发的管理后台，支持restful
API。 2、coco coco是SSH Server和Web Terminal
Server的组件，提供SSH和WebSocket接口，使用paramiko和flask开发。 3、luna
luna是Web Terminal
Server的前端，前端页面均由该项目提供，主要负责页面后台的渲染。
