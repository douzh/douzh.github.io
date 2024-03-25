

# msys2介绍与安装

MSYS2（Minimal SYStem 2）是一个集成了大量的GNU工具链、工具和库的开源软件包集合。它提供了一个类似于Linux的shell环境，可以在Windows系统中编译和运行许多Linux应用程序和工具。

MSYS2基于MinGW-w64平台，提供了一个完整的开发环境，包括GCC编译器、GDB调试器、Make、Git版本控制系统和许多其他开发工具。除了常用的开发库和工具之外，MSYS2还提供了许多专门针对Windows平台的库和工具，方便开发人员进行跨平台开发和移植工作。

由于MSYS2拥有比较完整的Linux工具链和库，因此它成为了许多跨平台开发和移植项目的首选工具。另外，使用MSYS2也可以轻松地在Windows系统中搭建一个类似于Linux的软件开发环境，方便开发人员进行开发和调试工作。

** MSYS2、Cygwin、MinGW-w64 区别与联系

MSYS2是一个包含MinGW-w64工具链、GNU工具集和一些开源库的平台，它提供了一种在Windows上编译和运行Unix/Linux程序的方式。MSYS2与MinGW-w64相似，但比MinGW-w64更完整和稳定，提供了Pacman包管理器以方便用户安装和管理软件包。

MinGW-w64是一个Windows下的C/C++编程工具集，它提供了运行在Windows上的GNU工具集和GCC编译器。MinGW-w64与MSYS2类似，但主要用于编译Windows本地应用程序，而非Unix/Linux程序。MinGW-w64也可以用于交叉编译，为其他平台生成Windows可执行文件。

Cygwin是一个在Windows平台上运行的兼容性层，提供了类Unix环境的工具和开发库。Cygwin将Unix程序编译为Windows本地代码，然后在Windows上运行。它提供了最完整的Linux/Unix环境，但相对于MSYS2和MinGW-w64，Cygwin的性能较差。

因此，MSYS2适用于需要在Windows上编译和运行Unix/Linux程序的场景，MinGW-w64适用于编译Windows本地应用程序的场景，Cygwin适用于需要最完整的Linux/Unix环境的场景。

## 安装

安装msys2

可以在官网下载：https://www.msys2.org/

也可以在清华镜像下载：

https://mirrors.tuna.tsinghua.edu.cn/msys2/distrib/x86_64/?C=M&O=D

在里面找最新的exe安装包即可。

安装好后按照https://mirrors.tuna.tsinghua.edu.cn/help/msys2/ 中的指导设置镜像源，这样下载包会比较快。

安装编译工具链

msys2中可用的工具链有以下几种：

https://packages.msys2.org/basegroups/mingw-w64-toolchain

若要安装gcc则执行

    pacman -S mingw-w64-x86_64-toolchain

若要安装clang则执行

    pacman -S mingw-w64-clang-x86_64-toolchain

