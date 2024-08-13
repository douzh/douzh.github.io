# termux

下载termux安装包apk

<https://f-droid.org/zh_Hans/packages/com.termux/>

换源

<https://mirrors.tuna.tsinghua.edu.cn/help/termux/>

termux-change-repo

安装Linux发行版

Termux提供了一个proot-distro软件包，用于管理Termux内部的Linux发行版。据说几乎没有性能损失。

pkg install proot-distro

目前，它支持以下发行版： Alpine Linux（别名：alpine）-最小的可用发行版。
Arch Linux（别名：archlinux） Debian 10 Buster（别名：debian-buster）
Kali Nethunter（别名：nethunter）-当前只有最小的构建。 Ubuntu 18.04 /
20.04（别名：ubuntu-18.04 / ubuntu-20.04）
要安装发行版，只需运行以下命令（假设已安装proot-distro）：

proot-distro list proot-distro install \<alias\>
