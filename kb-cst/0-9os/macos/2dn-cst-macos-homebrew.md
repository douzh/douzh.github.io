# Homebrew

## 一、什么是 Homebrew ？

Homebrew 官网有一句话：Homebrew complements macOS. （ Homebrew 使 macOS 更完整。）Homebrew 是 macOS 的套件管理工具，是高效下载软件的一种方法，相当于 Linux 下的 yum、apt-get 神器，用于下载存在依赖关系的软件包。通俗地说，Homebrew 是类似于 Mac App Store 的一个软件商店。

## 二、Homebrew 的好处

通过 Homebrew 下载的软件都来自于官网，绝对放心软件的安全性。而且它尽可能地利用系统自带的各种库，使得软件包的编译时间大大缩短，基本上不会造成冗余。

## 三、Homebrew 的安装

安装方法极其简单，使用系统终端（Terminal）应用，输入以下命令： /bin/bash -c "\$(curl -fsSL <https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh>)"

1.  测试 Homebrew 是否正确安装。

    $ brew -h

1.  若上一步输入命令，回车后提示：brew：command not found。则需要进行环境配置

## 四、切换 Homebrew 源

可能有朋友会遇到过 brew update
会卡住的情况，在国内的话可以切换为清华或中科院的镜像。

<https://mirrors.tuna.tsinghua.edu.cn/> <https://mirrors.ustc.edu.cn/>
<https://mirrors.ustc.edu.cn/help/>

相关环境变量

``` shell
export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.ustc.edu.cn/brew.git"
export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles.git"

brew update

export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git"
export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git"
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"
brew update
```

永久替换

``` shell
# 对于 bash 用户
echo 'export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.ustc.edu.cn/brew.git"' >> ~/.bash_profile
echo 'export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"' >> ~/.bash_profile
echo 'export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"' >> ~/.bash_profile

# 对于 zsh 用户
echo 'export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.ustc.edu.cn/brew.git"' >> ~/.zshrc
echo 'export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"' >> ~/.zshrc
echo 'export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"' >> ~/.zshrc
```

原始源

    # homebrew源
    https://github.com/Homebrew/brew.git
    # homebrew-core源
    https://github.com/Homebrew/homebrew-core.git
    # homebrew-cask源
    https://github.com/Homebrew/homebrew-cask.git

替换为清华的源

```shell
# 替换brew.git源
git -C "$(brew --repo)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git
# 替换 homebrew-core.git源
git -C "$(brew --repo homebrew/core)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-core.git
# 替换 homebrew-cask.git源
git -C "$(brew --repo homebrew/cask)" remote set-url origin https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/homebrew-cask.git
```

替换为中科大的源

```shell
# brew.git源
git -C "$(brew --repo)" remote set-url origin https://mirrors.ustc.edu.cn/brew.git
# homebrew-core.git源
git -C "$(brew --repo homebrew/core)" remote set-url origin https://mirrors.ustc.edu.cn/homebrew-core.git
# homebrew-cask.git源
git -C "$(brew --repo homebrew/cask)" remote set-url origin https://mirrors.ustc.edu.cn/homebrew-cask.git
# 配置homebrew-bottles
## bash用户
echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles' >> ~/.bash_profile
source ~/.bash_profile
## zsh用户
echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles' >> ~/.zshrc
source ~/.zshrc
```

还原

``` shell
# brew.git源
git -C "$(brew --repo)" remote set-url origin https://github.com/Homebrew/brew.git
# homebrew-core.git源
git -C "$(brew --repo homebrew/core)" remote set-url origin https://github.com/Homebrew/homebrew-core
# homebrew-cask.git源
git -C "$(brew --repo homebrew/cask)" remote set-url origin https://github.com/Homebrew/homebrew-cask
//最后执行brew update来尝试一下看是否速度更快。
```

个人比较推荐切换为国内源，安装包明显速度快很多。

以 USTC（中科院）镜像为例：

1.  替换 Homebrew 源

    $ cd "\$(brew –repo)" \$ git remote set-url origin <https://mirrors.ustc.edu.cn/brew.git>

1.  切换 Homebrew Core 源

    $ cd "\$(brew –repo)/Library/Taps/homebrew/homebrew-core" 
    $ git remote set-url origin <https://mirrors.ustc.edu.cn/homebrew-core.git>

1.  切换 Homebrew Cask 源

    cd "\$(brew –repo)"/Library/Taps/homebrew/homebrew-cask git remote
    set-url origin <https://mirrors.ustc.edu.cn/homebrew-cask.git>

关于更多 USTC Homebrew 镜像说明，请看 Homebrew 源使用帮助。
<http://mirrors.ustc.edu.cn/help/brew.git.html>

## 五、Homebrew 安装/卸载命令

其实细心的朋友可能会发现，Homebrew 的安装命令好像有两个：

    $ brew install \<package\> 
    $ brew install –cask \<package\>

这两者有什么区别呢？

官方描述：Homebrew Cask 扩展了 Homebrew，并为 Atom 和 Google Chrome 等 GUI macOS 应用程序的安装和管理带来了优雅、简单和快速。 为此，我们提供了友好的 CLI 工作流来管理作为二进制文件分发的 macOS 应用程序。

我们执行如下搜索命令，会发现：

```
$ brew search google

==> Formulae
aws-google-auth                          google-sparsehash
google-authenticator-libpam              google-sql-tool
google-benchmark                         googler
google-go                                googletest
google-java-format

==> Casks
google-ads-editor
google-analytics-opt-out
google-backup-and-sync
...
```

以上搜索命令，可以看到搜索关键词 google，结果会出现 Formulae 和 Casks 两种分类，有何区别？

「Formulae」一般是那些命令行工具、开发库、字体、插件等不含 GUI 界面的软件。

「Cask」是指那些含有 GUI 图形化界面的软件，如 Google Chrome、FireFox 、Atom 等。

其实所有的 Homebrew Cask 命令都以 brew 开头，这对 Casks 和 Formulae 均适用。

所以，使用 Homebrew 安装软件，只要使用如下命令即可：

    $ brew install \<package\>

其他一些命令：

    $ brew uninstall \<package\> # 卸载 
    $ brew reinstall \<package\> #重装

## 六、Homebrew 其他命令

1. 软件搜索命令

支持关键字、模糊搜索。假设我们想安装一个叫 Alfred 的软件，但不知道 Homebrew 是否支持安装该应用，我们可通过该方法查询。如输入 brew search alf 会列出所有符合条件的结果。

    $ brew search <key words>

2. 更新软件

获取最新的包，但该命令会先检查 Homebrew 本身是否有更新。

    $ brew update

很多朋友在这个操作，会卡在 Updating Homebrew… 按照以上方法切换至国内源几乎都能解决。除此之外，还有两种解决方法，请看文章。

3. 更新软件

如 brew upgrade highlight

    $ brew upgrade              # 更新所有
    $ brew upgrade <package>    # 更新指定软件

4. 查看 Homebrew 下载的软件存放路径

    $ brew --cache

5. 列出已安装的软件

    $ brew list             # 所有的软件，包括 Formulae  和 Cask
    $ brew list --formulae  # 所有已安装的 Formulae
    $ brew list --cask      # 所有已安装的 Casks

6. 列出可更新的软件

    $ brew outdated

7. 清理旧版本软件

如 brew cleanup wget

    $ brew cleanup            # 清理所有旧版本的包
    $ brew cleanup <package>  # 清理指定的旧版本包
    $ brew cleanup -n         # 查看可清理的旧版本包

8. 强制卸载某个软件

如 brew uninstall --force wget

    $ brew uninstall --force <package>

9. 锁定某个不想更新的软件

如 brew pin wget

    $ brew pin <package>       # 锁定指定包
    $ brew unpin <package>     # 取消锁定指定包

10. 查看已安装软件的依赖

    $ brew deps --installed --tree

11. 查看软件的信息

如 brew info wget

    $ brew info <package>     # 显示某个包信息
    $ brew info               # 显示安装的软件数量、文件数量以及占用空间
