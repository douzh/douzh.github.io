---
title: git ssh配置
date: 2024-03-16
type: tools
tags: 计算机/编程语言/tools
---

# git ssh配置

- [Github ssh配置官方指南](https://docs.github.com/zh/authentication/connecting-to-github-with-ssh)

## SSH常用命令

### ssh-keygen

生成ssh key，可以指定生成到文件，默认生成到id_rsa和id_rsa.pub

- id_rsa（私钥)
- id_rsa.pub（公钥）

```
ssh-keygen -t rsa -C xxxxxxxxxx@xx.com
```

* 注意-C这个参数是大写的
* xxxxxx那里用你的github邮箱

然后就一路回车就好，默认会放在.ssh文件夹里

### ssh-add

如果有多个ssh key，需要将私钥添加到ssh-agent中，可以使用`ssh-add -L`查看添加的密钥

```
ssh-add ~/.ssh/repo-0_deploy_key
```

在后台启动 ssh 代理

```
$ eval "$(ssh-agent -s)"
> Agent pid 59566
```

### ssh config

如果有多个ssh key，需要在`.ssh`添加config文件，配置不同私钥的使用服务

```
Host github.com-repo-0
        Hostname github.com
        IdentityFile ~/.ssh/repo-0_deploy_key

Host github.com-repo-1
        Hostname github.com
        IdentityFile ~/.ssh/repo-1_deploy_key
```

### ssh -T

确保您自己的 SSH 密钥已设置并正常运行。

可以通过在终端输入 `ssh -T git@<域名或config的Host值>` 来测试本地密钥是否正常工作

如果不确定是否在使用本地密钥，还可以检查服务器上的 SSH_AUTH_SOCK 变量：

```
$ echo "$SSH_AUTH_SOCK"
# Print out the SSH_AUTH_SOCK variable
> /tmp/ssh-4hNGMk8AZX/agent.79453
```

如果未设置变量，则表示代理转发不起作用：

```
$ echo "$SSH_AUTH_SOCK"
# Print out the SSH_AUTH_SOCK variable
> [No output]
$ ssh -T git@github.com
# Try to SSH to github
> Permission denied (publickey).
```


## 单密钥配置

SSH-secure shell,是一个建立在应用层上的安全协议,git基于这种安全协议，用户配置完SSH KEY后，可以在提交代码的时候不需要每次都输入用户名和密码

检查本地是否已经配置了SSH KEY

```
ls -al ~/.ssh
```
windows目录是`Users/uestc/.ssh/id_rsa`如果有配过会列出

如果没有建立一个新的SSH KEY

然后查看.ssh文件下id_rsa.pub文件内容

```
cat id_rsa.pub
```
复制文件内容，登录github,依次按照如下操作处理点击用户头像,Settings,SSH and GPG keys,new SSH key,将刚才复制的内容粘贴在那儿

然后只要在clone的时候选择SSH的链接就好

## 多密钥配置

如果有多个Github账号，第个账号的公钥不能相同，需要配置多个密钥

用不同Github账号的邮箱生成SSH KEY，保存到不同的文件中，用ssh-add添加密钥

将私钥用ssh-add添加到ssh-agent中

```
ssh-add ~/.ssh/repo-0_deploy_key
ssh-add ~/.ssh/repo-1_deploy_key
```

在`.ssh`目录创建config文件，添加配置

```
Host github.com-repo-0
        Hostname github.com
        IdentityFile ~/.ssh/repo-0_deploy_key

Host github.com-repo-1
        Hostname github.com
        IdentityFile ~/.ssh/repo-1_deploy_key
```

- Host github.com-repo-0 - 存储库的别名。
- Hostname github.com - 将主机名配置为与别名一起使用。
- IdentityFile ~/.ssh/repo-0_deploy_key - 将私钥分配给别名。

然后可以使用主机名的别名通过 SSH 与仓库进行交互，SSH 将使用分配给该别名的唯一部署密钥。 例如：

```
git clone git@github.com-repo-1:OWNER/repo-1.git
```

> 注意
> 正常ssh地址是 `git@github.com:OWNER/repo-1.git`
> 使用多SSH KEY时，需要将`github.com`替换成对应的Host别名