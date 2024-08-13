# nginx 

-   [nginx官网](http://nginx.org/en/index.html)
-   [Nginx w3cshool](https://www.w3cschool.cn/nginx/)

## 简介

Nginx 是一款轻量级的 Web 服务器/反向代理服务器及电子邮件（IMAP/POP3）代理服务器，其特点是占有内存少，并发能力强。

Nginx 由内核和模块组成，其中，内核的设计非常微小和简洁，完成的工作也非常简单，仅仅通过配置文件将客户端请求映射到一个 location block（location 是 Nginx配置中的一个指令，用于 URL 匹配），而在这个 location 中所配置的每个指令将会启动不同的模块去完成相应的工作。

Nginx 相对于 Apache 优点：

1) 高并发响应性能非常好，官方 Nginx 处理静态文件并发 5w/s
2) 反向代理性能非常强。（可用于负载均衡）
3) 内存和 cpu 占用率低。（为 Apache 的 1/5-1/10）
4) 对后端服务有健康检查功能。
5) 支持 PHP cgi 方式和 fastcgi 方式。
6) 配置代码简洁且容易上手。

2. Nginx 工作原理及安装配置

Nginx 由内核和模块组成，其中，内核的设计非常微小和简洁，完成的工作也非常简单，仅仅通过查找配置文件将客户端请求映射到一个 location block（location 是 Nginx配置中的一个指令，用于 URL 匹配），而在这个 location 中所配置的每个指令将会启动不同的模块去完成相应的工作。
Nginx 的模块从结构上分为

核心模块、基础模块和第三方模块：

- 核心模块：HTTP 模块、 EVENT 模块和 MAIL 模块
- 基础模块： HTTP Access 模块、HTTP FastCGI 模块、HTTP Proxy 模块和 HTTP Rewrite模块，
- 第三方模块：HTTP Upstream Request Hash 模块、 Notice 模块和 HTTP Access Key模块。

Nginx 的高并发得益于其采用了 epoll 模型，与传统的服务器程序架构不同，epoll 是linux 内核 2.6 以后才出现的。 Nginx 采用 epoll 模型，异步非阻塞，而 Apache 采用的是select 模型

Select 特点：select 选择句柄的时候，是遍历所有句柄，也就是说句柄有事件响应时，select 需要遍历所有句柄才能获取到哪些句柄有事件通知，因此效率是非常低。

epoll 的特点：epoll 对于句柄事件的选择不是遍历的，是事件响应的，就是句柄上事件来就马上选择出来，不需要遍历整个句柄链表，因此效率非常高

## 安装

### mac

    brew search nginx // 查询要安装的软件是否存在
    brew info nginx
    brew install nginx

-   Docroot默认为/usr/local/var/www
-   在/usr/local/etc/nginx/nginx.conf配置文件中默认端口被配置为8080，从而使nginx运行时不需要加sudo
-   nginx将在/usr/local/etc/nginx/servers/目录中加载所有文件
-   以及我们可以通过最简单的命令 ‘nginx’ 来启动nginx

注意：在/usr/local/etc/nginx/中没有servers目录，这个目录可以按照需要手动建立。

打开浏览器访问localhost:80

## nginx常用命令

-   启动命令： nginx
-   快速停止命令 nginx -s stop
-   平稳退出命令：nginx -s quit
-   重新加载配置文件命令：nginx -s reload
-   检查配置文件正确性：nginx -t

## 配置文件
```
    #nginx进程数，建议设置为等于CPU总核心数。
    worker_processes  1;
    # 事件区块开始
    events {
        #单个进程最大连接数（最大连接数=连接数*进程数）
        #根据硬件调整，和前面工作进程配合起来用，尽量大，但是别把cpu跑到100%就行。每个进程允许的最多连接数，理论上每台nginx服务器的最大连接数为。
        worker_connections  1024;
    }
    #设定http服务器，利用它的反向代理功能提供负载均衡支持
    http {
        #include：导入外部文件mime.types，将所有types提取为文件，然后导入到nginx配置文件中
        include       mime.types;
         #默认文件类型
        default_type  application/octet-stream;
        #开启高效文件传输模式，sendfile指令指定nginx是否调用sendfile函数来输出文件，对于普通应用设为 on，如果用来进行下载等应用磁盘IO重负载应用，可设置为off，以平衡磁盘与网络I/O处理速度，降低系统的负载。注意：如果图片显示不正常把这个改成off。
        #sendfile指令指定 nginx 是否调用sendfile 函数（zero copy 方式）来输出文件，对于普通应用，必须设为on。如果用来进行下载等应用磁盘IO重负载应用，可设置为off，以平衡磁盘与网络IO处理速度，降低系统uptime。
        sendfile        on;
          #长连接超时时间，单位是秒
        keepalive_timeout  65;
        # 第一个Server区块开始，表示一个独立的虚拟主机站点
        server {
            # 提供服务的端口，默认80
            listen       80;
            # 提供服务的域名主机名
            server_name  localhost;
            #对 "/" 启用反向代理,第一个location区块开始
            location / {
                root   html;  #服务默认启动目录
                index  index.html index.htm; # 默认的首页文件，多个用空格分开
            }
             # 错误页面路由
            error_page   500 502 503 504  /50x.html; # 出现对应的http状态码时，使用50x.html回应客户
            location = /50x.html { # location区块开始，访问50x.html
                root   html;  # 指定对应的站点目录为html
            }
        }
    }
```
-   nginx.conf
    -   worker~processes~
    -   events
        -   worker~connections~
    -   http
        -   include
        -   default~type~
        -   sendfile
        -   keepalive~timeout~
        -   server
            -   listen
            -   server~name~
            -   location
                -   root 静态文件 直接拼接 root + location
                -   alias 静态文件 alias 替换 location
                -   proxy~pass~ 代理服务
            -   error~page~

-   http全局块 http全局块配置的指令包括文件引入、MIME-TYPE 定义、日志自定义、连接超时时间、单链接请求数上限等。
-   server 块 这块和虚拟主机有密切关系，虚拟主机从用户角度看，和一台独立的硬件主机是完全一样的，该技术的产生是为了 节省互联网服务器硬件成本。 每个 http 块可以包括多个 server 块，而每个 server 块就相当于一个虚拟主机。 而每个 server 块也分为全局 server 块，以及可以同时包含多个 locaton 块。
-   全局 server 块 最常见的配置是本虚拟机主机的监听配置和本虚拟主机的名称或IP配置。
-   location 块 一个 server 块可以配置多个 location 块。 这块的主要作用是基于 Nginx 服务器接收到的请求字符串（例如 server~name~/uri-string），对虚拟主机名称 （也可以是IP 别名）之外的字符串（例如 前面的 /uri-string）进行匹配， 对特定的请求进行处理。 地址定向、数据缓 存和应答控制等功能，还有许多第三方模块的配置也在这里进行。

前端history模式404问题
```
location / { 
    try~files~ \$uri \$uri/ /index.html; 
    }
```
这段代码的作用是，当用户刷新页面时，Nginx会先检查当前URL是否存在，如果不存在，就会尝试访问index.html，从而可以正常显示页面。

## location

location是Nginx中的块级指令(block directive),，location指令的功能是用来匹配不同的url请求，进而对请求做不同的处理和响应，这其中较难理解的是多个location的匹配顺序，本文会作为重点来解释和说明。

开始之前先明确一些约定，我们输入的网址叫做请求URI，nginx用请求URI与location中配置的URI做匹配。

localtion 语法

location有两种匹配规则：

-   匹配URL类型，有四种参数可选，当然也可以不带参数。

    location \[ = \| \~ \| `* | ^` \] uri { … }

-   命名location，用@标识，类似于定于goto语句块。

    location @name { … }

location匹配顺序: = \> \^\~ \> \~ \| \~\* \> 最长前缀匹配 \> /

语法规则：

-   = 开头表示精确匹配
-   \^\~ 开头表示uri以某个常规字符串开头，理解为匹配url路径即可(非正则)
-   \~ 开头表示区分大小写的正则匹配
-   \~\* 开头表示不区分大小写的正则匹配
-   !\~和!\~\*分别为区分大小写不匹配及不区分大小写不匹配的正则
-   / 通用匹配，任何请求都会匹配到

1.  “=” ，精确匹配 内容要同表达式完全一致才匹配成功

```
    location = /abc/ {
      .....
     }

    # 只匹配http://abc.com/abc
    #http://abc.com/abc [匹配成功]
    #http://abc.com/abc/index [匹配失败]
```
2.  “\~”，执行正则匹配，区分大小写。

```
    location ~ /Abc/ {
      .....
    }
    #http://abc.com/Abc/ [匹配成功]
    #http://abc.com/abc/ [匹配失败]
```
3.  “\~\*”，执行正则匹配，忽略大小写


```
    location ~* /Abc/ {
      .....
    }
    # 则会忽略 uri 部分的大小写
    #http://abc.com/Abc/ [匹配成功]
    #http://abc.com/abc/ [匹配成功]
```
4.  “\^\~”，表示普通字符串匹配上以后不再进行正则匹配。

```
    location ^~ /index/ {
      .....
    }
    #以 /index/ 开头的请求，都会匹配上
    #http://abc.com/index/index.page  [匹配成功]
    #http://abc.com/error/error.page [匹配失败]
```
5.  不加任何规则时，默认是大小写敏感，前缀匹配，相当于加了“\~”与“\^\~”

```
    location /index/ {
      ......
    }
    #http://abc.com/index  [匹配成功]
    #http://abc.com/index/index.page  [匹配成功]
    #http://abc.com/test/index  [匹配失败]
    #http://abc.com/Index  [匹配失败]
    # 匹配到所有uri
```
6.  “@”，nginx内部跳转

```
    location /index/ {
      error_page 404 @index_error;
    }
    location @index_error {
      .....
    }
    #以 /index/ 开头的请求，如果链接的状态为 404。则会匹配到 @index_error 这条规则上。
```
### root 与 alias 的区别
```
    # 当请求 /i/top.gif，/data/w3/i/top.gif 会被返回。
    location /i/ {
        root /data/w3;
    }

    # 当请求 /i/top.gif，/data/w3/images/top.gif 会被返回。
    location /i/ {
        alias /data/w3/images/;
    }
```
### 通过”/” 实现通用匹配

第一种情况：
- location /bbb/
- proxy_pass http://ip:port
- 浏览器的请求: http://ip:port/bbb/
- 实际访问地址: http://ip:port/bbb/
- 结论：会将匹配路径/bbb一起加过去

第二种情况：
- location /bbb/
- proxy_pass http://ip:port/
- 浏览器的请求: http://ip:port/bbb/
- 实际访问地址: http://ip:port
- 结论会将/bbb抛弃的

第三种情况
- location /bbb/
- proxy_pass http://ip:port/ccc
- 浏览器的请求: http://ip:port/bbb/index.html
- 实际访问地址: http://ip:port/cxxindex.html

第四种情况
- location /bbb/
- proxy_pass http://ip:port/ccc/
- 浏览器的请求: http://ip:port/bbb/index.html
- 实际访问地址: http://ip:port/ccc/index.html

第五种情况
- location /bbb
- proxy_pass http://ip:port
- 浏览器的请求: http://ip:port/bbb
- 实际访问地址: http://ip:port/bbb/index.html


## 注意事项

### upstream

tomcat8及以上的版本不支持upstream中带有“\_”字符

服务器会报错：The character \[\_\] is never valid in a domain name

## 代理配置

### websocket

    upstream websocketservers{
       server 127.0.0.1:8081;
       server 127.0.0.1:8082;
    } 

    server {
        listen 80;
        server_name dev-im.com;

        location / {
            proxy_pass_header Server;
            proxy_set_header Host $http_host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Scheme $scheme;
            proxy_pass http://websocketservers;
            proxy_next_upstream error;
        }
        location /websocket/ {
            proxy_pass http://websocketservers;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
        }
    }

### 代理TCP

注意：在http模块之外，和Http模块并列

    stream {
        upstream netty{
         server 127.0.0.1:8088;
         server 127.0.0.1:8089;
        }
        server {
            listen          8077;
            proxy_pass      netty;
            proxy_protocol  on;
        }
    }
