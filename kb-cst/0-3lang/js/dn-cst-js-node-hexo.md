# hexo 

<https://hexo.io/zh-cn/>

Hexo 是一个快速、简洁且高效的博客框架。Hexo 使用
Markdown（或其他渲染引擎）解析文章，在几秒内，即可利用靓丽的主题生成静态网页。

安装 Hexo 相当简单，只需要先安装下列应用程序即可：

Node.js (Node.js 版本需不低于 10.13，建议使用 Node.js 12.0 及以上版本)
Git

    npm install -g hexo-cli

安装以后，可以使用以下两种方式执行 Hexo：

npx hexo \<command\> 将 Hexo 所在的目录下的 node~modules~
添加到环境变量之中即可直接使用 hexo \<command\>

    echo 'PATH="$PATH:./node_modules/.bin"' >> ~/.profile

## 命令

### init

\$ hexo init \[folder\]

新建一个网站。如果没有设置 folder ，Hexo 默认在目前的文件夹建立网站。

本命令相当于执行了以下几步：

Git clone hexo-starter 和 hexo-theme-landscape
主题到当前目录或指定目录。 使用 Yarn 1、pnpm 或 npm
包管理器下载依赖（如有已安装多个，则列在前面的优先）。npm 默认随 Node.js
安装。

### new

\$ hexo new \[layout\] \<title\>

新建一篇文章。如果没有设置 layout 的话，默认使用 ~config~.yml 中的
default~layout~ 参数代替。如果标题包含空格的话，请使用引号括起来。

\$ hexo new "post title with whitespace"

参数 描述 -p, –path 自定义新文章的路径 -r, –replace
如果存在同名文章，将其替换 -s, –slug 文章的
Slug，作为新文章的文件名和发布后的 URL

默认情况下，Hexo
会使用文章的标题来决定文章文件的路径。对于独立页面来说，Hexo
会创建一个以标题为名字的目录，并在目录中放置一个 index.md
文件。你可以使用 –path 参数来覆盖上述行为、自行决定文件的目录：

hexo new page –path about/me "About me"

以上命令会创建一个 source/about/me.md 文件，同时 Front Matter 中的 title
为 "About me"

注意！title 是必须指定的！如果你这么做并不能达到你的目的：

hexo new page –path about/me

此时 Hexo 会创建 source/~posts~/about/me.md，同时 me.md 的 Front Matter
中的 title 为 "page"。这是因为在上述命令中，hexo-cli 将 page
视为指定文章的标题、并采用默认的 layout。

### generate

\$ hexo generate

生成静态文件。 选项 描述 -d, –deploy 文件生成后立即部署网站 -w, –watch
监视文件变动 -b, –bail 生成过程中如果发生任何未处理的异常则抛出异常 -f,
–force 强制重新生成文件 Hexo 引入了差分机制，如果 public 目录存在，那么
hexo g 只会重新生成改动的文件。 使用该参数的效果接近 hexo clean && hexo
generate -c, –concurrency 最大同时生成文件的数量，默认无限制

该命令可以简写为

\$ hexo g

### publish

\$ hexo publish \[layout\] \<filename\>

发表草稿。

### server

\$ hexo server

启动服务器。默认情况下，访问网址为： <http://localhost:4000/>。 选项
描述 -p, –port 重设端口 -s, –static 只使用静态文件 -l, –log
启动日记记录，使用覆盖记录格式

### deploy

\$ hexo deploy

部署网站。 参数 描述 -g, –generate 部署之前预先生成静态文件

该命令可以简写为：

\$ hexo d

### render

\$ hexo render \<file1\> \[file2\] …

渲染文件。 参数 描述 -o, –output 设置输出路径

### migrate

\$ hexo migrate \<type\>

从其他博客系统 迁移内容。 clean

\$ hexo clean

清除缓存文件 (db.json) 和已生成的静态文件 (public)。

在某些情况（尤其是更换主题后），如果发现您对站点的更改无论如何也不生效，您可能需要运行该命令。

### list

\$ hexo list \<type\>

列出网站资料。 version

\$ hexo version

显示 Hexo 版本。
