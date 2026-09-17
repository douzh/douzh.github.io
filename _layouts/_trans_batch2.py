# -*- coding: utf-8 -*-
TRANS = {
"ThwV4HVTjhHq": """Trilium 同时支持 Markdown 的导入和导出，并尽可能保持兼容性。

## 导入

### 剪贴板导入

如果你想从剪贴板导入一小段 Markdown，可以通过编辑器块菜单完成：

![](Markdown_markdown-inline-import.gif)

### 文件导入

你也可以从文件导入 Markdown 文件：

*   单个 markdown 文件（扩展名为 .md）
*   整个 markdown 文件树（打包成 [.zip](https://en.wikipedia.org/wiki/Tar_(computing)) 压缩包）
    *   Markdown 文件需要打包成 ZIP 压缩包，因为浏览器无法读取目录，只能读取单个文件。
    *   你可以使用例如 [7-zip](https://www.7-zip.org) 将 markdown 文件目录打包成 ZIP 文件

![](Markdown_markdown-file-import.gif)

## 导出

### 子树导出

你可以将整个子树导出为 ZIP 压缩包，其中的目录结构会按照子树结构建模：

![](Markdown_markdown-export-subtree.gif)

### 单条笔记导出

如果你想只导出单条笔记而不包含其子树，可以从笔记操作菜单中完成：

![](Markdown_markdown-export-note.gif)

### 导出受保护笔记

如果你想导出受保护的笔记，请先进入受保护会话！这将以未加密的形式导出笔记，因此如果你重新导入到 Trilium，请确保重新保护这些笔记。

## 支持的语法

请参阅专门页面：<a class="reference-link" href="#root/_help_rK9grSgoExl9">支持的语法（Supported syntax）</a>""",

"PrgLiFypVLYw": """## 禁用身份验证

如果你只在 `localhost` 上运行 Trilium，或者身份验证由其他组件处理，你可以通过在 `config.ini` 中添加以下内容来禁用 Trilium 的身份验证：

```
[General]
noAuthentication=true
```

自 v0.94.1 起，禁用身份验证甚至会绕过<a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">多因素身份验证（Multi-Factor Authentication）</a>。

## 了解会话的工作方式

登录 Trilium 后，应用程序会将登录信息存储到浏览器中的 cookie 中，同时在服务器上存储为一个会话。

如果勾选了"记住我"，登录将在 21 天后过期。可以通过修改 `config.ini` 中的 `Session.cookieMaxAge` 值来调整此期限。例如，让会话在一天后过期：

```
[Session]
cookieMaxAge=86400
```

当未勾选"记住我"时，行为有所不同。在客户端/浏览器层面，身份验证没有过期日期，但一旦用户关闭浏览器，它会被自动清除。然而，服务器也会在与_应用程序的最后交互_后约 24 小时内解除此身份验证。

## 查看活动会话

登录会话现在与用户数据存储在同一个<a class="reference-link" href="#root/_help_wX4HbRucYSDD">数据库（Database）</a>中。要查看哪些会话处于活动状态，请打开<a class="reference-link" href="#root/_help_YKWqdJhzi2VY">SQL 控制台（SQL Console）</a>并运行以下查询：

```
SELECT * FROM sessions
```

过期的会话由服务器定期清理，通常每小时一次。

## 另请参阅

*   <a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">多因素身份验证（Multi-Factor Authentication）</a>""",

"xpEQq6DVwNEt": """在提交错误报告时，提供所有可用的错误日志非常重要。

有两种类型的错误日志，在报告错误时都非常有用。

*   <a class="reference-link" href="#root/_help_bnyigUA2UK7s">后端（服务器）日志（Backend (server) logs）</a>
*   <a class="reference-link" href="#root/_help_9yEHzMyFirZR">前端日志（Frontend logs）</a>

## 提供敏感数据

如果你不想在公开的 GitHub issue 中附加日志或任何敏感内容，可以随时通过我们的 Matrix [支持频道](https://github.com/TriliumNext/Trilium#-discuss-with-us)联系开发者。

使用此邮箱还可以提供任何有助于分析 bug 的内容，例如正在导入的文件/图片/ZIP 或[匿名化数据库（anonymized database）](#root/_help_x59R8J8KV5Bp)。

### 导出笔记子树以便复现

很多时候，bug 会在特定笔记中显现，拥有这些笔记将大大方便复现和修复。

在这种情况下，请在左侧树上右键单击相关笔记子树，选择"导出 - 导出为 HTML ZIP"：

![](Error%20logs_error-logs-export-subtree.png)""",

"qUcx4LMepbQt": """在某些情况下，直接修改 Trilium 使用的 SQLite 数据库是可取的。

如果你正在进行需要手动修改数据库的高级开发或故障排查，建议考虑创建 `document.db` 文件的备份。

## 使用 SQL 控制台在内部修改

SQL 控制台是 Trilium 内置的数据库编辑器。

请参阅<a class="reference-link" href="#root/_help_YKWqdJhzi2VY">SQL 控制台（SQL Console）</a>。

## 在外部修改数据库

有时 SQL 控制台无法使用（例如应用程序无法启动时）。

在进行外部修改时，请考虑关闭桌面应用程序。如果修改的是服务器数据库，请停止服务或 Docker 容器。

### 使用 DB Browser for SQLite

DB Browser for SQLite 是一个跨平台的编辑器，可以使用图形用户界面修改数据库。

操作步骤如下：

1.  在主菜单中选择"文件 → 打开数据库…"，然后导航到[数据目录（Data directory）](#root/_help_tAassRL4RSQL)中的数据库。
2.  选择_执行 SQL（Execute SQL）_选项卡。
3.  输入所需的 SQL 语句。
4.  按"执行 SQL"选项卡下方工具栏中的"播放"按钮（或 F5 键）。
5.  按主工具栏中的"写入更改（Write Changes）"。
6.  关闭应用程序或关闭数据库。

![](Manually%20altering%20the%20database_image.png)

### 使用 SQLite CLI

首先，通过指定数据库路径启动 SQLite 3 CLI：

```
sqlite3 ~/.local/share/trilium-data/document.db
```

*   在提示符中直接输入语句，并确保以 `;` 字符结尾。
*   要退出，直接输入 `.quit` 并回车。""",
}
