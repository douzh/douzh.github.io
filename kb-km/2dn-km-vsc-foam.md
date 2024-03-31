---
title: 知识管理Foam使用说明
date: 2024-03-13
type: kbase
tags: 知识管理/文档笔记
---
# Foam使用说明

- [Foam中文说明](https://xiaoland.github.io/Foam-Chinese-Document/)
- [VS Code 中的双链笔记：Foam 使用体验分享](https://ios.sspai.com/post/70956)

在 VS Code 中执行一个命令（内建的或者来自扩展的），需要通过命令面板（command palette，快捷键 `Ctrl + Shift + P`）。这篇文章中主要使用的一些命令如下：

- Foam: Create New Note：在当前目录下创建新的笔记条目。
- Foam: Show Graph：打开知识图谱页面。
- Foam: Open Daily Note：创建 Daily Note。
- Foam: Create New Note From Template：从模板创建新笔记。
  
在 VS Code 中搜索文件可以使用快捷键 `Ctrl + P` 弹出对应面板，通过文件名进行搜索。打开侧边栏的搜索面板（快捷键 `Ctrl + Shift + F`），可以对整个笔记库的内容进行检索。

## 创建双向链接

在 Foam 中创建一个双向链接与在其他软件中无异，即使用 `[[]]` 符号。如果被 `[[]]` 包括的文本有对应的笔记，就会成为一个双向链接。当将鼠标移动并悬浮在文本上时，会显示这一条目的预览，可以按下 Ctrl + 单击 或鼠标右键选择「转到定义」来打开这条笔记；如果没有对应的笔记，则会创建一个占位符，按下Ctrl + Click 创建可以对应的条目。

Foam 并不支持 Roam Research 式的块引用，但支持标题引用，使用方式为：`[[wikilink#heading]]`，这样便能引用对应条目中该标题下的内容。

## 笔记元数据

使用 Markdown 文档时，在笔记头部使用 YAML 语言格式的字段来定义这个文档的元数据是一个良好的习惯，Foam 也支持这一功能（note property）。其格式如下：

```yaml
---
title: Title Case Name
date: yyyy-mm-dd
type: feature
tags: tag1, tag2, tag3
---
```

`title` 属性定义了这条笔记的标题和在知识图谱（Foam Graph）上的名称（标识笔记时，优先级为： `title` 属性 > 正文的一级标题 > 笔记的文件名）。

`tags` 属性定义这条笔记的标签。多个标签之间用空格或半角逗号分隔。另外也可以通过在笔记正文中使用 `#tag` 来添加标签。Foam 支持多级标签即 `#tag/sub-tag`，但笔者并不使用，个人认为这还是在按树状结构来组织笔记。

`type` 属性可以用于在知识图谱中区分笔记的类型，可以将不同 `type` 属性的笔记用不同颜色表示。

也可以自定义其他的属性，如：日期（`date`）、作者（`author`）、来源（`source`）等。

## 侧边栏面板

Foam 的侧边栏面板包含这几项功能：

- 文件管理
- 大纲: 查看笔记的目录结构
- 时间线: 记录了文件修改和 git 操作的历史
- 标签管理（Tag Explorer）: 可以查看笔记库中的所有标签并进行检索
- 占位符（Placeholders）: 显示了所有被  `[[]]` 标记但没有创建对应笔记的项目
- 孤立笔记（Orphans）: 没有引用也没有被引用的笔记条目
- 反向链接（Backlinks）: 列出全部引用了此条目的笔记

这些功能面板可以通过拖动自行排序或隐藏，也可以拖动到底部面板（按下`Ctrl + J` 弹出）。个人习惯将反向链接面板和标签管理面板放到底部，方便查看。

## 高级功能

todo

代码片段：创建笔记元数据

插入时间戳

创建和使用模板