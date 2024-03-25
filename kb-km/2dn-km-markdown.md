---
title: markdown基础
date: 2018-01-01
type: kbase
tags: 知识管理/文档笔记
---
# markdown基础

| 功能       | 适用范围 | 语法                                                                                                                  |
| ---------- | -------- | --------------------------------------------------------------------------------------------------------------------- |
| 段落       |          | 空白行                                                                                                                |
| 换行符     |          | 行尾留下两个空格                                                                                                      |
| 标题       |          | `#`                                                                                                                 |
| 引用       |          | 使用\> 多个 \> 引用可以缩进                                                                                           |
| 列表       |          | \* \+ \- 数字                                                                                                           |
| 任务列表   |          | [\] 或者\[x\]                                                                                                         |
| 代码块     |          | 块\`\`\` 行内 \`                                                                                                     |
| 公式       |          | 块 `$$` 行内 `$`                                                                                                  |
| 上标下标   |          | html:`a<sub>2</sub>`  `a<sup>2</sup>`<br />latex: `$a_2$` `$a^2$`<br />md: `H~2~O`  `X^2^` 不生效 |
| 分割线     |          | `*** 或 ---`                                                                                                        |
| 目录       |          | `[TOC]`                                                                                                             |
| 链接       |          | ` [an example](http://example.com/ "Title")`                                                                        |
| 内部链接   |          | `[This link](#markdown基础)`                                                                                        |
| 引用式链接 |          |                                                                                                                       |
| 图片       |          | `![](/path/to/img.jpg "Optional title")`                                                                            |
| 强调       |          | 用一个 `*`或 `_`包括的文本                                                                                        |
| 加粗       |          | 两个 `**`或 `__`包裹的文本                                                                                        |
| 删除线     |          | 用\~\~包裹的文本                                                                                                        |
| 下划线     |          | `<u>Underline</u>`                                                                                                  |
| emoji      |          | `:smile:`😄                                                                                                         |
| 脚注       | 不通用   | `你可以使用脚注像这样[^脚注]` 添加脚注 `[^脚注]: 这里写脚注的文本`                                                |

## 块元素

### 段落和换行

一个段落只是一个或者多个连续的文本行。在Markdown源代码中，段落由多个空白行分隔。

按 `Shift`+`Enter`创建一个换行符。然而，大多数的编辑器会忽略单行中端，为了让其它的Markdown编辑器识别你的换行符，可以在行尾留下两个空格或者插入 `<br/>`。

### 标题

标题在行的开始使用1-6个散列字符，对应1-6的标题级别，例如：

```markdown
# 这是一级标题

## 这是二级标题

### 这是三级标题
```

### 引用

Markdown使用电子邮件风格>字符进行块引用。他们被表示为：

```markdown
> 这是一个包含两段的blockquote。这是第一段
>
> 这是第二段。Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.我也不知道这事啥意思



> 这是另一个有一个段落的blockquote。两个区块引用之间有三个空白行分隔。
```

> 这是一个包含两段的blockquote。这是第一段
>
> 这是第二段。Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.我也不知道这事啥意思

> 这是另一个有一个段落的blockquote。两个区块引用之间有三个空白行分隔。

### 列表

输入 `* list item 1`将创建一个无序的列表，`*`符号可以使用 `+`或者 `-`代替。

输入 `1. list item 1`将创建一个有序列表，他们的Markdown源代码如下：

```markdown
## un-ordered list
*   Red
*   Green
*   Blue

## ordered list
1.  Red
2.  Green
3.  Blue
```

### 任务列表

任务列表是标有\[ \] 或者\[x\] (未完成或者完成)的列表，例如：

```markdown
- [ ] a task list item
- [ ] list syntax required
- [ ] normal **formatting**, @mentions, #1234 refs
- [ ] incomplete
- [x] completed
```

- [ ] a task list item
- [ ] list syntax required
- [ ] normal **formatting**, @mentions, #1234 refs
- [ ] incomplete
- [X] completed

可以通过单击项目之前的复选框来更改完成/未完成的状态。

### 代码块

使用代码块很容易，输入\`\`\`然后按下 `entre`键。在\`\`\`之后添加一个可选的语言标识符，我们将通过它进行语法高亮：

例如：

```java
String str = new String("hello world!");
System.out.println(str)
```

### 数学公式

输入 `$$`，然后按下 `Enter`键将触发一个接收_Tex/LaTeX_源码的输入范围。例如：

在Markdown源代码文件中，数学公式是被 `$$`标记的_LaTeX_表达式：

```markdown
$$
\mathbf{V}_1 \times \mathbf{V}_2 =  \begin{vmatrix} 
\mathbf{i} & \mathbf{j} & \mathbf{k} \\
\frac{\partial X}{\partial u} &  \frac{\partial Y}{\partial u} & 0 \\
\frac{\partial X}{\partial v} &  \frac{\partial Y}{\partial v} & 0 \\
\end{vmatrix}
$$
```

$$
\mathbf{V}_1 \times \mathbf{V}_2 =  \begin{vmatrix} 
\mathbf{i} & \mathbf{j} & \mathbf{k} \\
\frac{\partial X}{\partial u} &  \frac{\partial Y}{\partial u} & 0 \\
\frac{\partial X}{\partial v} &  \frac{\partial Y}{\partial v} & 0 \\
\end{vmatrix}
$$

### 表格

```markdown
| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
```

最后，通过冒号 `：`在标题行中，可以定义文本对齐方式，最左侧的买好表示左对齐，最右侧的冒号表示右对齐，两次都有冒号表示中心对齐。

```markdown
| Left-Aligned  | Center Aligned  | Right Aligned |
| :------------ |:---------------:| -----:|
| col 3 is      | some wordy text | $1600 |
| col 2 is      | centered        |   $12 |
| zebra stripes | are neat        |    $1 |
```

### 分割线

在空白行输入 `***`或者 `---` 然后按 `enter`键会出现分割线

---

## 段元素

当你输入之后，段元素就会被渲染和呈现出来，将光标移动到段元素中，会显示Markdown源码，接下来将介绍段元素的用法：

### 目录

输入 `[toc]`，然后按 `enter`键将创建一个“目录”部分，从一个人的写作中提取所有标题，其内容将自动更新。

### 链接

Markdown支持两种风格的链接：内联和引用。在两种样式中，链接文本由\[方括号\]分隔。

要创建内联链接，请在链接文本的关闭方括号后立即使用一组常规括号。 在括号内，将链接所在的网址与链接的可选标题一起放在引号中。 例如：

```markdown
This is [an example](http://example.com/ "Title") inline link.

[This link](http://example.net/) has no title attribute.
```

实现效果：

This is [an example](http://example.com/%22Title%22) inline link. (`<p>This is <a href="http://example.com/" title="Title">`)

[This link](http://example.net/) has no title attribute. (`<p><a href="http://example.net/">This link</a> has no`)

### 引用式链接

“引用式链接” 是一种 Markdown 中用于提供链接定义的方式，其主要目的是使文本更加清晰和易读，减少文本中链接的干扰。

语法为:

```
[链接文本][标识符]

[标识符]: 实际链接URL "链接标题（可选）"
```

参考链接具有以下特点：

- 提高文本可读性： 参考链接可以使文本更加整洁，不会因为长链接而显得混乱。链接的定义被移到文档末尾或其他地方，使得文本更容易阅读。
- 复用链接定义： 如果文档中有多处需要使用相同的链接，可以通过参考链接一次定义，多次引用的方式实现。这样有助于维护和管理链接，防止出现重复定义。

### 内部链接

你可以将标题设置为一个连接，允许你点击标题后，跳转到文章中指定的部分，例如：

```
[This link](#markdown基础)
```

Ctrl + Click [This link](#markdown基础)会跳转到块元素标题的位置。

### URL地址

Typora允许插入URL作为链接，用尖括号包起来，`<`尖括号 `>`。

`<i@typora.io>` 就变成了[i@typora.io](mailto:i@typora.io).

### 图片

图片和链接看起来是一样的，但是图片需要在链接前加上 `!`感叹号字符，图片的语法为：

```markdown
![](/path/to/img.jpg)

![](/path/to/img.jpg "Optional title")
```

### 强调

Markdown将星号(`*`)和下划线(`_`)视为强调的指标，用一个 `*`或 `_`包括的文本，将被HTML中 `<em>`标签包裹，例如：

```markdown
*single asterisks*

_single underscores_
```

显示：

_single asterisks_

_single underscores_

GFM会忽略词中的下划线，因为下划线经常被用在代码和名字中，例如：

> wow\_great\_stuff
>
> do\_this\_and\_do\_that\_and\_another\_thing.

要在一个位置上产生一个文字星号或下划线，需要在前面加反斜杠：

```markdown
\*this text is surrounded by literal asterisks\*
```

建议使用 `*`字符。

### 加粗

两个 `**`或 `__`包裹的文本，例如：

```markdown
**double asterisks**

__double underscores__
```

显示：

**double asterisks**

**double underscores**

建议使用 `**`字符。

### 代码

使用反引号包裹代码，与预格式化的代码块不同，代码段是表示的是正常段落中的代码：

```markdown
Use the `printf()` function.
```

显示：

Use the `printf()` function.

### 删除线

用~~包裹的文本

`~~Mistaken text.~~` 会变成~Mistaken text.~

### 下划线

下划线由原始的HTML提供。

`<u>Underline</u>` 变成 `<u>`Underline `</u>` .

### emoji表情：happy

输入emoji语法：`:smile:`😄

用户可以通过按“ESC”键触发表情符号的自动完成建议，或在首选面板上启用后自动触发。 此外，还支持从菜单栏中的“Edit” - >“Emoji＆Symbols”直接输入UTF8表情符号。

### 数学公式

为了使用这个特性，请先在 `Preference`面板中的 `Markdwn`选择开启，然后使用 `$`来包裹TeX命令，例如：`$\lim_{x \to \infty} \exp(-x) = 0$`，将会渲染为LaTeX命令。

```
$lim\_{x \\to\\infty} \\exp(-x) = 0$
```

$lim\_{x \\to\\infty} \\exp(-x) = 0$

### 下标和上标

- 使用 `~`来包裹下标内容，例如：`H~2~O`,H~2~O
- 使用 `^`包裹上标内容，例如 `X^2^`,`X^2^

LaTex形式

- 下标：`$a_2$` $a_2$
- 上标：`$a^2$` $a^2$

Html形式

- 下标：`a<sub>2</sub>` a <sub>2 </sub>
- 上标：`a<sup>2</sup>` a <sup>2 </sup>
