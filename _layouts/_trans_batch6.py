# -*- coding: utf-8 -*-
TRANS = {
"LvouaBp1WS0Y": """从 v0.104.0 版本开始，Trilium 的导入器扩展为针对特定应用的导入。与标准 HTML 或<a class="reference-link" href="#root/_help_Oau6X9rCuegd">Markdown</a>导入/导出不同，这些导入器针对特定应用的功能和笔记结构进行了定制。

## 支持的应用

*   <a class="reference-link" href="#root/_help_GnhlmrATVqcH">Microsoft OneNote</a>，通过使用 Microsoft Graph 连接到你的账户。
*   <a class="reference-link" href="#root/_help_Y5mKeJ6dsCld">Notion</a>，通过 ZIP 导出。
*   <a class="reference-link" href="#root/_help_nhQVuO4zvIxi">Google Keep</a>，通过_Google 导出（Google Takeout）_的 ZIP 导出（Keep 没有专用的导出机制）。
*   <a class="reference-link" href="#root/_help_syuSEKf2rUGr">Evernote</a>，通过 ENEX 导出。
*   <a class="reference-link" href="#root/_help_83zmPBJRgfnW">Anytype</a>，通过 JSON 导出。
*   <a class="reference-link" href="#root/_help_iWD7wiIuMtgV">Obsidian</a>，通过 vault 的 ZIP 导出。

## 从其他应用导入数据

要从某个应用导入，有两种方式访问导入对话框：

*   在<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>中，右键单击笔记并选择_导入到笔记（Import into note）_。
*   在<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>区域，选择_导入文件（Import files）_。

顶部会出现受支持应用的列表，每个应用都有自己的配置。只需点击一个应用，然后按照屏幕上的说明继续操作即可。

## 致谢

*   Trilium 的导入器灵感来自 Obsidian 的 [Importer 插件](https://github.com/obsidianmd/obsidian-importer)（基于 MIT 许可证），例如关于 OneNote 连接过程或 Notion ID 管理方面。""",

"Gh7vim8mU3XG": """## 动机

Trilium 的核心特性是能够将你的笔记组织成分层树状结构。

因此，可以预期你会有一个精细而深入的笔记层级——每个子树都代表你的知识库更精细、更专业化的视图。

这是一种非常强大的方法，但它也隐含了一个假设：每个"子主题"由且仅由一个父级"拥有"。我会用一个例子来说明——假设我的基本结构是这样的：

*   技术
    *   编程
        *   Kotlin
        *   JavaScript
    *   操作系统
        *   Linux
        *   Windows

现在，我开始学习 [Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell))，并希望创建与此主题相关的笔记。但现在我面临一个问题：该把它归类到哪里。这里的问题是，Bash 既是一种编程语言，也是一种与 Linux 紧密相关的工具（shell）。它似乎同时属于这两者，我无法（也不想）只选择其中一个。

## 解决方案

对上述问题的解决方案是允许笔记有多个父级。

我把这些称为"克隆（clones）"，但这有点误导——并没有原始笔记和克隆笔记之分——两个父级分类中的笔记是完全相同的。

关于"克隆"的另一个误导之处在于，它暗示笔记的副本已经被创建。这并不完全正确，笔记本身只保留一份原始内容，它只是在树层级中的多个位置被引用。因此，在一个分类中修改它会改变所有其他分类中的它，因为它们都是同一个笔记。

这是使用克隆后的最终结构：

*   技术
    *   编程
        *   Kotlin
        *   JavaScript
        *   Bash
            *   一些子笔记 ...
    *   操作系统
        *   Linux
            *   Bash
                *   一些子笔记 ...
        *   Windows

所以现在"Bash"子树出现在层级中的多个位置。两个 Bash 子树是相同的，包含相同的子分类和笔记。

### 演示

![](Cloning%20Notes_create-clone.gif)

在演示中，你可以看到如何使用上下文菜单创建克隆。也可以使用添加链接对话框或 <kbd>Ctrl</kbd>+<kbd>C</kbd> 和 <kbd>Ctrl</kbd>+<kbd>V</kbd> [键盘快捷键](#root/_help_A9Oc6YKKc65v)来完成。

你可以查看所有可用克隆的列表：

*   在<a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>中，适用于<a class="reference-link" href="#root/_help_IjZS7iK5EXtb">新布局（New Layout）</a>。
*   对于旧布局，在<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>的"笔记路径（Note Paths）"选项卡中。

树视图中克隆笔记的标题右侧有一个星号，便于看出该笔记也放置在其他位置。

## 删除笔记/克隆

有了克隆，删除的工作方式可能不会立即显现。

如果你尝试删除一个笔记，它的工作方式如下：

1.  如果笔记有多个克隆，只删除这个克隆，并保持实际笔记（及其其他克隆）不变。
2.  如果这个笔记没有任何其他克隆，则删除该笔记
    *   对笔记的所有子笔记运行从 1. 开始的整个过程。""",
}
