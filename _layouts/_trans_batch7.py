# -*- coding: utf-8 -*-
TRANS = {
"WYTkBSXUaU4m": """Trilium 可以导入 ENEX 文件，Evernote 使用这种格式进行备份/导出。一个 ENEX 文件代表一个笔记本的内容（笔记和资源）。

## 导入过程

第一步是从 Evernote 导出数据：

*   导出单个笔记本：
    *   在左侧边栏中，选择_笔记本（Notebooks）_。
    *   如果你有笔记本堆栈，请展开它。
    *   右键单击一个笔记本并选择_导出笔记本（Export notebook）_。
    *   确保设置了以下选项：
        *   文件格式设置为_ENEX 格式（ENEX format）_。
        *   _导出笔记属性（Export note attributes）_中的所有选项都已勾选。
*   导出一批笔记（最多 100 条）：
    *   在左侧边栏中，选择_笔记（Notes）_。
    *   在笔记列表中，<kbd>Ctrl</kbd>-单击各个笔记以选择它们。或者，单击一个笔记，然后 <kbd>Shift</kbd>-单击更远的笔记以选择两者之间的所有笔记。
    *   底部应该有一个带选项的浮动条。选择 \[…\] → _导出（Export）_。
    *   确保设置了以下选项：
        *   文件格式设置为_ENEX 格式（ENEX format）_。
        *   _导出笔记属性（Export note attributes）_中的所有选项都已勾选。
*   导出单条笔记：
    *   选择该笔记。
    *   在右上角，按 \[…\] 按钮并选择_导出（Export）_。
    *   确保设置了以下选项：
        *   文件格式设置为_ENEX 格式（ENEX format）_。
        *   _导出笔记属性（Export note attributes）_中的所有选项都已勾选。

> [!TIP]
> 要批量导出多个笔记本，可以考虑使用名为 [evernote-backup](https://github.com/vzhd1701/evernote-backup) 的第三方 CLI 工具

拿到 ENEX 文件后，请执行以下操作将其导入 Trilium：

1.  在<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>中，右键单击并选择_导入到笔记（Import into note）_。
2.  在_从何处导入（Import from）_部分，选择_Evernote_。
3.  导入期间，你会看到"导入进行中（Import in progress）"消息。如果导入成功，消息会变为"导入成功完成（Import finished successfully）"，然后消失。
4.  我们建议你检查导入的笔记及其附件，以确认没有丢失任何数据。

## 支持的功能

以下功能在导入过程中会被 Trilium 保留：

*   基本格式（粗体、斜体、下划线、删除线、颜色、高亮、上标、下标、文本对齐、行内代码）。
*   标题层级（这些标题会被调整为从 H2 开始，因为 H1 保留给笔记标题，请参阅[标题（Headings）](#root/_help_Gr6xFaF6ioJ5)）
*   待办事项列表。新的任务格式会被折叠为标准待办事项列表
*   <a class="reference-link" href="#root/_help_mT0HEkOsz6i1">图片（Images）</a>和<a class="reference-link" href="#root/_help_0vhv7lsOLy82">附件（Attachments）</a>
*   <a class="reference-link" href="#root/_help_S6Xx8QIWTV66">列表（Lists）</a>（带项目符号或编号）
*   <a class="reference-link" href="#root/_help_NdowYOC1GFKS">表格（Tables）</a>
*   块引用
*   [提示框（Admonitions）](#root/_help_NwBbFdNZ9h7O)会被保留，包括其表情符号（作为内容的一部分添加）。
*   <a class="reference-link" href="#root/_help_QxEyIjRBizuC">代码块（Code blocks）</a>，会尽力恢复其语言。
*   <a class="reference-link" href="#root/_help_s1aBHPd79XYj">Mermaid 图表（Mermaid Diagrams）</a>
*   <a class="reference-link" href="#root/_help_YfYAtQBcfo5V">数学公式（Math Equations）</a>
*   折叠区域
*   外部链接
*   如果目标笔记属于同一次导入，内部链接会被重写为<a class="reference-link" href="#root/_help_hrZ1D00cLbal">内部（引用）链接（Internal (reference) links）</a>。

## 限制

*   单次导入的大小限制为 250Mb。如果文件总大小更大，你可以提高[上传限制（upload limit）](#root/_help_WOcw2SLH6tbX)，或拆分文件，并按需要多次运行导入。
*   所有资源（图片除外）都会作为笔记的附件创建。
*   如果 ENEX 文件中有 HTML，导入到 Trilium 后 HTML 格式可能会损坏或丢失。请参阅<a class="reference-link" href="#root/_help_wy8So3yZZlH9">报告问题（Reporting issues）</a>。

### 指向其他笔记的链接

自 v0.104.0 起，ENEX 导入器会尝试通过将 Evernote 特有的 URL 转换为 Trilium 的<a class="reference-link" href="#root/_help_hrZ1D00cLbal">内部（引用）链接（Internal (reference) links）</a>来自动重建指向其他笔记的链接。

由于 ENEX 格式不提供笔记的唯一 ID，笔记引用是通过笔记标题确定的。

限制：

*   只有属于同一次导入的笔记才会将其链接重写为引用链接，以避免链接到错误的笔记。
*   如果有两个同名笔记，则不会创建内部链接，以避免指向错误的笔记。
*   无法重写的链接（例如引用缺失/重复的笔记）将保留其原始的 `evernote://` URL。
*   它不会修复指向锚点的链接，也不会修复指向你在创建链接后在 Evernote 中重命名的笔记的链接。

#### 后处理笔记

> [!TIP]
> 此脚本允许在导入完成后重写链接，也应该允许在两次不同导入之间查找链接。

如果你想在导入所有 ENEX 文件后恢复 Trilium 中的内部链接，可以使用或改编这个自定义脚本：<a class="reference-link" href="#root/_help_dj3j8dG4th4l">按标题处理内部链接（Process internal links by title）</a>

该脚本执行以下操作：

1.  查找所有 Evernote 内部链接。
2.  对于每个链接，检查其链接文本是否与笔记标题匹配，如果匹配，则将 Evernote 链接替换为 Trilium 内部链接。如果不匹配，则保留 Evernote 链接。
3.  如果找到多个匹配笔记标题的笔记，则保留 Evernote 链接。
4.  它将结果输出到一个日志中，你可以在 Trilium 中相应的代码笔记里查看。

该脚本有以下限制：

*   它不会修复指向锚点的链接，也不会修复指向你在创建链接后在 Evernote 中重命名的笔记的链接。
*   一些笔记标题可能无法很好地识别，即使它们存在。特别是当笔记标题包含一些特殊字符时。如果这有问题，请考虑<a class="reference-link" href="#root/_help_wy8So3yZZlH9">报告问题（Reporting issues）</a>。

## 报告问题

导入 Evernote 笔记本时，你可能会发现笔记导入方式存在问题；在这种情况下，请考虑[报告（reporting）](#root/_help_wy8So3yZZlH9)它。

报告此类问题时，请务必提供以下信息：

*   原始笔记的 `.enex` 导出。这使我们能够重现问题。
*   一张导入前原始样子的截图，以及导入后样子的截图。""",
}
