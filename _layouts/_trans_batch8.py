# -*- coding: utf-8 -*-
TRANS = {
"iLOq7QKFCwlZ": """集合（Collections）是一种独特的笔记类型，它们没有内容，而是以各种展示方式显示其子笔记。

## 主要集合

|  |  |
| --- | --- |
| <figure class="image"><img style="aspect-ratio:1651/810;" src="Collections_collection_calendar.webp" width="1651" height="810"></figure> | <a class="reference-link" href="#root/_help_xWbu3jpNWapp">日历（Calendar）</a>      <br>显示周、月或年日历，笔记以事件形式显示。可以通过在日历上拖动轻松添加新事件。 |
| <figure class="image"><img style="aspect-ratio:1643/647;" src="Collections_collection_table.webp" width="1643" height="647"></figure> | <a class="reference-link" href="#root/_help_2FvYrpmOXm29">表格（Table）</a>      <br>将每个笔记显示为表格中的一行，同时显示<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>。这使笔记的属性易于可视化，也易于编辑。 |
| <figure class="image"><img style="aspect-ratio:1174/850;" src="Collections_collection_board.webp" width="1174" height="850"></figure> | <a class="reference-link" href="#root/_help_CtBQqbwXDx1w">看板（Kanban Board）</a>      <br>将笔记显示在列中，按标签的值分组。可以轻松创建或拖动项目和列来更改其状态。 |
| <figure class="image"><img style="aspect-ratio:844/639;" src="Collections_collection_geomap.webp" width="844" height="639"></figure> | <a class="reference-link" href="#root/_help_81SGnPGMk7Xc">地理图（Geo Map）</a>      <br>显示地理地图，笔记以地图上的标记/图钉形式表示。可以通过点击地图轻松添加新事件。 |
| <figure class="image"><img style="aspect-ratio:1120/763;" src="Collections_collection_presentation.webp" width="1120" height="763"></figure> | <a class="reference-link" href="#root/_help_zP3PMqaG71Ct">演示（Presentation）</a>      <br>将每个笔记显示为一张幻灯片，可以全屏演示并带平滑过渡，或导出为 PDF 以便分享。 |

## 经典集合

经典集合是只读模式，将所有子笔记的内容编译成一个连续的视图。这对于阅读被拆分成较小、可管理片段的大量信息非常理想。

*   <a class="reference-link" href="#root/_help_8QqnMzx393bx">网格视图（Grid View）</a>是子笔记的默认展示方式（请参阅<a class="reference-link" href="#root/_help_0ESUbbAxVnoK">笔记列表（Note List）</a>），笔记以带有可见标题和内容的磁贴形式显示。
*   <a class="reference-link" href="#root/_help_mULW0Q3VojwY">列表视图（List View）</a>与<a class="reference-link" href="#root/_help_8QqnMzx393bx">网格视图（Grid View）</a>类似，但它将笔记一个接一个地显示，内容可展开/折叠，并且可以递归工作。

经典集合使用分页来支持大量笔记。页面大小可以通过 `pageSize` 自定义。

## 创建新集合

要创建新集合，请在<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>中右键单击，查找_集合（Collections）_条目并选择所需类型。

默认情况下，集合带有默认配置，有时甚至带有示例笔记。要从头完全创建集合：

1.  创建一个_文本（Text）_类型（或任何类型）的新笔记。
2.  将[笔记类型](#root/_help_KSZ04uQ2D1St)更改为_集合（Collection）_。
3.  在<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>中，选择所需的视图类型。
4.  查阅相应视图类型的帮助页面，了解如何配置它们。

## 配置

要更改集合的配置，甚至切换到不同的集合（例如从看板切换到日历），请参阅笔记顶部的<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>栏。

## 已归档笔记

默认情况下，[已归档笔记（archived notes）](#root/_help_MKmLg5x6xkor)不会显示在集合中。可以通过转到<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>并勾选_显示已归档笔记（Show archived notes）_来更改此行为。

已归档笔记通常会以灰色显示，与普通笔记形成对比。

## 从笔记树中隐藏子笔记

对于项目数量较多的集合，出于性能考虑并减少杂乱，将项目从笔记树中隐藏会很有帮助。这对于独立集合（例如地理图或任务板）尤其有用。

为此，请转到<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>并选择_在树中隐藏子笔记（Hide child notes in tree）_。

## 高级用例

### 为集合添加描述

要在集合前添加文本，例如描述它：

1.  创建一个新集合。
2.  将[笔记类型](#root/_help_KSZ04uQ2D1St)从_集合（Collection）_更改为_文本（Text）_。

现在文本将显示在上方，同时仍保持集合视图。

这种方法的唯一缺点是<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>将不再显示。在这种情况下，请手动修改属性，或暂时切回_集合（Collection）_类型进行配置。

### 使用已保存搜索

集合默认只显示子笔记。然而，可以使用<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>功能显示整个树中的笔记，并带有高级查询功能。

为此，只需开始一个<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>，转到<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>中的_集合属性（Collection Properties）_选项卡，然后选择所需的集合类型。要保留基于搜索的集合，请使用<a class="reference-link" href="#root/_help_m523cpzocqaD">已保存搜索（Saved Search）</a>。

> [!IMPORTANT]
> 在搜索时，没有任何集合会显示搜索结果中的子笔记。原因是搜索可能会多次命中同一笔记，导致结果数量呈指数级上升。

## 底层机制

集合本身只是没有内容的笔记，依赖<a class="reference-link" href="#root/_help_0ESUbbAxVnoK">笔记列表（Note List）</a>机制（即在笔记底部列出子笔记的机制）来显示信息。

默认情况下，新集合使用预定义的<a class="reference-link" href="#root/_help_KC1HB96bqqHX">模板（Templates）</a>，这些模板安全地存储在<a class="reference-link" href="#root/_help_2mUhVmZK8RF3">隐藏笔记（Hidden Notes）</a>中，用于定义一些基本配置（例如视图类型），也定义一些<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>以方便编辑。

集合不将其配置（例如地图上的位置、表格中隐藏的列）存储在笔记自身的内容中，而是作为附件存储。""",

"B5oluBvu2gBY": """文本笔记是 Trilium 中的默认笔记类型，支持富文本格式化、表格、图像、提示框以及许多其他功能。

## 格式化工具栏

与文本笔记的大部分交互都是通过内置工具栏完成的。根据偏好，有两种不同的布局：

*   _浮动工具栏（Floating toolbar）_默认隐藏，只在需要时出现。在此模式下实际上有两个不同的工具栏：  
    <img src="1_Text_image.png" width="496" height="91"> <img src="2_Text_image.png" width="812" height="114">
*   一个在选择文本时出现的工具栏。它提供文本级别的格式化，例如粗体、斜体、文本颜色、行内代码等。  
    _<img src="Text_image.png" width="1109" height="124">_

更多信息请参阅<a class="reference-link" href="#root/_help_nRhnJkTT8cPs">格式化工具栏（Formatting toolbar）</a>。

## 功能与格式化

以下是文本笔记支持的各种功能列表：

| 专门文章 | 功能 |
| --- | --- |
| <a class="reference-link" href="#root/_help_Gr6xFaF6ioJ5">常规格式化（General formatting）</a> | 标题（章节标题、段落）、字号、粗体、斜体、下划线、删除线、上标、下标、字体颜色和背景颜色、移除格式 |
| <a class="reference-link" href="#root/_help_S6Xx8QIWTV66">列表（Lists）</a> | 项目符号列表、编号列表、待办列表 |
| <a class="reference-link" href="#root/_help_NwBbFdNZ9h7O">块引用和提示框（Block quotes & admonitions）</a> | 块引用、提示框 |
| <a class="reference-link" href="#root/_help_NdowYOC1GFKS">表格（Tables）</a> | 基本表格、合并单元格、样式化表格和单元格、表格标题 |
| <a class="reference-link" href="#root/_help_UYuUB1ZekNQU">开发者专用格式（Developer-specific formatting）</a> | 行内代码、代码块、键盘快捷键 |
| <a class="reference-link" href="#root/_help_AgjCISero73a">脚注（Footnotes）</a> | 脚注 |
| <a class="reference-link" href="#root/_help_mT0HEkOsz6i1">图片（Images）</a> | 图片 |
| <a class="reference-link" href="#root/_help_QEAPj01N5f7w">链接（Links）</a> | 外部链接、Trilium 内部链接 |
| <a class="reference-link" href="#root/_help_nBAXQFj20hS1">包含笔记（Include Note）</a> | 包含笔记 |
| <a class="reference-link" href="#root/_help_CohkqWQC1iBv">插入按钮（Insert buttons）</a> | 符号、[缺失笔记]、Mermaid 图表、水平分隔线、分页符 |
| <a class="reference-link" href="#root/_help_dEHYtoWWi8ct">[缺失笔记]</a> | 缩进（含 Markdown 导入）、[剪切到子笔记（Cut to subnote）](#root/_help_2x0ZAX9ePtzV) |
| <a class="reference-link" href="#root/_help_gLt3vA97tMcp">[缺失笔记]</a> | [斜杠命令（Slash Commands）](#root/_help_ZlN4nump6EbW)、[模板（Templates）](#root/_help_KC1HB96bqqHX)、[格式刷（Format Painter）](#root/_help_5wZallV2Qo1t) |

## 只读模式与编辑模式

文本笔记通常以编辑模式打开。然而，如果笔记太大或笔记被明确标记为只读，它们可能以只读模式打开。更多信息请参阅<a class="reference-link" href="#root/_help_CoFPLs3dRlXc">[缺失笔记]</a>。

## 键盘快捷键

有许多键盘快捷键可以在不使用鼠标的情况下格式化文本。所有按键组合的参考，请参阅<a class="reference-link" href="#root/_help_A9Oc6YKKc65v">键盘快捷键（Keyboard Shortcuts）</a>。此外，请参阅<a class="reference-link" href="#root/_help_QrtTYPmdd1qq">类 Markdown 格式化（Markdown-like formatting）</a>作为键盘快捷键的替代方案。

## 内容宽度

为了在更宽的屏幕上获得可读性，文本笔记的宽度通过一个可配置选项进行限制。更多信息请参阅<a class="reference-link" href="#root/_help_t596jLvPrqkS">[缺失笔记]</a>。

## 转换为 Markdown 笔记

文本笔记可以转换为<a class="reference-link" href="#root/_help_6RM1Q7ppFVoj">Markdown</a>笔记。请参阅<a class="reference-link" href="#root/_help_jludwdW7N4hg">在笔记类型之间转换（Converting between note types）</a>。

## 技术细节

对于文本编辑功能，Trilium 使用一个名为<a class="reference-link" href="#root/_help_MI26XDLSAlCD">[缺失笔记]</a>的商业产品（具有开源基础）。这带来了拥有强大 WYSIWYG（所见即所得）编辑器的好处。""",
}
