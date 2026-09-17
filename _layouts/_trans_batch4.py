# -*- coding: utf-8 -*-
TRANS = {
"DrRneFhQ9YLP": """Trilium 支持多种[笔记类型（Note Types）](#root/_help_KSZ04uQ2D1St)的一个核心方面，是它使用了各种现成或可复用的库。

本页展示了一些所使用的技术，既为了更好理解 Trilium 的工作原理，也为了致谢这些技术的开发者。

## CKEditor

CKEditor 是[文本（Text）](#root/_help_iPIMuisry3hd)笔记背后的编辑器，也集成在应用程序的各个方面，例如<a class="reference-link" href="#root/_help_AlJ73vBCjWDw">状态栏（Status bar）</a>中的属性编辑器，或<a class="reference-link" href="#root/_help_GBBMSlVSOIGP">AI</a>侧边栏或笔记中的聊天框。

更多信息请参阅<a class="reference-link" href="#root/_help_MI26XDLSAlCD">CKEditor</a>。

## Excalidraw

[Excalidraw](https://excalidraw.com/) 是<a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a>笔记背后的技术。该库的源代码可在 [GitHub](https://github.com/excalidraw/excalidraw) 上获取。

我们使用的是未修改的版本，因此它与原版存在相同的[问题（issues）](https://github.com/excalidraw/excalidraw/issues)。

## MapLibre GL JS

Trilium v0.105.0 为<a class="reference-link" href="#root/_help_81SGnPGMk7Xc">地理图（Geo Map）</a>集合引入了 [MapLibre GL JS](https://github.com/maplibre/maplibre-gl-js)，通过使用图形加速（WebGL）带来了更好的性能。Trilium 自带自己的轨迹解析逻辑以及大部分 UI，例如右侧面板。

## MindElixir

MindElixir 是我们用于<a class="reference-link" href="#root/_help_gBbsAeiuUxI5">思维导图（Mind Map）</a>笔记类型的库。主库可在 [GitHub 上的 mind-elixir-core](https://github.com/SSShooter/mind-elixir-core/issues) 获取。

Trilium 自带自己的 UI，涵盖工具栏、上下文菜单、缩放按钮以及点击节点时显示的右侧面板。

## FullCalendar

[FullCalendar](https://fullcalendar.io/) 是<a class="reference-link" href="#root/_help_xWbu3jpNWapp">日历（Calendar）</a>集合背后的技术，提供各种视图（日、周、月、年）和事件管理。Trilium 还以编辑时出现的弹出窗口以及<a class="reference-link" href="#root/_help_CssoWBu8I7jF">集合属性（Collection Properties）</a>中出现的头部等形式自带 UI。

基于 MIT 许可证授权。""",

"45vpyB99b5XE": """本页说明如何在 TriliumNext 中操作笔记树，重点介绍移动笔记。

![](Note%20Tree_image.png)

## 拖放

![拖放示例](Note%20Tree_drag-and-drop.gif)

你可以通过拖放笔记轻松重新排列笔记树，如上例所示。

## 键盘操作

![使用键盘键移动笔记的示例](Note%20Tree_move-note-with-keyboard.gif)Trilium 使用以下[快捷键（shortcuts）](#root/_help_A9Oc6YKKc65v)提供高效的键盘操作：

*   <kbd>Ctrl</kbd> + <kbd><span>↑</span></kbd> 和 <kbd>Ctrl</kbd> + <kbd><span>↓</span></kbd>：在顺序中向上或向下移动笔记。
*   <kbd>Ctrl</kbd> + <kbd><span>←</span></kbd>：通过将笔记的父级改为其祖父级，在层级中向上移动笔记。
*   <kbd>Ctrl</kbd> + <kbd><span>→</span></kbd>：通过将笔记的父级设置为当前位于其上方的笔记，在层级中向下移动笔记（此操作最好通过演示或实际操作来理解）。
*   <kbd><span>←</span></kbd> 和 <kbd><span>→</span></kbd>：展开和折叠子树。

## 上下文菜单

你也可以使用上下文菜单中熟悉的剪切和粘贴功能来移动笔记，或使用关联的键盘[快捷键（shortcuts）](#root/_help_A9Oc6YKKc65v)：`CTRL-C`（[复制（copy）](#root/_help_IakOLONlIfGI)）、<kbd>Ctrl</kbd> + <kbd>X</kbd>（剪切）和 <kbd>Ctrl</kbd> + <kbd>V</kbd>（粘贴）。

更多信息请参阅<a class="reference-link" href="#root/_help_YtSN43OrfzaA">笔记树上下文菜单（Note tree contextual menu）</a>。

## 树设置

点击树工具栏中的树图标打开树设置弹窗。它包含以下选项：

*   **隐藏归档笔记**：启用后，树中不显示已归档的笔记。
*   **自动折叠笔记**：启用后，笔记在一段时间不活动后会自动折叠，以保持树的整洁。
*   **跟随活动笔记**：启用（默认）后，树会自动滚动并展开父节点，以保持当前活动笔记可见。禁用后，树与导航完全脱离——只更新活动笔记的背景高亮，但树的视口及其展开/折叠状态永远不会因导航而改变。可随时使用十字准星按钮手动将树跳转到活动笔记。

## 键盘快捷键

笔记树带有多个键盘快捷键以加快编辑速度，请参阅专门的<a class="reference-link" href="#root/_help_DvdZhoQZY9Yd">键盘快捷键（Keyboard shortcuts）</a>部分。""",

"NCY5OgXkK1DG": """前端脚本是在客户端（浏览器环境）运行的自定义 JavaScript 笔记。

前端脚本有四种类型：

|  |  |
| --- | --- |
| 常规脚本 | 使用当前应用程序和笔记上下文运行。可以手动运行，也可以在启动时自动运行。 |
| <a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a> | 可以在各种位置引入新的 UI 元素，例如<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>附近、内容区域，甚至<a class="reference-link" href="#root/_help_RnaPdbciOfeq">右侧边栏（Right Sidebar）</a>。 |
| <a class="reference-link" href="#root/_help_4Gn3psZKsfSm">启动栏小组件（Launch Bar Widgets）</a> | 与<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>类似，但专用于<a class="reference-link" href="#root/_help_xYmIYSP6wE3F">启动栏（Launch Bar）</a>。它们可以简单地为启动栏引入新的按钮或图形元素。 |
| <a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a> | 允许在笔记内使用 HTML 或 Preact JSX 渲染自定义内容。 |

对于不需要用户界面的更高级行为（例如批量修改笔记），请参阅<a class="reference-link" href="#root/_help_SPirpZypehBG">后端脚本（Backend scripts）</a>。

## 脚本

脚本没有任何特殊要求。它们可以使用代码笔记上的_执行（Execute）_按钮手动运行，也可以自动运行，请参阅<a class="reference-link" href="#root/_help_irkwjdNtjcxR">事件（Events）</a>。

## 小组件

小组件需要特定格式，Trilium 才能将它们集成到 UI 中。

*   对于传统小组件，脚本笔记必须导出一个 `BasicWidget` 或派生类（请参阅<a class="reference-link" href="#root/_help_GhurYZjh8e1V">感知笔记上下文的小组件（Note context aware widget）</a>或<a class="reference-link" href="#root/_help_M8IppdwVHSjG">右侧面板小组件（Right pane widget）</a>）。
*   对于 Preact 小组件，需要使用名为 `defineWidget` 的内置辅助函数。

更多信息请参阅<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>。

## 脚本 API

Trilium 的前端 API 以全局变量 `api` 的形式提供给所有在前端上下文运行的脚本。关于 API 的参考，请参阅<a class="reference-link" href="#root/_help_Q2z6av6JZVWm">前端 API（Frontend API）</a>。

### 教程

关于构建小组件的更多信息，请查看[小组件基础（Widget Basics）](#root/_help_SynTBQiBsdYJ)。""",

"kwZgWIQRy44x": """你的 Trilium 数据存储在 [SQLite](https://www.sqlite.org) 数据库中，其中包含所有笔记、树结构、元数据以及大部分配置。数据库文件名为 `document.db`，存储在应用程序默认的[数据目录（Data directory）](#root/_help_tAassRL4RSQL)中。

## 演示笔记

首次启动 Trilium 时，它会提供一组笔记来展示应用程序的各种功能。

更多信息请参阅<a class="reference-link" href="#root/_help_6tZeKvSHEUiB">演示笔记（Demo Notes）</a>。

## 手动修改数据库

Trilium 提供了很大的灵活性，也为高级用户提供了调整它的机会。如果你需要直接探索或修改数据库，可以使用 [SQLite Browser](https://sqlitebrowser.org/) 等工具直接操作数据库文件。

更多信息请参阅[手动修改数据库（Manually altering the database）](#root/_help_oyIAJ9PvvwHX)。

## 如何重置数据库

如果你想重新开始并：

*   创建并切换到新的空白知识库
*   使用从服务器或桌面应用程序同步的不同知识库
*   恢复备份

你可以导航到**设置 → 数据库**，然后点击**重新开始（Start over）**按钮。系统会提示你重启应用程序，并可在继续前选择备份当前数据库。

> [!IMPORTANT]
> 对于 Web 客户端，Trilium 无法自动重启。你需要手动重启服务器/容器，然后重新加载 Web 应用程序。

重启后，设置界面会出现。你可以选择备份现有数据库，然后像全新安装一样继续设置菜单。只有在选择创建新数据库、与服务器同步或从备份恢复后，现有数据库才会被覆盖。

### 旧方法

如果你想试验 Trilium 并让它恢复到原始状态，可以通过删除当前数据库来实现。重启应用程序时，它会生成一个包含原始演示笔记的新数据库。

要删除数据库，只需进入[数据目录（data directory）](#root/_help_tAassRL4RSQL)并删除 `document.db` 文件（以及任何以 `document.db` 开头的其他文件）。

如果你不需要保留可能存储在 `config.ini` 文件中的任何配置，可以删除[数据目录（data directory's）](#root/_help_tAassRL4RSQL)的所有内容，以完全恢复应用程序的原始状态。你也可以查看[配置（configuration）](#root/_help_Gzjqa934BdH4)文件，将所有的 `config.ini` 值作为环境变量提供。""",
}
