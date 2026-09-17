# -*- coding: utf-8 -*-
TRANS = {
"Y5pCr7P1spvA": """<figure class="image image-style-align-center image_resized" style="width:76.29%;"><img style="aspect-ratio:1479/865;" src="In-editor AI assistant_image.png" width="1479" height="865"></figure>

AI 助手作用于你正在编写的文本：选择一段文字，让它重写、翻译或转为表格，或者在光标处请求新内容。与 [AI 聊天](#root/_help_LrKznOtw9cQD)不同，它不会打开对话窗口，结果会就地预览，只有在你接受后才会进入笔记。

## 要求

只有在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _AI / LLM_ 中至少配置了一个提供商并为其选择了至少一个模型后，助手才可用。在此之前，工具栏条目完全不会显示。

## 访问 AI 助手

要访问 AI 助手：

*   在<a class="reference-link" href="#root/_help_nRhnJkTT8cPs">格式化工具栏（Formatting toolbar）</a>中查找<img class="image_resized" style="aspect-ratio:150/150;width:3.16%;" src="In-editor AI assistant_ai.svg" width="150" height="150">按钮。按按钮本身进入_询问 AI（Ask AI）_模式，或按其旁边的箭头键访问快速命令和模型选择。
*   在<a class="reference-link" href="#root/_help_ZlN4nump6EbW">斜杠命令（Slash Commands）</a>中查找_AI 助手（AI assistant）_或任何以 _AI_ 为前缀的快速命令。
*   按 <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>K</kbd>。
*   在桌面应用中，右键单击文本并选择_AI 助手（AI assistant）_选项。

## 助手处理什么内容

| 情况 | 发送什么 |
| --- | --- |
| 选中了文本 | 选中内容。 |
| 未选中任何内容，并且你选择了一个快速操作 | 光标所在的段落。 |
| 未选中任何内容，并且你输入自己的提示 | 什么也不发送，响应是在光标处写入的新内容。 |

在所有情况下，助手还会发送目标前后（target 周围）的文本，以便模型能够判断它正在写入什么：当要求_继续写作（Continue writing）_或总结时，只能看到所选单词的模型是在猜测它们周围的文档。这些周围文本只是上下文，永远不会被重写。

> [!IMPORTANT]
> 这意味着单次运行发送的内容比你高亮的段落要多。请参阅<a class="reference-link" href="#root/_help_cRRrdZjgyhNc">隐私（Privacy）</a>了解到底有什么离开你的机器。

## 快速操作

工具栏按钮旁边的箭头会打开一个预置指令菜单，按功能分组。

*   _重新格式化（Reformat）_操作产生真正的 Trilium 内容而不是纯文本：_图表（Diagram）_插入 Mermaid 图表，_标注（Callout）_插入提示框，_可折叠（Collapsible）_插入可折叠块。
*   _翻译（Translate）_组列出你启用的[内容语言](#root/_help_veGu4faJErEM)，如果未设置内容语言则列出预定义列表。可以从编辑器内部填写，子菜单的最后一项直接打开语言配置。

需要处理对象的操作在没有可处理对象时会变灰（例如没有选择的空段落）。

## 请求特定内容

选择_询问 AI…（Ask AI…）_会打开一个提示框，你可以在其中用自己的话描述更改，例如："让这个不那么正式"、"为 2024 添加一行"、"为这一节写一个引言段落"。按 <kbd>Enter</kbd> 或发送按钮运行它。

一旦响应到达，你可以在同一个提示框中继续，助手会将其视为对话而不是新请求：在_翻译成德语_之后，要求_让它更短_会缩短德语版本，而不是原始文本。

## 审查结果

响应会流式生成，可以随时按_停止（Stop）_按钮中断；在此之前到达的内容仍然可用。

在你明确表示之前，不会向笔记写入任何内容。审查提供：

*   **结果（Result）** 显示将要插入的响应。
*   **更改（Changes）** 显示与原始文本的内联差异，标记插入和删除。当模型重写了段落而不是编辑它时，审查会在_结果（Result）_上打开：两个共同点很少的文本之间的差异比答案本身更难阅读。
*   **重试（Try again）**，重新运行相同的指令。
*   **替换（Replace）**，替换原始段落。
*   **在下方插入（Insert below）**，保留原始文本并在其后添加响应。

Trilium 还会报告生成它的模型、消耗的 token，以及对于报告该信息的提供商，报告价格。

如果你要求更正但没有可更正的内容，助手会指示这一点，而不是显示空白差异。

## 选择模型

快速操作菜单底部显示助手运行的模型，并允许你从在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _AI / LLM_ 中选择的模型中选择另一个，按提供商分组。

也可以在请求特定内容时或助手生成响应后更改模型（例如，如果响应需要更好的模型）。

这个选择与聊天使用的模型是分开的，并且会跨笔记和设备记住。如果未设置，助手会跟随第一个配置的提供商。""",

"onXZFHEZNtWj": """自定义小组件是脚本的一个特殊子集，在应用程序的特定部分渲染图形元素。它们可用于向 Trilium 应用程序添加新功能。

## Preact with JSX vs. 原生 jQuery

在旧版 Trilium 中，自定义小组件完全使用 jQuery 与 Trilium 的内部小组件架构（例如 `BasicWidget`、`NoteContextAwareWidget`）组合编写。

从 v0.101.0 开始，自定义小组件也可以使用<a class="reference-link" href="#root/_help_KLsqhjaqh1QW">Preact</a>框架的 JSX 编写。旧版和 Preact 小组件具有相同的能力，只有一个区别：

*   Preact 小组件默认是内容大小的，而旧版小组件需要在构造函数中应用 `this.contentSized()`。更多信息请参阅<a class="reference-link" href="#root/_help_gMkgcLJ6jBkg">故障排除（Troubleshooting）</a>中的相应章节。

只要可能，小组件示例将同时以旧版和 Preact 格式提供。

## 创建自定义小组件

1.  创建一个[代码（Code）](#root/_help_6f9hih2hXXZk)笔记。
2.  将语言设置为：
    1.  旧版使用 jQuery 的小组件选择 JavaScript (frontend)。
    2.  Preact 小组件选择 JSX。你可能需要先转到选项（Options） → 代码（Code）启用该语言。
3.  应用 `#widget` [标签](#root/_help_HI6GBBIduIgv)。

## 从一个简单示例开始

让我们从创建一个在内容区域附近显示消息的小组件开始。按照上一节创建代码笔记，并使用以下内容。

### 旧版（jQuery）

```
class HelloCenterPane extends api.BasicWidget {

    constructor() {
        super();
        this.contentSized();
    }

    get parentWidget() { return "center-pane" }

    doRender() {
        this.$widget = $("<span>Center pane</span>");
    }
    
}

module.exports = new HelloCenterPane();
```

[刷新应用程序](#root/_help_s8alTXmpFR61)，小组件应该出现在内容区域下方。

### Preact 版本

```
import { defineWidget } from "trilium:preact";

export default defineWidget({
    parent: "center-pane",
    render: () => <span>Center pane from Preact.</span>
});
```

[刷新应用程序](#root/_help_s8alTXmpFR61)，小组件应该出现在内容区域下方。

## 小组件位置（父小组件）

小组件可以放置在应用程序的以下区域之一：

| `parentWidget` 的值 | 描述 | 示例小组件 | 特殊要求 |
| --- | --- | --- | --- |
| `left-pane` | 出现在容纳<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>的同一个窗格中。 | 与上面相同，只是 `parentWidget` 不同。 | 无。 |
| `center-pane` | 在内容区域。如果打开了分屏，小组件将跨越所有分屏。 | 参见上面的示例。 | 无。 |
| `note-detail-pane` | 在内容区域中，位于笔记详情区域内。如果打开了分屏，小组件将包含在分屏内。如果小组件是笔记特定的，这是理想的。 | <a class="reference-link" href="#root/_help_GhurYZjh8e1V">笔记上下文感知小组件（Note context aware widget）</a> | 小组件必须导出 `class` 而不是类的实例（例如 `no new`），因为它需要为每个笔记复制，以便分屏正常工作。由于导出的是 `class` 而不是实例，`parentWidget` getter 必须是 `static`，否则小组件会被忽略。 |
| `right-pane` | 在<a class="reference-link" href="#root/_help_RnaPdbciOfeq">右侧边栏（Right Sidebar）</a>中，作为一个专用部分。 | <a class="reference-link" href="#root/_help_M8IppdwVHSjG">右侧窗格小组件（Right pane widget）</a> | 虽然不是强制性的，但最好使用 `RightPanelWidget` 而不是 `BasicWidget` 或 `NoteContextAwareWidget`。 |

要将小组件放置在其他位置，只需更改传给旧版小组件的 `get parentWidget()` 或 Preact 的 `parent` 字段的值。请注意，某些位置（如 `note-detail-pane` 和 `right-pane`）有需要考虑的特殊要求（请参阅上面的表格）。

## 启动栏小组件

启动栏小组件类似于_自定义小组件_，但特定于<a class="reference-link" href="#root/_help_xYmIYSP6wE3F">启动栏（Launch Bar）</a>。更多信息请参阅<a class="reference-link" href="#root/_help_4Gn3psZKsfSm">启动栏小组件（Launch Bar Widgets）</a>。

## 自定义位置

自定义小组件的位置通过 `position` 整数定义。

在旧版小组件中：

```
class MyWidget extends api.BasicWidget {
	// [..
	get position() { return 10; }
}
```

在 Preact 小组件中：

```
export default defineWidget({
    // [...]
    position: 10
});
```""",
}
