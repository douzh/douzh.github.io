# -*- coding: utf-8 -*-
TRANS = {
"iPUwA7ypTuJr": """自 v0.101.0 起，Trilium 为前端脚本集成了 Preact，包括对 JSX 的支持。

Preact 可用于：

*   <a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>，使用 JSX 代码笔记而不是 HTML 笔记。
*   <a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>，可以使用 JSX 替换旧的基于 jQuery 的机制。

要开始使用，第一步是在代码语言列表中启用 JSX。转到选项（Options） → 代码笔记（Code Notes），勾选"JSX"语言。然后，继续阅读<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>或<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>中的文档，两者都有关于如何使用新的 Preact 集成的章节。

> [!IMPORTANT]
> 本文档假定你已具备 React 或 Preact 的相关知识。作为入门，可以考虑 [FreeCodeCamp 前端开发库课程](https://www.freecodecamp.org/learn/front-end-development-libraries-v9/)或 [Preact 教程](https://preactjs.com/tutorial/)。

## 导入/导出

将 Preact 与 JSX 一起使用时，有一种特殊语法提供类似 ES 的导入。这种 `import` 语法为更直观、不依赖全局对象的方式铺平了道路，并为未来可能引入的更好的自动补全支持做好准备。

### API 导入

代替：

```jsx
api.showMessage("Hello");
```

JSX 版本如下：

```jsx
import { showMessage } from "trilium:api";
showMessage("hello");
```

### Preact API 导入（hooks、组件）

有一个专门用于 Preact 的新<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>，它提供 Trilium 内部也使用的共享组件以及 hooks 等。

```jsx
import { useState } from "trilium:preact";
const [ myState, setMyState ] = useState("Hi");
```

### 导出

JSX 笔记可以导出一个组件，用于<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>或<a class="reference-link" href="#root/_help_Bqde6BvPo05g">组件库（Component libraries）</a>：

```jsx
export default function() {
    return (
        <>
            <p>Hello world.</p>
        </>
    );
}
```

### 导入/导出不是必需的

这些导入是语法糖，旨在取代 `api` 全局对象的使用（请参阅<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>）。

> [!NOTE]
> `import` 和 `export` 语法仅适用于 JSX 笔记。标准/jQuery 代码笔记仍然需要使用 `api` 全局对象和 `module.exports`。

## 底层机制

与 JavaScript 不同，JSX 需要预处理将其转换为 JavaScript（就像 TypeScript 一样）。为此，Trilium 使用 [Sucrase](https://github.com/alangpierce/sucrase)，这是一个将 JSX 处理为纯 JavaScript 的 JavaScript 库。处理在每次脚本运行时完成（对于小组件，在每次程序启动时进行）。如果你注意到由于编译时间长而导致的性能下降，请考虑[向我们报告问题](#root/_help_wy8So3yZZlH9)。""",

"fEqSpbCn0Ydu": """自 v0.101.0 起，Trilium 为前端脚本集成了 Preact，包括对 JSX 的支持。

Preact 可用于：

*   <a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>，使用 JSX 代码笔记而不是 HTML 笔记。
*   <a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>，可以使用 JSX 替换旧的基于 jQuery 的机制。

要开始使用，第一步是在代码语言列表中启用 JSX。转到选项（Options） → 代码笔记（Code Notes），勾选"JSX"语言。然后，继续阅读<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>或<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>中的文档，两者都有关于如何使用新的 Preact 集成的章节。

> [!IMPORTANT]
> 本文档假定你已具备 React 或 Preact 的相关知识。作为入门，可以考虑 [FreeCodeCamp 前端开发库课程](https://www.freecodecamp.org/learn/front-end-development-libraries-v9/)或 [Preact 教程](https://preactjs.com/tutorial/)。

## 导入/导出

将 Preact 与 JSX 一起使用时，有一种特殊语法提供类似 ES 的导入。这种 `import` 语法为更直观、不依赖全局对象的方式铺平了道路，并为未来可能引入的更好的自动补全支持做好准备。

### API 导入

代替：

```jsx
api.showMessage("Hello");
```

JSX 版本如下：

```jsx
import { showMessage } from "trilium:api";
showMessage("hello");
```

### Preact API 导入（hooks、组件）

有一个专门用于 Preact 的新<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>，它提供 Trilium 内部也使用的共享组件以及 hooks 等。

```jsx
import { useState } from "trilium:preact";
const [ myState, setMyState ] = useState("Hi");
```

### 导出

JSX 笔记可以导出一个组件，用于<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>或<a class="reference-link" href="#root/_help_Bqde6BvPo05g">组件库（Component libraries）</a>：

```jsx
export default function() {
    return (
        <>
            <p>Hello world.</p>
        </>
    );
}
```

### 导入/导出不是必需的

这些导入是语法糖，旨在取代 `api` 全局对象的使用（请参阅<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>）。

> [!NOTE]
> `import` 和 `export` 语法仅适用于 JSX 笔记。标准/jQuery 代码笔记仍然需要使用 `api` 全局对象和 `module.exports`。

## 底层机制

与 JavaScript 不同，JSX 需要预处理将其转换为 JavaScript（就像 TypeScript 一样）。为此，Trilium 使用 [Sucrase](https://github.com/alangpierce/sucrase)，这是一个将 JSX 处理为纯 JavaScript 的 JavaScript 库。处理在每次脚本运行时完成（对于小组件，在每次程序启动时进行）。如果你注意到由于编译时间长而导致的性能下降，请考虑[向我们报告问题](#root/_help_wy8So3yZZlH9)。""",

"Q0A1Gg3pgIOP": """Trilium 的核心功能之一是支持多种笔记类型，具体取决于需求。

## 通过笔记树创建不同类型的新笔记

Trilium 中的默认笔记类型（例如创建新笔记时）是<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>，因为它用于一般用途。

要创建不同类型的新笔记，请转到<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>，右键单击要放置新笔记的现有笔记，然后选择：

*   _在后面插入笔记（Insert note after）_，将新笔记放在所选笔记下方。
*   _插入子笔记（Insert child note）_，将笔记作为所选笔记的子项插入。

![](Note%20Types_image.png)

## 通过添加链接或新标签页创建不同类型的新笔记

*   在<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>笔记中添加[链接](#root/_help_QEAPj01N5f7w)时，输入新笔记所需的标题并按回车。之后会询问笔记的类型。
*   类似地，创建新标签页时，输入所需标题并按回车。

## 更改笔记的类型

可以通过<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>中的_基本属性（Basic Properties）_选项卡更改已创建笔记的类型。请注意，通常只在笔记为空时更改笔记类型才是好主意。也可以用于编辑[笔记的来源（source of a note）](#root/_help_4FahAwuGTAwC)。

## 支持的笔记类型

Trilium 支持以下笔记类型：

| 笔记类型 | 描述 |
| --- | --- |
| <a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a> | 默认笔记类型，支持富文本格式化、图像、提示框和从右到左支持。 |
| <a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a> | 使用等宽字体，可以比文本笔记存储更大块的代码或纯文本，并有更好的语法高亮。 |
| <a class="reference-link" href="#root/_help_m523cpzocqaD">已保存搜索（Saved Search）</a> | 存储搜索信息（搜索文本、条件等）以备后用。例如，可用于快速过滤大量笔记。可以轻松触发搜索。 |
| <a class="reference-link" href="#root/_help_iRwzGnHPzonm">关系图（Relation Map）</a> | 允许轻松创建笔记及它们之间的关系。主要用于关系型数据，例如家谱。 |
| <a class="reference-link" href="#root/_help_bdUJEHsAPYQR">笔记地图（Note Map）</a> | 显示笔记之间的关系，无论是通过关系还是它们的层级结构。 |
| <a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a> | 用于<a class="reference-link" href="#root/_help_CdNpE2pqjmI6">脚本（Scripting）</a>，显示另一个笔记的 HTML 内容。这允许显示任何类型的内容，只要背后有脚本来生成它。 |
| <a class="reference-link" href="#root/_help_GTwFsgaA0lCt">集合（Collections）</a> | 将笔记的子项显示为网格、列表，或者更专门的：日历。          <br>  <br>通常适用于轻松阅读短笔记。 |
| <a class="reference-link" href="#root/_help_s1aBHPd79XYj">Mermaid 图表（Mermaid Diagrams）</a> | 显示条形图、流程图、状态图等图表。需要一些技术知识，因为图表是用专门的格式编写的。 |
| <a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a> | 允许轻松绘制草图、图表、手写内容。使用与 [excalidraw.com](https://excalidraw.com) 相同的技术。 |
| <a class="reference-link" href="#root/_help_1vHRoWCEjj0L">网页视图（Web View）</a> | 显示外部网页的内容，类似于浏览器。 |
| <a class="reference-link" href="#root/_help_gBbsAeiuUxI5">思维导图（Mind Map）</a> | 通过将想法放置在层级布局中，便于头脑风暴。 |
| <a class="reference-link" href="#root/_help_81SGnPGMk7Xc">地理图（Geo Map）</a> | 将笔记的子项显示为地理地图，一个用例是规划假期。它甚至支持基本轨迹。也可以从它创建笔记。 |
| <a class="reference-link" href="#root/_help_W8vYD3Q1zjCR">文件（File）</a> | 表示上传的文件，例如 PDF、图像、视频或音频文件。 |""",

"RsgLJ2Bh2p6v": """## 默认主题

Trilium 附带几个预装的配色主题，默认是浅色主题。要切换到深色主题或任何其他可用主题，请导航到选项（Options）菜单（可通过左上角的应用图标访问），选择外观（Appearance）选项卡，然后选择你偏好的主题。

![深色主题](Themes_dark-theme.png)

## 创建自定义 CSS 主题

Trilium 支持自定义用户主题，允许你个性化应用程序的外观。要创建自定义主题，请按照以下步骤操作：

1.  **创建 CSS 代码笔记**：首先创建一个 `CSS` 类型的[代码笔记](#root/_help_6f9hih2hXXZk)。
2.  **标注** `#appTheme`：给你的笔记添加[属性](#root/_help_zEY4DaJG4YT5) `#appTheme=my-theme-name`，其中 `my-theme-name` 是你自定义主题的名称。
3.  **定义你的样式**：在笔记中编写自定义 CSS。下面是一个自定义主题的示例：

```
@font-face {
  font-family: 'Raleway';
  font-style: normal;
  font-weight: 400;
  src: url('/custom/fonts/raleway.woff2') format('woff2');
}

:root {
    --main-font-family: 'Raleway' !important;
    --main-font-size: normal;
    --tree-font-family: inherit;
    --tree-font-size: normal;
    --detail-font-family: inherit;
    --detail-font-size: normal;
    --detail-text-font-family: 'Garamond' !important;

    --main-background-color: #404552;
    --main-text-color: #AFB8C6;
    --main-border-color: #AFB8C6;
    --accented-background-color: #383C4A;
    --more-accented-background-color: #2F343F;
    --header-background-color: #383C4A;
    --button-background-color: #2F343F;
    --button-disabled-background-color: #404552;
    --button-border-color: #333;
    --button-text-color: #AFB8C6;
    --button-border-radius: 2px;
    --primary-button-background-color: #6c757d;
    --primary-button-text-color: white;
    --primary-button-border-color: #6c757d;
    --muted-text-color: #86919F;
    --input-text-color: #AFB8C6;
    --input-background-color: #404552;
    --hover-item-text-color: white;
    --hover-item-background-color: #4877B1;
    --active-item-text-color: white;
    --active-item-background-color: #4877B1;
    --menu-text-color: #AFB8C6;
    --menu-background-color: #383C4A;
    --tooltip-background-color: #383C4A;
    --link-color: lightskyblue;
    --modal-background-color: #404552;
    --modal-backdrop-color: black;
    --scrollbar-border-color: rgba(175, 184, 198, 0.5);
}

body .note-detail-text {
    font-size: 120%;
}

body .CodeMirror {
    filter: invert(100%) hue-rotate(180deg);
}
```

### 激活你的自定义主题

创建自定义主题后：

1.  转到"菜单" → "选项" → "外观"。
2.  在主题选择下拉列表中，你应该会看到你的自定义主题以你通过 `#appTheme` [标签](#root/_help_zEY4DaJG4YT5)提供的名称列出。
3.  选择你的自定义主题以激活它。

如果你对主题进行了更改，请按 <kbd>Ctrl</kbd> + <kbd>R</kbd> 重新加载前端以应用更新。

### 分享和导入主题

自定义主题可以导出为 `.tar` 归档文件，可以与其他人分享。但是，从不受信任的来源导入主题时要小心，因为它们可能包含可执行脚本，构成安全风险。

一个示例用户主题_钢蓝（Steel Blue）_在演示文档中可用。

![钢蓝主题](Themes_steel-blue.png)

### 将自定义 CSS 用于特定目的

除了完整主题之外，Trilium 还允许使用与主题无关的自定义 CSS。这在脚本环境中特别有用，你可能想修改特定的 UI 元素，例如更改树视图中笔记的颜色。

### 应用自定义 CSS

要使用自定义 CSS：

1.  **创建 CSS 代码笔记**：创建一个 `CSS` 类型的[代码（Code）](#root/_help_6f9hih2hXXZk)笔记。
2.  **添加** `appCss` **标签**：给笔记标注 `#appCss` [标签](#root/_help_zEY4DaJG4YT5)。
3.  **编写你的 CSS**：在笔记中添加自定义 CSS 规则。

例如：

```
/* 自定义 CSS 以样式化特定元素 */
.tree-item {
    color: #ff6347; /* 更改树项目颜色 */
}
```

当 Trilium 前端启动时，所有标注了 `appCss` 的笔记都会自动包含在 HTML 页面的样式元素中。

更改后，按 <kbd>Ctrl</kbd> + <kbd>R</kbd> 重新加载前端以应用你的新样式。

![](Themes_image.png)

### 样式化树中的特定笔记

要对树中的某些笔记应用特定样式：

*   **使用** `cssClass` **属性**：给笔记添加 `cssClass` [属性](#root/_help_zEY4DaJG4YT5)，并为其赋一个表示所需 CSS 类的值。
*   **定义** `iconClass`：你还可以使用 `iconClass` 属性为笔记定义自定义图标，从 [Box Icons](https://boxicons.com) 或你自己的自定义类中选择。

例如，如果你想样式化特定类型的笔记，例如包含 PNG 图像的笔记，你可以使用 `type-image mime-image-png` 等类来定位它们。

### 用户提供的主题

有一个用户创建主题的图库，展示了 Trilium 社区开发的各种自定义。更多信息请查看<a class="reference-link" href="#root/_help_VbjZvtUek0Ln">主题图库（Theme Gallery）</a>。

### 资源路径管理

在自定义主题或 CSS 中引用内置资源（如图像）时，可以通过使用 `vX` 别名来避免硬编码版本号。例如，与其指定 `/assets/v0.57.0-beta/images/icon-grey.png`，你可以使用 `/assets/vX/images/icon-grey.png` 以保持你的主题与 Trilium 未来版本兼容。""",
}
