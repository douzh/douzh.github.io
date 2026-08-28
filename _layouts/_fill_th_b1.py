# -*- coding: utf-8 -*-
import sys, json
sys.path.insert(0, r'D:\mycloud\0pnbase\kb-main\_layouts')
from _mcp import set_note

MP = json.load(open(r'D:\mycloud\0pnbase\kb-main\_layouts\help_map.json', encoding='utf-8'))['node_map']
C = {}

C['_help_7NfNr5pZpVKV'] = """## Step 1. Find a place to place the themes
## 第 1 步：为主题找一个放置位置

Organization is an important aspect of managing a knowledge base. When developing a new theme or importing an existing one it's a good idea to keep them into one place.
组织化管理是管理知识库的一个重要方面。在开发新主题或导入现有主题时，最好将它们放在一个地方。

As such, the first step is to create a new note to gather all the themes.
因此，第一步是创建一个新笔记来汇集所有主题。

![](Creating%20a%20custom%20theme_5_Creating%20a%20custom%20theme_im.png)
（图片：主题文件夹）

## Step 2. Create the theme
## 第 2 步：创建主题

|  |  |
| --- | --- |
| ![](Creating%20a%20custom%20theme_3_Creating%20a%20custom%20theme_im.png) | Themes are code notes with a special attribute. Start by creating a new code note.<br>主题是带有特殊属性的代码笔记。首先创建一个新的代码笔记。 |
| ![](Creating%20a%20custom%20theme_1_Creating%20a%20custom%20theme_im.png) | Then change the note type to a CSS code.<br>然后将笔记类型更改为 CSS 代码。 |
| ![](Creating%20a%20custom%20theme_Creating%20a%20custom%20theme_im.png) | In the _Owned Attributes_ section define the `#appTheme` attribute to point to any desired name. This is the name that will show up in the appearance section in settings.<br>在_自有属性（Owned Attributes）_部分定义 `#appTheme` 属性以指向任何所需的名称。这是将显示在设置中的外观部分里的名称。 |

## Step 3. Define the theme's CSS
## 第 3 步：定义主题的 CSS

As a very simple example we will change the background color of the launcher pane to a shade of blue.
作为一个非常简单的示例，我们将把启动器面板的背景颜色改为蓝色调。

To alter the different variables of the theme:
要更改主题的不同变量：

```css
:root {
	--launcher-pane-background-color: #0d6efd;
}
```

## Step 4. Activating the theme
## 第 4 步：激活主题

Refresh the application (Ctrl+Shift+R is a good way to do so) and go to settings. You should see the newly created theme:
刷新应用程序（Ctrl+Shift+R 是一个好方法）并转到设置。你应该会看到新创建的主题：

![](Creating%20a%20custom%20theme_2_Creating%20a%20custom%20theme_im.png)
（图片：设置中的主题）

Afterwards the application will refresh itself with the new theme:
之后，应用程序将用新主题自行刷新：

![](Creating%20a%20custom%20theme_4_Creating%20a%20custom%20theme_im.png)
（图片：应用新主题）

Do note that the theme will be based off of the legacy theme. To override that and base the theme on the new TriliumNext theme, see: [Theme base (legacy vs. next)](#root/_help_WFGzWeUK6arS)
请注意，主题将基于旧版（legacy）主题。要覆盖这一点并使主题基于新的 TriliumNext 主题，请参阅：[主题基础（旧版 vs. 新版）](#root/_help_WFGzWeUK6arS)

## Step 5. Making changes
## 第 5 步：进行更改

Simply go back to the note and change according to needs. To apply the changes to the current window, press <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>R</kbd> to refresh.
只需回到笔记并根据需要进行更改。要将更改应用到当前窗口，请按 <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>R</kbd> 刷新。

It's a good idea to keep two windows, one for editing and the other one for previewing the changes.
保持两个窗口是一个好主意，一个用于编辑，另一个用于预览更改。"""

C['_help_WFGzWeUK6arS'] = """By default, any custom theme will be based on the legacy light theme. To use the TriliumNext theme instead, add the `#appThemeBase=next` attribute onto the existing theme. The `appTheme` attribute must also be present.
默认情况下，任何自定义主题都基于旧版浅色主题。要改用 TriliumNext 主题，请在现有主题上添加 `#appThemeBase=next` 属性。`appTheme` 属性也必须存在。

![](Customize%20the%20Next%20theme_image.png)
（图片：设置 Next 主题基础）

The `appThemeBase` label can be set to one of the following values:
`appThemeBase` 标签可以设置为以下值之一：

*   `next`, for the TriliumNext (auto light or dark mode).
*   `next-light`, for the always light mode of the TriliumNext.
*   `next-dark`, for the always dark mode of the TriliumNext.
*   Any other value is ignored and will use the legacy white theme instead.
*   `next`，用于 TriliumNext（自动浅色或深色模式）。
*   `next-light`，用于 TriliumNext 的始终浅色模式。
*   `next-dark`，用于 TriliumNext 的始终深色模式。
*   任何其他值将被忽略，并将使用旧版白色主题代替。

## Overrides
## 覆盖

Do note that the TriliumNext theme has a few more overrides than the legacy theme. Due to that, it is recommended to use `#trilium-app` with a next theme instead of the `:root` of a legacy theme.
请注意，TriliumNext 主题比旧版主题有更多的覆盖项。因此，建议在 next 主题中使用 `#trilium-app` 而不是旧版主题的 `:root`。

```css
#trilium-app {
	--launcher-pane-background-color: #0d6efd;
}
```"""

C['_help_AlhDUqhENtH7'] = """It is possible to provide a CSS file to be used regardless of the theme set by the user.
可以提供一份不受用户设置的主题影响、始终生效的 CSS 文件。

|  |  |
| --- | --- |
| ![](Custom%20app-wide%20CSS_image.png) | Start by creating a new note and changing the note type to CSS<br>首先创建一个新笔记并将笔记类型更改为 CSS |
| ![](2_Custom%20app-wide%20CSS_image.png) | In the ribbon, press the “Owned Attributes” section and type `#appCss`.<br>在功能区中，按 “自有属性（Owned Attributes）” 部分并输入 `#appCss`。 |
| ![](3_Custom%20app-wide%20CSS_image.png) | Type the desired CSS.   <br>  <br>Generally it's a good idea to append `!important` for the styles that are being changed, in order to prevent other<br>输入所需的 CSS。   <br>  <br>一般来说，对于正在更改的样式，追加 `!important` 是一个好主意，以防止其他样式覆盖它们 |

## Seeing the changes
## 查看更改

Adding a new _app CSS note_ or modifying an existing one does not immediately apply changes. To see the changes, press Ctrl+Shift+R to refresh the page first.
添加新的_应用 CSS 笔记（app CSS note）_或修改现有的笔记不会立即应用更改。要查看更改，请先按 Ctrl+Shift+R 刷新页面。

## Sample use cases
## 示例用例

### Customizing the printing stylesheet
### 自定义打印样式表

> [!TIP]
> Since v0.99.2, it's no longer possible to use `#appCss` to customize the printing CSS, since the printing is now done in an isolated environment.
> 
> However, it's still possible to customize the CSS via `~printCss`; see <a class="reference-link" href="#root/_help_NRnIZmSMc5si">Printing &amp; Exporting as PDF</a> for more information.
> 提示：自 v0.99.2 起，不再可能使用 `#appCss` 来自定义打印 CSS，因为打印现在在隔离环境中完成。
> 
> 然而，仍然可以通过 `~printCss` 自定义 CSS；有关更多信息，请参阅<a class="reference-link" href="#root/_help_NRnIZmSMc5si">打印与导出为 PDF（Printing &amp; Exporting as PDF）</a>。

### Per-workspace styles
### 按工作区设置样式

When using <a class="reference-link" href="#root/_help_9sRHySam5fXb">Workspaces</a>, it can be helpful to create a visual distinction between notes in different workspaces.
使用<a class="reference-link" href="#root/_help_9sRHySam5fXb">工作区（Workspaces）</a>时，在不同工作区的笔记之间创建视觉区分会很有帮助。

To do so:
要做到这一点：

1.  In the note with `#workspace`, add an inheritable attribute `#cssClass(inheritable)` with a value that uniquely identifies the workspace (say `my-workspace`).
2.  Anywhere in the note structure, create a CSS note with `#appCss`.
1.  在带有 `#workspace` 的笔记中，添加一个可继承的属性 `#cssClass(inheritable)`，其值唯一标识该工作区（比如 `my-workspace`）。
2.  在笔记结构中的任何位置，创建一个带有 `#appCss` 的 CSS 笔记。

#### Change the color of the icons in the <a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">Note Tree</a>
#### 更改<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>中图标的颜色

```css
.fancytree-node.my-workspace.fancytree-custom-icon {
    color: #ff0000;
}
```

#### Change the color of the note title and the icon
#### 更改笔记标题和图标的颜色

To change the color of the note title and the icon (above the content):
要更改笔记标题和图标的颜色（在内容上方）：

```css
.note-split.my-workspace .note-icon-widget button.note-icon,
.note-split.my-workspace .note-title-widget input.note-title {
    color: #ff0000;
}
```

#### Add a watermark to the note content
#### 为笔记内容添加水印

<figure class="image image-style-align-right image_resized" style="width:39.97%;"><img style="aspect-ratio:641/630;" src="1_Custom app-wide CSS_image.png" width="641" height="630"></figure>
（图片：水印示例）

1.  Insert an image in any note and take the URL of the image.
2.  Use the following CSS, adjusting the `background-image` and `width`and `height` to the desired values.
1.  在任意笔记中插入一张图像并获取图像的 URL。
2.  使用以下 CSS，将 `background-image` 以及 `width` 和 `height` 调整为所需的值。

```css
.note-split.my-workspace .scrolling-container:after {
    position: fixed;
    content: "";
    background-image: url("/api/attachments/Rvm3zJNITQI1/image/logo.png");
    background-size: contain;
    background-position: center;
    background-repeat: no-repeat;
    width: 237px;
    height: 44px;
    bottom: 1em;
    right: 1em;
    opacity: 0.5;
    z-index: 0;
}
```

## Limitations
## 限制

Some parts of the application can't be styled directly via custom CSS because they are rendered in an isolated mode (shadow DOM), more specifically:
应用程序的某些部分无法直接通过自定义 CSS 设置样式，因为它们在隔离模式（影子 DOM）中渲染，具体来说：

*   The slides in a <a class="reference-link" href="#root/_help_zP3PMqaG71Ct">Presentation</a>.
*   <a class="reference-link" href="#root/_help_zP3PMqaG71Ct">演示文稿（Presentation）</a>中的幻灯片。"""

for src, content in C.items():
    nid = MP.get(src)
    if not nid:
        print(src, 'NO MAPPING')
        continue
    r = set_note(nid, content)
    print(src, nid, '->', r[:80])
