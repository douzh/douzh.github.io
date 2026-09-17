# -*- coding: utf-8 -*-
TRANS = {
"VEi95epAyiaa": """Trilium 允许你将选定的笔记分享为**可公开访问**的只读文档。此功能对于直接从你的 Trilium 笔记发布内容特别有用，使其他人可以在线访问。

<figure class="image"><img style="aspect-ratio:1144/660;" src="Sharing_image.png" width="1144" height="660"></figure>

## 功能、交互和限制

*   按笔记标题搜索。
*   基于用户浏览器设置的自动深色/浅色模式。
*   移动端友好的布局，带侧边栏。
*   可折叠树，使用与应用程序相同的笔记图标。
*   可自定义的标志。
*   深色/浅色模式切换按钮，也会存储用户偏好。
*   快速导航按钮（上一个和下一个笔记）。
*   显示笔记的最后更新日期。

### 按笔记类型

|  | 支持的功能 | 限制 |
| --- | --- | --- |
| <a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a> | 目录。代码块的语法高亮（前提是选择了语言，如果启用了"自动检测"则不生效）。数学公式的渲染。包含笔记（仅当被包含的笔记也被分享时）。 | 行内 Mermaid 图表不会渲染。 |
| <a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a> | 基本支持（以等宽字体显示笔记内容）。 | 无语法高亮。 |
| <a class="reference-link" href="#root/_help_m523cpzocqaD">已保存搜索（Saved Search）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_iRwzGnHPzonm">关系图（Relation Map）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_bdUJEHsAPYQR">笔记地图（Note Map）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_GTwFsgaA0lCt">集合（Collections）</a> | 子笔记以固定格式显示。 | 更高级的视图类型（如日历视图）不支持。 |
| <a class="reference-link" href="#root/_help_s1aBHPd79XYj">Mermaid 图表（Mermaid Diagrams）</a> | 图表显示为矢量图像。 | 不支持进一步交互。 |
| <a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a> | 图表显示为矢量图像。 | 不支持进一步交互。 |
| <a class="reference-link" href="#root/_help_1vHRoWCEjj0L">网页视图（Web View）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_gBbsAeiuUxI5">思维导图（Mind Map）</a> | 图表显示为矢量图像。 | 不支持进一步交互。 |
| <a class="reference-link" href="#root/_help_81SGnPGMk7Xc">地理图（Geo Map）</a> | 不支持。 |  |
| <a class="reference-link" href="#root/_help_W8vYD3Q1zjCR">文件（File）</a> | 基本交互（下载文件）。 | 不支持进一步交互。 |

虽然分享功能很强大，但它也有一些限制：

*   **代码笔记（Code Notes）**：无语法高亮。
*   **静态笔记树（Static Note Tree）**
*   **受保护的笔记（Protected Notes）**：无法分享。
*   **包含笔记（Include Notes）**：不支持。

其中一些限制可能会在未来的更新中解决。

## 前提条件

要使用分享功能，你必须拥有 Trilium 的<a class="reference-link" href="#root/_help_WOcw2SLH6tbX">服务器安装（Server Installation）</a>。这是必要的，因为笔记将从服务器托管。

## 分享笔记

1.  **启用分享**：要分享笔记，请在笔记界面中切换`共享（Shared）`开关。启用分享后，会出现一个 URL，你可以点击访问分享的笔记。
    
    ![分享笔记](Sharing_share-single-note.png)
2.  **访问分享的笔记**：提供的链接将在你的浏览器中打开笔记。如果你的服务器没有配置公网 IP，URL 将指向 `localhost (127.0.0.1)`。

## 分享笔记子树

当你分享一个笔记时，实际上分享的是它下方的整个笔记子树。如果笔记有子笔记，它们也会包含在分享内容中。例如，分享"格式化"子树将显示一个带有基本导航的页面，用于浏览该子树中的所有笔记。

## 查看和管理分享的笔记

你可以通过点击<a class="reference-link" href="#root/_help_x3i7MxGccDuM">全局菜单（Global menu）</a>中的"显示分享的笔记子树（Show Shared Notes Subtree）"查看所有分享笔记的列表。这允许你管理和浏览所有已公开的笔记。

## 安全注意事项

*   分享的笔记发布在开放的互联网上，任何拥有 URL 的人都可以访问，除非笔记受密码保护。
*   URL 的随机性不提供安全性，因此不要通过此功能分享敏感信息至关重要。
*   Trilium 会采取预防措施保护你公开分享的实例不泄漏非分享笔记的信息，包括为<a class="reference-link" href="#root/_help_wX4HbRucYSDD">数据库（Database）</a>打开单独的只读连接。根据你的威胁模型，使用<a class="reference-link" href="#root/_help_ycBFjKrrwE9p">导出静态 HTML 用于网页发布（Exporting static HTML for web publishing）</a>并使用经过实战检验的 Web 服务器（如 Nginx 或 Apache）提供静态内容可能更有意义。

### 密码保护

要使用用户名和密码保护分享的笔记，可以使用 `#shareCredentials` 属性。将标签以 `#shareCredentials="username:password"` 的格式添加到笔记。要保护整个子树，请确保该标签是[可继承的](#root/_help_bwZpz2ajCEwO)。

## 高级分享选项

### 自定义分享笔记的外观

默认设计应该是一个很好的起点，但你可以使用自己的 CSS 自定义它：

*   **自定义 CSS**：通过向笔记添加 `~shareCss` 关系，将 CSS<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记链接到分享页面。如果希望此样式应用于整个子树，请使标签可继承。可以通过添加 `#shareHiddenFromTree` 标签将 CSS 代码笔记从树导航中隐藏。
*   **省略默认 CSS**：对于大规模样式更改，使用 `#shareOmitDefaultCss` 标签以避免与 Trilium 的[默认样式表](#root/_help_Wy267RK4M69c)冲突。

### 添加 JavaScript

你可以使用 `~shareJs` 关系向分享的笔记注入自定义 JavaScript。这允许你使用 `fetchNote()` API 访问笔记属性或遍历笔记树，该 API 根据其 ID 检索笔记数据。

### 添加自定义 HTML

你可以使用 `~shareHtml` 关系将自定义 HTML 片段注入分享页面的特定位置。HTML 笔记应包含你想要注入的原始 HTML 内容，你可以通过向 HTML 片段笔记本身添加 `#shareHtmlLocation` 标签来控制它出现的位置。

`#shareHtmlLocation` 标签接受 `location:position` 格式的值：

*   **位置（Locations）**：`head`、`body`、`content`
*   **位置点（Positions）**：`start`、`end`

例如：

*   `#shareHtmlLocation=head:start` - 在 `<head>` 部分的开头注入 HTML
*   `#shareHtmlLocation=head:end` - 在 `<head>` 部分的末尾注入 HTML（默认）
*   `#shareHtmlLocation=body:start` - 在 `<body>` 部分的开头注入 HTML
*   `#shareHtmlLocation=content:start` - 在内容区域的开头注入 HTML
*   `#shareHtmlLocation=content:end` - 在内容区域的末尾注入 HTML

如果未指定位置，HTML 将默认注入到 `content:end`。

示例：

```javascript
const currentNote = await fetchNote();
const parentNote = await fetchNote(currentNote.parentNoteIds[0]);

for (const attr of parentNote.attributes) {
    console.log(attr.type, attr.name, attr.value);
}
```

### 自定义分享模板

要完全重新设计分享，可以创建或使用现有的[自定义分享模板](#root/_help_Ad7Ku5gOHrDU)。

### 创建人类可读的 URL 别名

分享的笔记通常具有类似 `http://domain.tld/share/knvU8aJy4dJ7` 的 URL，其中最后一部分是笔记的 ID。你可以通过向单个笔记添加 `#shareAlias` 标签（例如 `#shareAlias=highlighting`）使这些 URL 更友好。这将把 URL 更改为 `http://domain.tld/share/highlighting`。

**重要**：

1.  确保别名是唯一的。
2.  不支持在别名中使用斜杠（`/`）创建子路径。

> [!TIP]
> *   要轻松识别没有分享别名的页面，请使用 `#!shareAlias` 运行<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>。
> *   为了更快地输入分享别名，请考虑使用<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>（例如 `#label:shareAlias(inheritable)="promoted,alias=Slug,single,text"`）。

### 设置自定义网站图标

要为你的分享页面自定义网站图标，请创建指向包含网站图标（例如 `.ico` 格式）的文件笔记的 `~shareFavicon` 关系。

### 将笔记分享为根

你可以通过添加 `#shareRoot` 标签将特定笔记或文件夹指定为分享内容的根。访问 `[http://domain.tld/share](http://domain/share)` 时此笔记将被链接，使 Trilium 更容易用作完整的网站。

> [!TIP]
> 考虑将此与 `#shareIndex` 标签结合使用，该标签将显示所有分享笔记的列表。

### 显示分享笔记的索引

访问分享时，子笔记将显示在左侧的树中。但由于可以分享多个笔记树，显示所有不同分享树的列表可能很有用。

为此，创建一个分享的文本笔记并应用 `shareIndex` 标签。查看时，分享根的列表将显示在笔记底部。

### 链接到外部网站

有时在分享笔记旁边包含指向外部网站的链接很有用——例如在分享导航或索引中。为此，向笔记添加 `#shareExternalLink` 标签，以目标 URL 作为其值（例如 `#shareExternalLink="https://example.com"`）。

任何指向此笔记的链接都会重定向到外部网站并在新的浏览器标签页中打开，而不是打开笔记自己的分享页面。这适用于：

*   `#shareIndex` 标签生成的列表；
*   父笔记下显示的"子页面（Subpages）"列表；
*   其他分享笔记中指向此笔记的行内链接。

URL 必须是绝对的，并包含协议（例如 `https://`）。

> [!NOTE]
> 笔记仍然存在于分享树中，其自己的页面仍然可以通过其直接 URL 访问——该标签只改变指向它的链接的行为。

## 属性参考

| 属性 | 描述 |
| --- | --- |
| `#shareHiddenFromTree` | 此笔记从左侧导航树中隐藏，但仍可通过其 URL 访问 |
| `#shareExternalLink` | 当链接指向此笔记时（从分享索引、子页面列表或其他分享笔记中的行内链接），它会重定向到新标签页中给定的外部 URL，而不是笔记的分享页面。值是绝对 URL，例如 `https://example.com`。 |
| `#shareAlias` | 定义一个别名，笔记将在 `https://your_trilium_host/share/[your_alias]` 下可用 |
| `#shareOmitDefaultCss` | 默认分享页面 CSS 将被省略。在你进行大规模样式更改时使用。 |
| `#shareRoot` | 标记在 /share 根目录下服务的笔记。 |
| `#shareDescription` | 定义要添加到描述 HTML meta 标签的文本 |
| `#shareRaw` | 笔记将以原始格式服务，不带 HTML 包装器。另请参阅<a class="reference-link" href="#root/_help_Qjt68inQ2bRj">直接提供笔记内容（Serving directly the content of a note）</a>了解不设置属性的替代方法。 |
| `#shareDisallowRobotIndexing` | 向网络爬虫指示此笔记的页面不应被索引，方式为：设置 `X-Robots-Tag: noindex` HTTP 头。设置 `noindex, follow` meta 标签。 |
| `#shareCredentials` | 要求凭据才能访问此分享笔记。值的格式应为 `username:password`。别忘了使其可继承以应用于子笔记/图像。 |
| `#shareIndex` | 带有此标签的笔记将列出所有分享笔记的根。 |
| `#shareHtmlLocation` | 定义通过 `~shareHtml` 关系注入的自定义 HTML 应放置的位置。应用于 HTML 片段笔记本身。格式：`location:position`，其中 location 是 `head`、`body` 或 `content`，position 是 `start` 或 `end`。默认为 `content:end`。 |

### 自定义标志

可以调整显示在左侧窗格左上角的标志。

| 属性 | 描述 |
| --- | --- |
| `~shareLogo` | 设置为用作标志的图像的关系。图像必须是分享树的一部分（如果需要可以隐藏）。 |
| `#shareLogoWidth` | 为标志设置的宽度（像素，无单位）。默认为 `53`。 |
| `#shareLogoHeight` | 为标志设置的高度（像素，无单位）。默认为 `40`。 |
| `#shareRootLink` | 按下标志时要导航到的 URL。 |

### 自定义 OpenGraph

| 属性 | 描述 |
| --- | --- |
| `#shareOpenGraphColor` | 调整 `theme-color` meta 属性。 |
| `#shareOpenGraphURL` | 调整 `og:url` 和 `twitter:url` meta 属性。 |
| `#shareOpenGraphDomain` | 调整 `twitter:domain` meta 属性。 |
| `#shareOpenGraphImage`  <br>`~shareOpenGraphImage` | 可以是标签（这种情况下值按原样传递），也可以是到图像<a class="reference-link" href="#root/_help_W8vYD3Q1zjCR">文件（File）</a>的关系。这控制 `og:image` meta 属性。 |

## 致谢

自 v0.95.0 起，引入了一个新主题（并默认启用），它极大地改善了分享功能的视觉外观及其功能（例如移动支持、深色/浅色模式、可折叠树等）。这个主题是 [zerebos](https://github.com/zerebos) 的 [Trilium Rocks!](https://github.com/zerebos/trilium.rocks) 的改编。""",
}
