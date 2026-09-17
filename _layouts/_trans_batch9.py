# -*- coding: utf-8 -*-
TRANS = {
"O4yjkaVH3YxV": """Trilium 通常通过**反向代理**暴露到互联网，反向代理负责 TLS 终止、提供单一公共入口点，并且（可选）允许你在一个域名下托管多个实例。Trilium 本身在其本地端口上以纯 HTTP 运行；代理位于其前面。

本节收集了特定提供商的指南以及无论使用哪个代理都适用的配置。

## 提供商指南

*   <a class="reference-link" href="#root/_help_ud6MShXL4WpO">Nginx</a>
*   <a class="reference-link" href="#root/_help_5ERVJb9s4FRD">Traefik</a>
*   <a class="reference-link" href="#root/_help_fDLvzOx29Pfg">使用 Docker 的 Apache</a>

## 适用于每个代理

无论你选择哪个代理，请确保以下事项：

*   **转发 WebSocket 升级。** Trilium 使用 WebSocket 进行同步和实时 UI 更新。你的代理必须传递 `Upgrade` 和 `Connection` 头，否则客户端看起来会加载，但永远不会收到更新。
*   **配置受信任的代理。** 以便 Trilium 看到真实的客户端 IP（用于速率限制、日志记录和安全 cookie），而不是代理的地址。请参阅<a class="reference-link" href="#root/_help_LLzSMXACKhUs">受信任的代理（Trusted proxy）</a>。
*   **在代理处终止 TLS。** 请参阅<a class="reference-link" href="#root/_help_l2VkvOwUNfZj">HTTPS (TLS)</a>。

## 在 Trilium 前面放置身份验证代理

某些设置会在代理处放置一个**前向认证 / SSO 层**（Authelia、Authentik、oauth2-proxy 或 OIDC 插件），以便_每个_请求都必须先通过交互式登录。

这会**破坏同步和初始设置。** 同步是一个机器对机器的过程，在后台无人值守地运行——它没有浏览器，无法完成交互式登录挑战。当代理拦截同步请求时，它返回的是 HTML 登录页面而不是预期的 JSON，客户端会以类似错误失败：

```
同步失败：请求 GET https://trilium.example.com/api/setup/status 失败，
错误：意外的标记 '<'，'<!doctype "... 不是有效的 JSON

```

有两种方法可以解决这个问题：

*   **首选，使用 Trilium 的内置身份验证。** 根本不要在代理处对 Trilium 设置网关。Trilium 可以自己认证 Web 界面，包括通过 [OpenID Connect](#root/_help_x1lQNKQBqLcA)。这使同步端点可以按设计的方式进行认证。
*   **将同步端点从代理的身份验证中豁免。** 如果必须将认证保留在代理处，请从前向认证检查中排除这些路径：
    *   `/api/setup/*`
    *   `/api/login/sync`
    *   `/api/sync/*`

> [!NOTE]
> 这是安全的：Trilium 自己认证这些端点。同步使用基于共享密钥的无挑战 HMAC 握手，一次性设置种子请求受密码保护——因此在代理处豁免它们不会暴露对你的数据的未认证访问。

一个 Nginx 示例，其中 `/auth` 是你的前向认证端点：

```
server {
    # ... TLS、server_name 等 ...

    # 同步和设置端点自行认证——跳过前向认证。
    location ~ ^/api/(sync|login/sync|setup) {
        auth_request off;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://trilium;
    }

    # 所有其他请求都经过前向认证。
    location / {
        auth_request /auth;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://trilium;
    }
}
```""",

"KULw53GiQ4vA": """<figure class="image"><img style="aspect-ratio:1071/146;" src="Attributes_image.png" width="1071" height="146"></figure>

在 Trilium 中，属性是分配给笔记的键值对，提供额外的元数据或功能。有两种主要类型的属性：

1.  <a class="reference-link" href="#root/_help_HI6GBBIduIgv">标签（Labels）</a>可用于多种用途，例如存储元数据或配置笔记的行为。标签也是可搜索的，增强了笔记检索。
    
    更多信息，包括预定义标签，请参阅<a class="reference-link" href="#root/_help_HI6GBBIduIgv">标签（Labels）</a>。
2.  <a class="reference-link" href="#root/_help_Cq5X6iKQop6R">关系（Relations）</a>定义笔记之间的连接，类似于链接。这些可用于元数据和脚本目的。
    
    更多信息，包括预定义关系列表，请参阅<a class="reference-link" href="#root/_help_Cq5X6iKQop6R">关系（Relations）</a>。

这些属性在组织、分类和增强笔记功能方面发挥着至关重要的作用。

## 属性类型

从概念上讲，有两种类型的属性（同时适用于标签和关系）：

1.  **系统属性（System attributes）**  
    顾名思义，这些属性具有特殊含义，因为它们会被 Trilium 解释。例如 `color` 属性会改变笔记在<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>和链接中显示的颜色，`iconClass` 会改变笔记的图标。
2.  **用户定义属性（User-defined attributes）**  
    这些是用户可以使用的自由形式的标签或关系。它们可以纯粹用于分类目的（特别是与<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>结合时），也可以通过<a class="reference-link" href="#root/_help_CdNpE2pqjmI6">脚本（Scripting）</a>赋予意义。

在实践中，Trilium 不会直接区分属性是系统的还是用户定义的。如果标签或关系与内置名称之一匹配（例如前面提到的 `iconClass`），则被视为系统属性。在创建<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>时请记住这一点，以免意外更改系统属性（除非有意为之）。

## 查看属性列表

当前笔记的标签和关系都显示在<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>的_拥有的属性（Owned Attributes）_部分中，可以在其中查看和编辑。继承的属性显示在功能区的_继承的属性（Inherited Attributes）_部分中，在那里只能查看。

在属性列表中，标签以 `#` 字符为前缀，而关系以 `~` 字符为前缀。

## 属性定义和提升属性

<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>为属性创建类似表单的编辑体验，使属性的组织和管理更加容易。

## 多重性

Trilium 中的属性可以是"多值的"，意味着多个同名属性可以共存。这可以与<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>结合使用以轻松添加它们。

## 属性继承

Trilium 支持属性继承，允许子笔记从父笔记继承属性。更多信息请参阅<a class="reference-link" href="#root/_help_bwZpz2ajCEwO">属性继承（Attribute Inheritance）</a>。""",

"I1BPKT4HF7wh": """> [!TIP]
> 要快速理解 Mermaid 语法，请参阅<a class="reference-link" href="#root/_help_WWgeUaBb7UfC">语法参考（Syntax reference）</a>（官方文档）。

<figure class="image image-style-align-center"><img style="aspect-ratio:886/663;" src="2_Mermaid Diagrams_image.png" width="886" height="663"></figure>

Trilium 支持 Mermaid，它支持各种图表，如流程图、时序图、类图、状态图、饼图等，全部使用图表的文本描述，而不是手动绘制图表。

这种笔记类型是分屏视图，意味着源代码和文档预览并排显示。更多信息请参阅<a class="reference-link" href="#root/_help_SL5f1Auq7sVN">分屏视图的笔记类型（Note types with split view）</a>。

## 示例图表

从 v0.103.0 开始，Mermaid 图表不再以示例流程图开始，而是底部的窗格会显示所有支持的图表，每个图表都带有示例代码：

*   只需点击任何示例即可应用它。
*   一旦在代码编辑器中输入内容或选择了示例，该窗格就会消失。要让它再次出现，只需清空笔记的内容。

## 布局

根据正在编辑的图表和用户偏好，Mermaid 笔记类型支持两种布局：

*   水平布局，源代码（可编辑部分）在屏幕左侧，预览在右侧。
*   垂直布局，源代码在屏幕底部，预览在顶部。

可以随时通过按<a class="reference-link" href="#root/_help_XpOYSgsLkTJy">浮动按钮（Floating buttons）</a>区域中的![](Mermaid%20Diagrams_image.png)图标在两种布局之间切换。

## 交互

*   图表的源代码（Mermaid 格式）显示在笔记的左侧或底部（取决于布局）。
    *   更改图表代码会自动刷新图表。
*   图表的预览显示在笔记的右侧或顶部（取决于布局）：
    *   预览右下角有专门的按钮来控制缩放、缩小或居中图表：![](1_Mermaid%20Diagrams_image.png)
    *   按住鼠标左键并拖动可以移动预览。
    *   也可以使用滚轮进行缩放。
    *   图表变化时，预览上的缩放和位置将保持固定，以便更轻松地处理大型图表。
*   可以通过将鼠标悬停在源代码/预览窗格之间的边框上并拖动来调整它们的大小。
*   在<a class="reference-link" href="#root/_help_XpOYSgsLkTJy">浮动按钮（Floating buttons）</a>区域：
    *   可以通过_将编辑窗格移动到左侧 / 底部_选项将源代码/预览从左-右或底-顶布局排列。
    *   按_锁定编辑（Lock editing）_会自动将笔记标记为只读。在此模式下，代码窗格被隐藏，图表全尺寸显示。同样，按_解锁编辑（Unlock editing）_会将只读笔记标记为可编辑。
    *   按_将图像引用复制到剪贴板_可以将图表的图像表示插入到文本笔记中。更多信息请参阅<a class="reference-link" href="#root/_help_0Ofbk1aSuVRu">图像引用（Image references）</a>。
    *   按_将图表导出为 SVG_可以下载图表的可缩放/矢量渲染。可以用于在缩放时不会失真的图表展示。
    *   按_将图表导出为 PNG_可以下载图表的普通图像（1 倍缩放、光栅）。可以用于通过电子邮件等更传统的渠道发送图表。

## 图表中的错误

如果源代码中有错误，错误将显示在信息窗格中。

在错误状态下，图表将不再渲染，之前可用的图表将保留在预览部分。""",

"ylo1oZtjQP5B": """Trilium 支持存储和显示图像。支持的格式有 PNG、JPEG、GIF、BMP、WebP、AVIF 和 SVG。

图像可以作为笔记的[附件](#root/_help_0vhv7lsOLy82)上传，也可以作为独立的[笔记](#root/_help_kBrnXNG3Hplm)放入[笔记树](#root/_help_kBrnXNG3Hplm)中。其引用可以复制到文本笔记中，以便在文本中显示。

## 上传图像

要向笔记添加图像：

*   只需将其从文件资源管理器拖到 Trilium 内的笔记编辑器上，图像就会被上传。
*   或者，在<a class="reference-link" href="#root/_help_nRhnJkTT8cPs">格式化工具栏（Formatting toolbar）</a>中查找_插入图像（Insert image）_图标。
*   你也可以从网页复制并粘贴图像（请参阅下面的章节）。

## 剪贴板与图像自动下载

Trilium 对复制到剪贴板和从剪贴板粘贴的图像有特殊处理。

*   对于文本和图像的混合，图像会由服务器（或桌面应用，取决于正在使用什么）自动下载。
    
    *   这意味着图像必须可公开访问，并且可以从 Trilium 运行的位置访问。无法访问的图像最终会变成损坏的图像。
*   如果将单个图像粘贴到 Trilium，它将优先使用剪贴板中的图像。这使得可以复制 Trilium 否则无法访问的图像，例如 Google Chat、Slack 等。
*   从 Trilium 复制带图像的文本并粘贴到另一个应用（如 Microsoft Word 或 LibreOffice Writer）时，图像将被保留。

图像的自动下载默认启用，可以从<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _媒体（Media）_ → _自动下载图像（Download images automatically）_进行切换。

## 配置图像

单击图像会弹出一个包含多个选项的弹出窗口：  
![](7_Images_image.png)

### 对齐

第一组选项配置对齐方式，按顺序为：

| 图标 | 选项 | 预览 | 描述 |
| --- | --- | --- | --- |
| ![](5_Images_image.png) | 行内 | ![](1_Images_image.png) | 顾名思义，可以将图像放在段落内，并像文本块一样移动。使用拖放或剪切粘贴来移动它。 |
| ![](8_Images_image.png) | 居中图像 | ![](2_Images_image.png) | 图像将作为块显示并居中，不允许在其左侧或右侧有文本。 |
| ![](4_Images_image.png) | 文本环绕 | ![](6_Images_image.png) | 图像将显示在文本的左侧或右侧。 |
| ![](Images_image.png) | 块对齐 | ![](3_Images_image.png) | 与_居中图像_类似，图像将作为块显示，并左对齐或右对齐，但不允许文本在其两侧流动。 |

## 压缩

由于 Trilium 并不是真正的图像数据的主要存储，它会在将上传的图像存储到数据库之前尝试压缩和调整大小（使用相当激进的设置）。你可能会注意到一些质量下降。基本质量设置可在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _媒体（Media）_中找到。

如果想以原始分辨率保存图像，建议将它们作为笔记的附件保存（在<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a> → _导入文件（Import files）_中查找上下文菜单）。

## 并排对齐图像

通常有两种方法并排显示图像：

*   如果它们大小大致相同，只需根据上面的对齐部分将两个图像设为行内。图像可以拖放到同一行。
*   如果它们大小不同，创建一个带不可见边框的[表格](#root/_help_NdowYOC1GFKS)。""",

"gBJUFdOLz9GA": """Trilium 支持创建<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记，即允许你存储一些编程代码并高亮显示的笔记。特殊情况是 JavaScript 代码笔记，它们也可以在 Trilium 内执行，与<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>结合可以提供额外功能。

## 架构概述

要进一步了解，我必须解释 Trilium 的基本架构——本质上它是一个经典的 Web 应用——它有两个主要组件：

*   在浏览器中运行的前端（使用 HTML、CSS、JavaScript）——主要用于与用户交互、显示笔记等。
*   在 node.js 运行时中运行 JavaScript 代码的后端——负责存储笔记、加密笔记等。

所以我们有前端和后端，每个都有各自的责任，但它们的共同特点是都运行 JavaScript 代码。再加上我们能创建 JavaScript<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记这一事实，就有意思了。

## 用例

*   <a class="reference-link" href="#root/_help_TjLYAo3JMO8X">"新任务（New Task）"启动器按钮</a>

## 操作处理器

将笔记保存到数据库是后端的责任，所以我们立即将控制权传递给后端，并请它创建笔记。一旦完成，我们显示新创建的笔记，以便用户可以设置任务标题，也许还有一些属性。

## 脚本执行

所以我们有一个脚本，它会在工具栏中添加按钮。但是我们如何执行它呢？一种可能是点击"播放"图标（用红圈标记）。问题是这种 UI 更改受 Trilium 运行时的时间限制，所以当我们重启 Trilium 时，按钮就不在了。

我们需要在每次 Trilium 启动时执行它，但我们可能不想在每次启动时都手动点击播放按钮。

解决方案用底部红圈标记——这个笔记有[标签](#root/_help_zEY4DaJG4YT5) `#run=frontendStartup`——这是 Trilium 能理解的"系统"标签之一。正如你可能猜到的，这会导致所有带有此标签的脚本笔记在 Trilium 前端启动时被执行。

(`#run=frontendStartup` 不适用于[移动端前端（Mobile frontend）](#root/_help_RDslemsQ6gCp)——如果你想让脚本在那里运行，请给脚本 `#run=mobileStartup` 标签)。

### 执行按钮

可运行的代码笔记（前端或后端）和已保存的 SQL 控制台可以可选地在描述旁边有一个专用的执行按钮。

为此，请应用以下[标签](#root/_help_HI6GBBIduIgv)：

*   一个 `#executeButton`，其值作为按钮的标签显示。
*   一个可选的 `#executeDescription`，在其旁边添加说明文本。

## 自动补全与代码检查

从 Trilium v0.104.0 开始，前端脚本、后端脚本和渲染笔记受益于自动补全系统。

输入 <kbd>.</kbd> 时自动触发自动补全，或按 <kbd>Ctrl</kbd>+<kbd>Space</kbd> 手动触发。

除此之外，编辑器还会显示语法错误和警告，例如不可达代码。

> [!NOTE]
> 如果你注意到报告的错误/警告有误报，或者 API 不正确或缺失，请随时[提交问题（open a issue）](#root/_help_wy8So3yZZlH9)并附上代码示例。

## 更多展示

你可以在<a class="reference-link" href="#root/_help_5668rwcirq1t">高级展示（Advanced Showcases）</a>中看到更多带解释的脚本。

## 事件

请参阅<a class="reference-link" href="#root/_help_GPERMystNGTB">事件（Events）</a>。

## 脚本 API

请参阅<a class="reference-link" href="#root/_help_GLks18SNjxmC">脚本 API（Script API）</a>。""",

"RnGPOFzZTT85": """对于[脚本代码笔记](#root/_help_CdNpE2pqjmI6)，Trilium 提供了一个 API，让它们可以访问应用程序的各种功能。

有两个 API：

*   一个用于前端脚本：<a class="reference-link" href="#root/_help_Q2z6av6JZVWm">前端 API（Frontend API）</a>
*   一个用于后端脚本：<a class="reference-link" href="#root/_help_MEtfsqa5VwNi">后端 API（Backend API）</a>

在两种情况下，API 都位于全局变量 `api` 中，可以在脚本中的任何位置使用。

例如，要向用户显示消息，可以使用以下前端脚本：

```
api.showMessage("你好，世界。");
```

> [!NOTE]
> **注意**  
> 脚本 API 目前是实验性的，可能在未来的更新中发生变化。""",

"stK1EEQ7I07S": """对于[脚本代码笔记](#root/_help_CdNpE2pqjmI6)，Trilium 提供了一个 API，让它们可以访问应用程序的各种功能。

有两个 API：

*   一个用于前端脚本：<a class="reference-link" href="#root/_help_Q2z6av6JZVWm">前端 API（Frontend API）</a>
*   一个用于后端脚本：<a class="reference-link" href="#root/_help_MEtfsqa5VwNi">后端 API（Backend API）</a>

在两种情况下，API 都位于全局变量 `api` 中，可以在脚本中的任何位置使用。

例如，要向用户显示消息，可以使用以下前端脚本：

```
api.showMessage("你好，世界。");
```

> [!NOTE]
> **注意**  
> 脚本 API 目前是实验性的，可能在未来的更新中发生变化。""",
}
