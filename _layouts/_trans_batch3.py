# -*- coding: utf-8 -*-
TRANS = {
"39unKGhUFGBr": """要在桌面上安装 Trilium，请按照以下步骤操作：

1.  **下载最新版本**：从 GitHub 的[最新发布页面](https://github.com/TriliumNext/Trilium/releases/latest)获取适用于你操作系统的二进制发行版。
2.  **解压软件包**：将下载的软件包解压到你选择的位置。
3.  **运行应用程序**：通过执行解压文件夹中的 `trilium` 可执行文件来启动 Trilium。

## 启动脚本

Trilium 提供各种启动脚本来自定义你的体验：

*   `trilium-no-cert-check`：启动 Trilium 时不验证 [TLS 证书](#root/_help_l2VkvOwUNfZj)，在连接到使用自签名证书的服务器时很有用。
    *   或者，在启动 Trilium 前设置 `NODE_TLS_REJECT_UNAUTHORIZED=0` 环境变量。
*   `trilium-portable`：以便携模式启动 Trilium，此时[数据目录](#root/_help_tAassRL4RSQL)会创建在应用程序目录内，便于移动整个安装。Electron 的内部数据（缓存、字典等）也存储在数据目录内，因此不会向系统的漫游配置文件写入任何文件。
*   `trilium-safe-mode`：以"安全模式"启动 Trilium，禁用可能导致应用程序崩溃的任何启动脚本。

## 同步

对于希望与服务器实例同步数据的 Trilium 桌面用户，请参阅<a class="reference-link" href="#root/_help_cbkrhQjrkKrh">同步（Synchronization）</a>指南以获取详细说明。""",

"cSgjbDO3SCet": """### 行内代码

行内代码使用等宽字体格式化文本，以表示句子中的技术内容，例如代码、路径等。

![](5_Developer-specific%20formatting_image.png)

行内代码用于说明文件系统路径以及 shell 命令（此处为 `git`）的示例。

插入行内代码：

*   通过[格式化工具栏（Formatting toolbar）](#root/_help_nRhnJkTT8cPs)，查找 ![](3_Developer-specific%20formatting_image.png) 按钮。
*   输入 \`code\`，其中 `code` 是希望自动格式化为行内代码的文本。

### 代码块

代码块将一段代码显示为专用块：

![](4_Developer-specific%20formatting_image.png)

一个启用了 JavaScript 语法高亮的代码块。

请注意，代码块不适用于非常大的代码段。请改用专用的[代码（Code）](#root/_help_6f9hih2hXXZk)笔记类型。

更多信息请参阅专门文档：[代码块（Code blocks）](#root/_help_QxEyIjRBizuC)

### 键盘快捷键

这允许将一段文本标记为快捷键。

![](Developer-specific%20formatting_image.png)

要应用此样式，请按[格式化工具栏（Formatting toolbar）](#root/_help_nRhnJkTT8cPs)中![](1_Developer-specific%20formatting_image.png)组里的![](6_Developer-specific%20formatting_image.png)按钮。在浮动工具栏上，选中文本时会出现这些按钮。

或者，按 <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>K</kbd> 组合键为当前选中的文本应用键盘快捷键格式。

可以通过再次按下按钮（从界面或键盘）来移除该格式，或者使用 <img src="2_Developer-specific formatting_image.png" width="17" height="16"> _移除格式（Remove formatting）_按钮。""",

"GSJbXGisvcFM": """启动栏小组件是<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>的一个子集，可用于在<a class="reference-link" href="#root/_help_xYmIYSP6wE3F">启动栏（Launch Bar）</a>中渲染自定义按钮和小组件。

## 为桌面布局创建启动栏小组件

与<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>不同，启动栏小组件的设置过程略有不同：

1.  创建一个_JavaScript（前端）_类型或 JSX（用于基于 Preact 的小组件）的代码笔记。
    *   脚本本身使用与<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>相同的概念，包括使用 `NoteContextAwareWidget` 或 `BasicWidget`（根据需要）。
    *   传统格式和 Preact 格式的示例，请参阅<a class="reference-link" href="#root/_help_IPArqVfDQ4We">笔记标题小组件（Note Title Widget）</a>和<a class="reference-link" href="#root/_help_gcI7RPbaNSh3">模拟时钟（Analog Watch）</a>。
2.  不要设置 `#widget`，因为该属性是为<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>保留的。
3.  在<a class="reference-link" href="#root/_help_x3i7MxGccDuM">全局菜单（Global menu）</a>中，选择_配置启动栏（Configure launchbar）_。
4.  在_可见启动器（Visible Launchers）_部分，选择_添加自定义小组件（Add a custom widget）_。
5.  为新建的启动器命名（也可以选择是否命名）。
6.  在<a class="reference-link" href="#root/_help_OFXdgB2nNk1F">提升属性（Promoted Attributes）</a>部分，修改 _widget_ 字段以指向新创建的笔记。
7.  [刷新（Refresh）](#root/_help_s8alTXmpFR61)界面。

## 移动端布局

自定义启动栏小组件也受<a class="reference-link" href="#root/_help_RDslemsQ6gCp">移动端前端（Mobile Frontend）</a>支持。操作步骤如下：

1.  转到<a class="reference-link" href="#root/_help_x3i7MxGccDuM">全局菜单（Global menu）</a>→ _高级（Advanced）_ → _显示隐藏子树（Show Hidden Subtree）_。
2.  查找 _移动端启动栏（Mobile Launch Bar）_，并按照上一节相同的创建机制进行操作。""",

"ZlacHrx96iIP": """与在客户端/浏览器端运行的[前端脚本（front-end scripts）](#root/_help_yIhgI5H7A2Sm)不同，后端脚本直接在 Trilium 服务器的 Node.js 环境中运行。

后端脚本既可以用于<a class="reference-link" href="#root/_help_WOcw2SLH6tbX">服务器安装（Server Installation）</a>（将在运行服务器的设备上运行），也可以用于<a class="reference-link" href="#root/_help_poXkQfguuA0U">桌面安装（Desktop Installation）</a>（将在 PC 上运行）。

> [!IMPORTANT]
> 从 v0.104.0 开始，后端脚本默认被禁用，以减少攻击面。更多信息请参阅<a class="reference-link" href="#root/_help_fiHicjpHjIRJ">安全（Security）</a>。

## 后端脚本的优势

后端脚本的好处在于它们可以非常强大，例如可以访问底层系统，比如读取文件或执行进程。

然而，后端脚本的主要优势在于它们可以更容易地访问笔记，因为笔记信息已经加载到内存中。而在客户端，必须先手动加载笔记。

## 创建后端脚本

创建一个新的<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记，并选择语言_JavaScript（Trilium 后端）_。

## 运行后端脚本

后端脚本既可以手动运行（通过脚本页面上的执行按钮），也可以在特定事件上触发。

此外，脚本可以在服务器启动时自动运行、按固定时间间隔运行，或在发生某个事件（例如属性被修改）时运行。更多信息请参阅专门的<a class="reference-link" href="#root/_help_GPERMystNGTB">事件（Events）</a>页面。

## 脚本 API

Trilium 通过 `api` 对象暴露了一组脚本可以直接使用的 API。关于此 API 的参考，请参阅<a class="reference-link" href="#root/_help_MEtfsqa5VwNi">后端 API（Backend API）</a>。""",
}
