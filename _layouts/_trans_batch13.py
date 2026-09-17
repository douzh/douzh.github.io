# -*- coding: utf-8 -*-
TRANS = {
"u1QzNAKOIwgl": """_新布局（New layout）_是 v0.101.0 中引入的一系列 UI/UX 更改，它大幅改变了现有的 UI 元素，也添加了一些新元素。这个新布局的目标是使应用程序现代化、更直观，同时减少杂乱。

## 新引入的功能

### 状态栏

在窗口底部有一条新栏，称为_状态栏（Status bar）_。这条栏包含多个项目，如面包屑导航，以及当前笔记的信息和设置，如[内容语言](#root/_help_veGu4faJErEM)和<a class="reference-link" href="#root/_help_zEY4DaJG4YT5">属性（Attributes）</a>。

更多信息请参阅[专门页面](#root/_help_AlJ73vBCjWDw)。

<figure class="image"><img style="aspect-ratio:1150/27;" src="5_New Layout_image.png" width="1150" height="27"></figure>

### 行内标题

在 Trilium 的先前版本中，标题栏一直固定。在新布局中，既有固定标题栏，也有随文本滚动的标题栏。新引入的标题称为_行内标题（Inline title）_，它以更大的字体显示标题，同时显示创建日期和修改日期等附加信息。

每当标题滚动过去时，就会显示固定标题。

这只影响<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>和<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记。占据整个屏幕的笔记类型（如<a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a>）将始终只有固定标题栏。

根据笔记类型，行内标题还会呈现一些更具交互性的选项，例如能够切换笔记类型（见下文）。

<figure class="image"><img style="aspect-ratio:899/122;" src="New Layout_image.png" width="899" height="122"><figcaption>显示在笔记顶部、可以滚动过去的<em>行内标题（Inline title）</em>。</figcaption></figure><figure class="image"><img style="aspect-ratio:910/104;" src="4_New Layout_image.png" width="910" height="104"><figcaption>固定标题栏。标题只在滚动过<em>行内标题（Inline title）</em>后出现。</figcaption></figure>

### 新的笔记类型切换器

创建新的<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>或<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记时，_行内标题_下方会出现一个笔记类型切换器。除了更改笔记类型，还可以应用[模板](#root/_help_KC1HB96bqqHX)。

一旦输入文本，切换器就会消失。

<img src="6_New Layout_image.png" width="735" height="143">

### 笔记徽章

笔记徽章出现在固定笔记标题附近，指示关于笔记的重要信息，例如它是否为只读。一些徽章也是可交互的。

<figure class="image"><img style="aspect-ratio:910/49;" src="3_New Layout_image.png" width="910" height="49"></figure>

以下徽章可用：

*   **只读徽章（Read-only badge）**，当笔记由于自动只读或手动只读而不可编辑时显示。单击徽章将临时编辑笔记（类似于编辑[浮动按钮](#root/_help_XpOYSgsLkTJy)）。
*   **分享徽章（Share badge）**，指示当前笔记已分享。徽章还会指示分享是在本地网络上（对于未设置<a class="reference-link" href="#root/_help_cbkrhQjrkKrh">同步（Synchronization）</a>的桌面应用）还是可公开访问（对于服务器）。
*   **网页剪辑徽章（Web clip badge）**，指示笔记是否使用<a class="reference-link" href="#root/_help_MtPxeAWVAzMg">网页剪辑器（Web Clipper）</a>剪辑。徽章充当链接，可以点击导航到页面，或右键单击以获取更多选项。
*   **执行徽章（Execute badge）**，用于具有执行按钮或描述的[脚本](#root/_help_CdNpE2pqjmI6)或[已保存的 SQL 查询](#root/_help_YKWqdJhzi2VY)。

其中一些徽章取代了笔记顶部专用的面板。

### 可折叠区域

<figure class="image"><img style="aspect-ratio:496/265;" src="1_New Layout_image.png" width="496" height="265"></figure>

以下区域已可折叠：

*   _提升属性（Promoted Attributes）_
    *   对于全高笔记（如<a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a>），提升属性默认折叠以腾出空间。
    *   以前用于触发提升属性功能区选项卡（已不再起作用）的键盘快捷键已重新用于切换提升属性。
*   _已编辑笔记（Edited Notes）_，为<a class="reference-link" href="#root/_help_l0tKav7yLHGF">[缺失笔记]</a>出现，现在显示在标题下方。
    *   该区域是否折叠取决于<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → 外观中的选择。
*   _搜索属性（Search Properties）_，为完整的<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>和<a class="reference-link" href="#root/_help_m523cpzocqaD">已保存搜索（Saved Search）</a>出现。

### 保存状态指示器

<img class="image-style-align-right" src="2_New Layout_image.png" width="168" height="47">在笔记标题右侧，对文档进行更改后会显示一个临时指示器，指示文档是否已保存。

它指示以下状态：

*   _未保存（Unsaved）_，如果更改即将保存。
*   _保存中（Saving）_，如果更改正在保存。
*   _已保存（Saved）**，如果所有更改都成功保存到服务器。
*   _错误（Error）_，如果更改无法保存，例如由于与服务器通信的问题。

所有更改保存后，指示器会在几秒后自动隐藏。

## 对现有布局的更改

### 功能区的移除

最显著的更改是功能区的移除。功能区中的所有操作和选项都已整合到应用程序的其他位置。

以下是一度属于功能区的所有选项卡在新布局中的可用方式：

*   "格式化工具栏"被重新定位到页面顶部。
    *   不再是每个分屏一个，现在每个标签页有一个格式化工具栏。这为工具栏项目留出了更多空间。
*   "拥有的属性（Owned attributes）"和"继承的属性（Inherited attributes）"被合并并移到状态栏区域（一个显示在另一个上方）。
*   "基本属性（Basic Properties）"被整合到<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>菜单中。
    *   这里唯一的例外是语言组合框，现在可以在状态栏（屏幕右上角）找到。
*   "文件（File）"和"图片（Image）"选项卡
    *   按钮被移到笔记标题右侧，作为<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>中的专门条目。
    *   信息部分已合并到状态栏的_笔记信息（Note info）_部分。
*   已编辑笔记
    *   移到标题下方，显示在可折叠区域内，笔记表示为徽章/芯片。
    *   该区域是展开还是折叠取决于选项 → 外观中的"已编辑笔记功能区选项卡将自动在日记笔记上打开"设置。
*   搜索定义选项卡
    *   移到标题下方，位于可折叠区域内。
    *   新搜索默认展开，已保存搜索折叠。
*   笔记地图现在在笔记操作菜单中可用。
    *   笔记地图不再是打开到功能区的面板中，而是在侧边栏的<a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>中打开，在那里也可以最大化。
*   "笔记信息（Note info）"选项卡被移到一个小的 (i) 图标中，位于状态栏。
*   "相似笔记（Similar notes）"选项卡
    *   相似笔记不再是打开到功能区的面板中，而是在侧边栏的<a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>中显示。
*   集合属性选项卡被重新定位到笔记标题下方，并分组为：
    *   一个用于快速切换视图的组合框。
    *   一个子菜单中的当前视图的单独设置。
*   一些较小的功能区选项卡被转换为徽章，出现在面包屑部分的笔记标题附近：
    *   剪辑网页的原始 URL 指示器（`#pageUrl`）。
    *   SQL 和脚本执行按钮。

> [!NOTE]
> 功能区键盘快捷键（例如 `toggleRibbonTabClassicEditor`）已被重新用于新布局，它们将在新布局中切换相应的面板。

### 浮动按钮的移除

大多数按钮被重新定位到笔记标题右侧的<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>区域，但以下例外：

*   编辑按钮作为徽章显示在笔记标题附近。
*   _反向链接（Backlinks）_现在显示在侧边栏的<a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>中。
    *   或者，反向链接计数显示在状态栏中，单击时侧边栏会在正确部分打开。
*   关系图缩放按钮现在是关系图本身的一部分。
*   导出图像为 PNG/SVG 现在在笔记操作菜单的_导出为图像（Export as image）_选项中。

### 侧边栏的更改

侧边栏（也称为右侧窗格）也收到了一些重要更改。

最重要的是，v0.105.0 将侧边栏拆分为多个选项卡，并带有附加功能：

*   <a class="reference-link" href="#root/_help_2dcGDhp9WFKb">[缺失笔记]</a>，汇集目录和高亮列表。
*   <a class="reference-link" href="#root/_help_uNtgN0cUKEBc">[缺失笔记]</a>，提供编辑标签、关系和（提升的）属性定义的图形化方法。
*   一个专用的<a class="reference-link" href="#root/_help_LrKznOtw9cQD">[缺失笔记]</a>。
*   <a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>，将笔记地图、笔记路径、反向链接和相似笔记组合在一起。

侧边栏的先前版本会根据是否有要显示的项目而上下文显示。这导致在分屏视图的两个窗格之间移动时偶尔发生内容偏移。在新布局中，侧边栏更像<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>窗格，即使没有要显示的内容也保持可见。

为了切换侧边栏，屏幕右上角（在 Windows 和 Linux 上靠近窗口按钮）有一个新按钮。

现在侧边栏的每个部分（例如"目录"、"高亮列表"）都可以单独折叠，并会记住是否被折叠。

一些侧边栏项目也有上下文菜单，由标题上的三个点表示。例如，高亮过滤器可以直接从该菜单调整。

自定义小组件仍然受支持。对于自定义脚本，三个点菜单允许快速导航到相应的脚本笔记。

## 如何切换新布局

从 v0.101.0 开始，这个新布局默认启用。可以通过转到<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → 外观并选择_旧布局（Old layout）_回退到旧布局。

> [!IMPORTANT]
> 由于引入了新布局，它成为标准布局。_旧布局（Old layout）_被视为已弃用，不会接收新功能（例如面包屑），因为我们专注于新布局。在某个时候，旧布局将被完全移除，因为维护两个有重大差异的布局会造成维护负担。""",

"s5yEiZdU32Oh": """Trilium 可以连接大型语言模型，并将其用作直接在你的笔记上工作的助手：询问关于你正在阅读的笔记的问题、让它起草或重组内容、或者让它为你编写脚本和小组件。

该集成默认关闭，在你启用并配置提供商之前不会做任何事情；Trilium 本身不附带任何模型。你选择哪个提供商也决定了你的笔记去向：按使用计费的云 API、你已经付费的订阅，或运行在你自己的硬件上的模型（在这种情况下，什么都不会离开机器）。请参阅<a class="reference-link" href="#root/_help_bkPZAMT7d2sR">提供商（Providers）</a>了解每个涉及什么，以及<a class="reference-link" href="#root/_help_cRRrdZjgyhNc">隐私（Privacy）</a>了解到底发送了什么。

启用后，助手可以作为：

*   一种专门的笔记类型，它可以通过工具读取和修改笔记，如果你更希望它只看到你输入的内容，你可以在每次对话中关闭它。
*   右侧边栏中的一个面板，其行为与专门的笔记类型类似，但它还可以可选地访问当前笔记。
*   一个<a class="reference-link" href="#root/_help_VRoP0g1uHHc2">编辑器内 AI 助手（In-editor AI assistant）</a>，带有作用于所选文本的内置操作（如校对、总结），以及<a class="reference-link" href="#root/_help_mVEYwt20exhO">自定义 AI 快速操作（Custom AI quick actions）</a>。

## 功能亮点

*   基于聊天的界面，消息实时流式传输。
*   为 AI 提供当前正在查看的笔记作为上下文。
*   用于更改笔记内容、创建新笔记等的工具。
*   关于上下文窗口使用和每条消息定价的统计信息。
*   用于多模态聊天的附件（图像、文本文件、PDF）。
*   可选的 MCP，允许外部聊天工具（例如 Claude Code）对 Trilium 内的笔记进行操作。

## 示例用例

*   创建任何类型的<a class="reference-link" href="#root/_help_MgibgPcfeuGz">自定义小组件（Custom Widgets）</a>。
*   轻松创建<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>，例如_为我创建一个允许我玩井字棋的渲染笔记。确保使用 Preact 而不是旧版 jQuery。_
*   为<a class="reference-link" href="#root/_help_IF7Q6I9x7zuw">仪表盘（Dashboard）</a>创建小组件，例如计算器、秒表、番茄钟。

> [!NOTE]
> 已知 Claude Sonnet 能在很少指导的情况下产生非常好的前端或后端脚本，因为 AI 已经被告知如何编写它们。

## LLM 提供商

Trilium 支持四种不同类型的提供商：

*   **云提供商（Cloud providers）**  
    按使用付费，使用 API 密钥，与你可能已有的任何订阅分开计费
    *   Anthropic (Claude)
    *   OpenAI (GPT)
    *   Google (Gemini)
    *   DeepSeek
*   **基于订阅的（Subscription-based）**  
    复用现有订阅而不是按使用付费。
    *   目前只支持 Claude Code。
*   **本地或自托管 LLM 解决方案**
    *   Ollama
    *   LM Studio。
*   **自定义 OpenAI 兼容端点**  
    用于 Trilium 不直接支持的其他提供商，无论是本地还是托管的（例如 OpenRouter、Groq、Mistral）。

关于每个提供商的更多信息，请参阅<a class="reference-link" href="#root/_help_bkPZAMT7d2sR">提供商（Providers）</a>。请参阅专门的<a class="reference-link" href="#root/_help_cRRrdZjgyhNc">隐私（Privacy）</a>以更好地了解什么数据被发送给提供商。

## 启用 AI 集成

要启用 AI 集成，只需转到<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _AI / LLM_，按对话框右上角的开关并配置一个提供商。

## 创建新聊天

有两种不同的聊天界面：

*   一个在侧边栏中。
*   一种专门的笔记类型。

### 侧边栏界面

### 专门的笔记类型

专门的聊天笔记与侧边栏界面类似，但它使更长的对话阅读更舒适。

与侧边栏不同，AI 不会意识到它所在的当前笔记。

### 模板

聊天笔记可以设置为<a class="reference-link" href="#root/_help_KC1HB96bqqHX">模板（Templates）</a>以方便复用。整个对话历史都会被保留，允许对 LLM 进行一种基本形式的专业化，现有聊天充当系统提示。

### 模型选择

在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a>中配置提供商后，下一步是选择聊天可用的模型。

模型仅在模型选择列表可见时从提供商动态获取。要更改模型列表，只需按模型选择框中的编辑按钮。

已知模型会显示定价信息。定价信息（每百万 token 的价格）内嵌在应用程序中（使用 LiteLLM 数据的一个子集），并随 Trilium 新版本更新。本地提供商被视为免费，而自定义端点提供商不提供任何定价信息。

## 功能

### 网页搜索

AI 可以可选地搜索网页以查找关于特定主题的更多信息。

此功能默认开启，但可以通过单击聊天底部的模型选择器并取消勾选_网页搜索（Web search）_轻松禁用。

> [!NOTE]
> 目前只支持 LLM 提供商自带的搜索。外部搜索提供商（如 Exa、Tavily 和 SearXNG）尚不支持。

### 笔记访问（工具）

工具允许智能体 AI 直接在你的 Trilium 实例中理解和操作笔记。

此功能默认开启，但可以通过单击聊天底部的模型选择器并取消勾选_笔记访问（Note access）_轻松禁用。

以下是 Trilium 为 LLM 提供的一些工具：

*   在笔记级别：
    *   搜索笔记
    *   获取笔记的元数据或内容。
    *   编辑笔记
        *   有多种机制供 LLM 编辑笔记：完全重写、查找/替换文本序列或追加。
        *   每当 AI 进行更改时，都会保存一个[修订（revision）](#root/_help_vZWERwf8U3nx)以便能够还原任何不需要的更改。
    *   创建新笔记
    *   重命名或删除笔记。
*   在属性级别：
    *   获取完整属性列表或特定属性。
    *   设置属性的值。
    *   删除属性。
*   在树级别：
    *   获取笔记的直接子项。
    *   获取笔记的整个子树。
    *   将笔记移动或克隆到其他地方。
*   对于<a class="reference-link" href="#root/_help_0vhv7lsOLy82">附件（Attachments）</a>：
    *   获取附件的元数据。
    *   获取附件的内容。
*   技能（Skills）（请参阅专门章节）。

> [!WARNING]
> 目前**没有实现笔记工具的权限管理**，这意味着 LLM 可能删除现有笔记或用笔记弄乱树。通常大多数操作很容易还原（删除笔记、恢复已删除的笔记、还原对笔记的修改），但有些操作更难还原（例如设置属性，因为没有属性历史）。

> [!NOTE]
> Gemini 有一个特殊情况：_笔记访问（Note access）_和_网页搜索（Web search）_不能同时启用。

### 附件

自 Trilium v0.140.0 起，<a class="reference-link" href="#root/_help_0vhv7lsOLy82">附件（Attachments）</a>支持多模态聊天：

*   光栅图像（作为视觉输入发送，SVG 除外），支持以下格式：PNG、JPEG、GIF、WebP。
*   PDF，以原生方式发送给提供商（Anthropic、OpenAI 和 Google 支持）。
*   SVG 图像（作为原始 HTML 发送）。
*   文本文件。

要上传附件：

*   按文本框下方的专用_附加（Attach）_按钮（回形针图标）。
*   使用 Ctrl+V 直接从剪贴板粘贴图像。

上传一个或多个附件后，它们会出现在文本框正上方：

*   图像有一个小缩略图，便于识别。
*   每个附件都可以通过按相应的 X 按钮删除。

存在附件时，LLM 会被指示优先考虑该附件，即使它可以访问当前笔记。

> [!NOTE]
> 目前 Trilium 在将附件发送给 LLM 提供商之前不会预处理它们（例如通过<a class="reference-link" href="#root/_help_TiQbQDgP8L5t">文本提取（OCR）</a>）。

### 提及（Mentions）

提及是一种插入当前笔记之外的其他笔记引用的方式，使用与<a class="reference-link" href="#root/_help_hrZ1D00cLbal">内部（引用）链接（Internal (reference) links）</a>相同的机制。要引用另一个笔记，只需输入 @ 后跟要引用的笔记名称。

此功能在启用笔记工具时最有用，否则 LLM 将无法访问给定的笔记。

### 技能（Skills）

Trilium 中的技能是专门的指令集，通过理解 Trilium 帮助 AI 更高效。

这些技能默认不加载以避免增加 token 消耗，但如果启用_笔记工具（Note tools）_，AI 可以按需加载它们。

以下技能是内置的：

*   搜索语法：理解<a class="reference-link" href="#root/_help_eIg8jdvaoNNd">搜索（Search）</a>的完整语法。
*   后端脚本：能够编写正确的<a class="reference-link" href="#root/_help_SPirpZypehBG">后端脚本（Backend scripts）</a>。
*   前端脚本：能够编写正确的前端脚本（基本脚本、小组件、<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>）。

启用_笔记工具（Note tools）_时，技能将自动提供给 AI，因此不需要用户交互。

> [!NOTE]
> 目前不支持自定义技能，但已在计划中。

### MCP

Trilium 带有内置的 MCP 服务器，允许你使用外部智能体（如 Claude Code）访问你的数据库。更多详情请参阅专门的<a class="reference-link" href="#root/_help_KOWBFgsZXJAD">MCP</a>页面。

## 历史

### v0.102.0 中的移除

从 v0.102.0 开始，AI/LLM 集成已从 Trilium Notes 核心中移除。

虽然在这个功能的开发上投入了大量精力，但从长远来看，维护和支持它被证明是不可持续的。

升级到 v0.102.0 时，你的聊天笔记将被保留，但不再是专用的聊天窗口，而是变成普通的<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记，揭示对话的底层 JSON。

### v0.103.0 中的重新引入

鉴于 AI 领域的最新进展，我们决定再给 LLM 集成一次机会。v0.103.0 引入了一个全新的聊天系统。

导致重新实现的关键变化之一是，现在我们使用一个库（[Vercel AI](https://github.com/vercel/ai)）来管理内部机制和 LLM 提供商之间的差异，而不是自己实现。""",
}
