# -*- coding: utf-8 -*-
TRANS = {
"BM2PpcgGjM0v": """Trilium 原生支持以下格式的导入和导出。

## 支持的格式

*   HTML：
    *   这是 Trilium 使用的主要格式，使用标准标签来表示基本格式和布局（例如 `<strong>`、`<table>`、`<pre>`）。
    *   请注意，HTML 不是标准化格式，因此一些更具体的功能，如提示块或<a class="reference-link" href="#root/_help_hrZ1D00cLbal">内部（引用）链接（Internal (reference) links）</a>，可能不被其他应用程序支持。
    *   笔记还可以导出为[可用于网页发布的静态 HTML](#root/_help_ycBFjKrrwE9p)。
*   <a class="reference-link" href="#root/_help_Oau6X9rCuegd">Markdown</a>
    *   大部分格式都会被保留，请参阅<a class="reference-link" href="#root/_help_rJ9grSgoExl9">支持的语法（Supported syntax）</a>。
*   OPML（大纲交换格式）
    *   支持用于纯文本的 OPML v1.0 和带 HTML 支持的 v2.0。

要从 OneNote、Notion 等其他应用程序导入，请参阅<a class="reference-link" href="#root/_help_ckZ2uNWQmVeX">从其他应用程序导入数据（Importing data from other applications）</a>。

## 最大导入大小

v0.104.0 之前的版本有 250 MiB 的上传限制，可以通过 `TRILIUM_NO_UPLOAD_LIMIT` 环境变量绕过；从 v0.104.0 起该限制被移除。

尽管如此，**单个项目**（无论是<a class="reference-link" href="#root/_help_W8vYD3Q1zjCR">文件（File）</a>、<a class="reference-link" href="#root/_help_0vhv7lsOLy82">附件（Attachments）</a>甚至<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>笔记）仍然有大小限制。限制约为 374 MiB，由<a class="reference-link" href="#root/_help_cbkrhQjrkKrh">同步（Synchronization）</a>协议决定。尝试导入此类大文件将被拒绝。

在大规模导入或导出期间，内存消耗可能会激增，但会保持在 2 GB 左右。已使用约 2.4 GB 数据库（约 21000 条笔记）测试。

> [!IMPORTANT]
> 对于<a class="reference-link" href="#root/_help_poXkQfguuA0U">桌面安装（Desktop Installation）</a>，有两种不同的导入机制：
> 
> *   快速导入，从<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>。
> *   导入对话框（通过在笔记树中右键单击 → _导入到笔记（Import into note）_，或从<a class="reference-link" href="#root/_help_8YBEPzcpUgxw">笔记按钮（Note buttons）</a>）。
> 
> 处理大文件（数 GB）时，建议使用导入对话框，因为它有特殊机制确保文件直接从磁盘读取而不是再次上传。

## 导出根笔记

根笔记是最顶层的笔记。导出它的行为与任何其他笔记相同：在<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>中右键单击它并选择_导出（Export）_。

从 v0.104.0 开始，导入根笔记时，它将被导入为你现有根笔记的子项。此行为确保导入不能更改或覆盖你现有的笔记。

你可以使用树的<a class="reference-link" href="#root/_help_yTjUdsOi4CIE">多选（Multiple selection）</a>轻松将笔记移到根节点，并删除冗余的根笔记。

| 之前 | 之后 |
| --- | --- |
| root<br>  └─ one<br>  └─ two | root（现有）<br>  └─ root（来自导入）<br>      └─ one<br>      └─ two |

> [!TIP]
> 与其导出完整的 ZIP（包括根笔记），不如考虑使用[备份（backups）](#root/_help_ODY7qQn5m2FT)。备份始终包含整个结构，以及 ZIP 导出没有的额外信息：保持笔记 ID、包含选项/令牌，并更好地处理<a class="reference-link" href="#root/_help_bwg0e8ewQMak">受保护笔记（Protected Notes）</a>。""",

"x6d4zSRRm1Id": """> [!TIP]
> 如需快速开始，请查阅<a class="reference-link" href="#root/_help_9qPsTWBorUhQ">API 参考（API Reference）</a>。

ETAPI 是 Trilium 的公共/外部 REST API。自 Trilium v0.50 起可用。

## API 客户端

作为直接调用 API 的替代方案，有一些客户端库可以简化这一过程：

*   [trilium-py](https://github.com/Nriver/trilium-py)，你可以使用 Python 与 Trilium 通信。

## 获取令牌

与 REST API 的所有操作都必须使用令牌进行身份验证。你可以从 选项（Options）→ ETAPI 获取此令牌，或使用 `/auth/login` REST 调用以编程方式获取（参见[规范（spec）](https://github.com/TriliumNext/Trilium/blob/master/src/etapi/etapi.openapi.yaml)）。

## 身份验证

### 通过 `Authorization` 头

```
GET https://myserver.com/etapi/app-info
Authorization: ETAPITOKEN
```

其中 `ETAPITOKEN` 是在上一步中获取的令牌。

为了与各种工具兼容，也可以以 `Bearer ETAPITOKEN` 格式指定 `Authorization` 头的值（自 0.93.0 起）。

### 基本身份验证

自 v0.56 起你也可以使用基本认证格式：

```
GET https://myserver.com/etapi/app-info
Authorization: Basic BATOKEN
```

*   其中 `BATOKEN = BASE64(username + ':' + password)` —— 这是标准的基本认证序列化
*   其中 `username` 是 "etapi"
*   而 `password` 是上面描述的生成的 ETAPI 令牌。

基本认证旨在用于只支持基本认证的工具。

## 使用 Bash 脚本交互

可以编写简单的 Bash 脚本与 Trilium 交互。作为例子，以下是如何获取笔记的 HTML 内容：

```
#!/usr/bin/env bash

# 配置
TOKEN=z1vA4fkGxjOR_ZXLrZeqHEFOv65yV3882iFCRtNIK9k9iWrHliITNSLQ=
SERVER=http://localhost:8080

# 按 ID 下载笔记
NOTE_ID="i6ra4ZshJhgN"
curl "$SERVER/etapi/notes/$NOTE_ID/content" -H "Authorization: $TOKEN" 
```

确保替换以下值：

*   `TOKEN` 替换为你的 ETAPI 令牌。
*   `SERVER` 替换为你的 Trilium 实例的正确协议、主机名和端口。
*   `NOTE_ID` 替换为要下载的现有笔记 ID。

再举一个例子，要获取笔记的 .zip 导出并将其放在名为 `out` 的目录中，只需将脚本中的最后一条语句替换为：

```
curl -H "Authorization: $TOKEN" \
	-X GET "$SERVER/etapi/notes/$NOTE_ID/export" \
    --output "out/$NOTE_ID.zip"
```""",

"6HcOzchIPSTM": """Trilium 指标 API（Metrics API）提供关于你的 Trilium 实例的全面监控数据，专为 Prometheus 等外部监控系统设计。

## **端点（Endpoint）**

*   **URL**：`/etapi/metrics`
*   **方法（Method）**：`GET`
*   **身份验证（Authentication）**：需要 ETAPI 令牌
*   **默认格式（Default Format）**：Prometheus 文本格式

## **身份验证（Authentication）**

你需要一个 ETAPI 令牌来访问指标端点。通过以下方式获取：

```
# 获取一个 ETAPI 令牌
curl -X POST http://localhost:8080/etapi/auth/login \
  -H "Content-Type: application/json" \
  -d '{"password": "your_password"}'

```

## **用法（Usage）**

### **Prometheus 格式（默认）**

```
curl -H "Authorization: YOUR_ETAPI_TOKEN" \
  http://localhost:8080/etapi/metrics

```

以 Prometheus 文本格式返回指标：

```
# HELP trilium_info Trilium instance information
# TYPE trilium_info gauge
trilium_info{version="0.91.6",db_version="231",node_version="v18.17.0"} 1 1701432000

# HELP trilium_notes_total Total number of notes including deleted
# TYPE trilium_notes_total gauge
trilium_notes_total 1234 1701432000

```

### **JSON 格式**

```
curl -H "Authorization: YOUR_ETAPI_TOKEN" \
  "http://localhost:8080/etapi/metrics?format=json"

```

以 JSON 格式返回详细的指标，用于调试或自定义集成。

## **可用指标（Available Metrics）**

### **实例信息（Instance Information）**

*   `trilium_info` - 带标签的版本和构建信息

### **数据库指标（Database Metrics）**

*   `trilium_notes_total` - 总笔记数（包括已删除的）
*   `trilium_notes_deleted` - 已删除笔记数
*   `trilium_notes_active` - 活动笔记数
*   `trilium_notes_protected` - 受保护笔记数
*   `trilium_attachments_total` - 总附件数
*   `trilium_attachments_active` - 活动附件数
*   `trilium_revisions_total` - 总笔记修订数
*   `trilium_branches_total` - 活动分支数
*   `trilium_attributes_total` - 活动属性数
*   `trilium_blobs_total` - 总 blob 记录数
*   `trilium_etapi_tokens_total` - 活动 ETAPI 令牌数
*   `trilium_embeddings_total` - 笔记嵌入（如果可用）

### **分类指标（Categorized Metrics）**

*   `trilium_notes_by_type{type="text|code|image|file"}` - 按类型统计的笔记
*   `trilium_attachments_by_type{mime_type="..."}` - 按 MIME 类型统计的附件

### **统计信息（Statistics）**

*   `trilium_database_size_bytes` - 数据库大小（字节）
*   `trilium_oldest_note_timestamp` - 最旧笔记的时间戳
*   `trilium_newest_note_timestamp` - 最新笔记的时间戳
*   `trilium_last_modified_timestamp` - 最后修改时间戳

## **Prometheus 配置**

添加到你的 `prometheus.yml`：

```
scrape_configs:
  - job_name: 'trilium'
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: '/etapi/metrics'
    bearer_token: 'YOUR_ETAPI_TOKEN'
    scrape_interval: 30s

```

## **错误响应（Error Responses）**

*   `400` - 无效的格式参数
*   `401` - 缺失或无效的 ETAPI 令牌
*   `500` - 内部服务器错误

## **Grafana 仪表盘（Grafana Dashboard）**

<figure class="image"><img style="aspect-ratio:2594/1568;" src="1_Metrics_image.png" width="2594" height="1568"></figure>

你也可以使用为 TriliumNext 创建的 Grafana 仪表盘——只需从<a class="reference-link" href="#root/_help_bOP3TB56fL1V">grafana-dashboard.json</a>获取 JSON，然后按照这些截图导入仪表盘：

<figure class="image"><img style="aspect-ratio:1881/282;" src="2_Metrics_image.png" width="1881" height="282"></figure>

然后粘贴 JSON 并点击加载（load）：

<figure class="image"><img style="aspect-ratio:1055/830;" src="Metrics_image.png" width="1055" height="830"></figure>""",

"RC8UiNCM0fHL": """_文件（File）_笔记类型可用于附加各种外部文件，例如图像、视频或 PDF 文档。

## 上传文件

由于这些文件来自外部来源，因此无法直接创建_文件（File）_笔记类型：

*   将文件拖入<a class="reference-link" href="#root/_help_oPVyFC7WL2Lp">笔记树（Note Tree）</a>。
*   右键单击笔记并选择_导入到笔记（Import into note）_并将其指向受支持的文件之一。

## 支持的文件类型

### PDF

PDF 可以上传到 Trilium，它们将显示实时预览，并完整支持各种功能，例如目录、批注，以及记住最后阅读的页面。更多信息请参见专门的<a class="reference-link" href="#root/_help_XJGJrpu7F9sh">PDF</a>页面。

### Office 文档

Trilium 为 Office 文档提供预览，例如 Word（`.docx`）、Excel（`.xlsx`）和 PowerPoint（`.pptx`）。更多信息请参见专门的<a class="reference-link" href="#root/_help_KG48zgNY31NW">Office 文档（Office documents）</a>。

### 图像

<figure class="image image-style-align-center image_resized" style="width:50%;"><img style="aspect-ratio:879/766;" src="2_File_image.png" width="879" height="766"></figure>

交互：

*   _复制引用到剪贴板（Copy reference to clipboard）_，用于在<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>笔记中嵌入图像。
    *   更多信息参见<a class="reference-link" href="#root/_help_0Ofbk1aSuVRu">图像引用（Image references）</a>。
    *   或者，按下<a class="reference-link" href="#root/_help_XpOYSgsLkTJy">浮动按钮（Floating buttons）</a>中对应的按钮。

### 视频

请参阅<a class="reference-link" href="#root/_help_AjqEeiDUOzj4">音频和视频（Audio & Video）</a>。

### 音频

<figure class="image image-style-align-center image_resized" style="width:50%;"><img style="aspect-ratio:850/243;" src="1_File_image.png" width="850" height="243"></figure>

添加受支持的音频文件将显示一个可以播放它的基本音频播放器。

交互：

*   可以使用专用按钮播放/暂停音频。
*   在进度条上拖动鼠标或点击进度条将快进歌曲。
*   可以设置音量。
*   可以通过音量旁边的上下文菜单调整播放速度。

### 文本文件

<figure class="image image-style-align-center image_resized" style="width:50%;"><img style="aspect-ratio:926/347;" src="File_image.png" width="926" height="347"></figure>

被识别为包含文本的文件将显示其内容的预览。此类文件的一个常见用例是嵌入内容不一定对用户感兴趣的文本文件，例如第三方库或生成的内容，如果需要可以下载。

请注意，文本文件通常会被[导入](#root/_help_mHbBMPDPkVV5)为<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>或<a class="reference-link" href="#root/_help_6f9hih2hXXZk">代码（Code）</a>笔记。要绕过此行为并创建_文件（File）_笔记类型，请使用_导入到笔记（Import into note）_功能并取消勾选_将 HTML、Markdown 和 TXT 作为文本笔记导入（Import HTML, Markdown and TXT as text notes）_，以及_将已识别的代码文件作为代码笔记导入（Import recognized code files as code notes）_。

由于使用文件而不是笔记的用例之一是显示大文件，内容预览限制为相对较少的字符。要查看完整文件，请考虑在外部应用程序中打开它。

### GPS 轨迹

Trilium 显示 `.gpx` 格式的 GPS 轨迹信息，例如距离、时长、海拔剖面、轨迹、标记。

当 `.gpx` 笔记被放入<a class="reference-link" href="#root/_help_81SGnPGMk7Xc">地理图（Geo Map）</a>时，轨迹本身也会显示在地图上。

<figure class="image"><img style="aspect-ratio:1500/807;" src="4_File_image.png" width="1500" height="807"></figure>

### 未知文件类型

<figure class="image image-style-align-center image_resized" style="width:50%;"><img style="aspect-ratio:532/240;" src="3_File_image.png" width="532" height="240"></figure>

如果文件无法被识别为上述任何受支持的文件类型，它将被视为未知文件。在这种情况下，所有默认交互都可用，例如下载或在外部打开文件，但不会有内容预览。

## 交互

*   无论文件类型如何，在<a class="reference-link" href="#root/_help_BlN9DFI679QC">功能区（Ribbon）</a>的_图片（Image）_或_文件（File）_选项卡中都会显示一系列按钮。
    *   _下载（Download）_，将下载文件供本地使用。
    *   _打开（Open）_，将使用系统默认应用程序打开文件。
    *   上传新修订以用新文件替换旧文件。
*   无法更改_文件（File）_笔记的笔记类型。
*   从[笔记菜单](#root/_help_8YBEPzcpUgxw)转换为[附件](#root/_help_0vhv7lsOLy82)。

## 与其他笔记的关系

*   文件也会根据其类型显示在<a class="reference-link" href="#root/_help_0ESUbbAxVnoK">笔记列表（Note List）</a>中：
    
    <img class="image_resized" style="aspect-ratio:853/315;width:50%;" src="5_File_image.png" width="853" height="315">
*   非图像文件可以通过<a class="reference-link" href="#root/_help_nBAXQFj20hS1">包含笔记（Include Note）</a>功能作为只读小组件嵌入到文本笔记中。
*   图像文件可以通过<a class="reference-link" href="#root/_help_0Ofbk1aSuVRu">图像引用（Image references）</a>像普通图像一样嵌入到文本笔记中。

## 文件大小限制

单个文件不能大于 374 MiB。更多信息请参阅<a class="reference-link" href="#root/_help_mHbBMPDPkVV5">导入与导出（Import & Export）</a>中的_最大导入大小（Maximum import size）_。""",
}
