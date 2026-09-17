# -*- coding: utf-8 -*-
TRANS = {
"z5TN5NCWzga3": """本指南概述了在自己的服务器上安装 Trilium 的步骤。如果你想设置[同步（synchronization）](#root/_help_cbkrhQjrkKrh)或在浏览器中使用 Trilium（可从任何地方访问），可以考虑这个选项。

## 安装选项

有几种在服务器上安装 Trilium 的方式，各有优势：

*   **推荐**：[Docker 安装](#root/_help_rWX5eY045zbE) - 适用于 **AMD64** 和 **ARM** 架构。
*   [打包的服务器安装](#root/_help_3tW6mORuTHnB)
*   [PikaPods 托管服务](https://www.pikapods.com/pods?run=trilium-next)
*   [手动安装](#root/_help_J1Bb6lVlwU5T)
*   [Kubernetes](#root/_help_DCmT6e7clMoP)
*   [Cloudron](https://www.cloudron.io/store/com.github.trilium.cloudronapp.html)
*   [HomelabOS](https://homelabos.com/docs/software/trilium/)
*   [NixOS 模块](#root/_help_moVgBcoxE3EK)

服务器安装同时包含 Web 和[移动端前端（mobile frontends）](#root/_help_RDslemsQ6gCp)。

## 配置

完成服务器安装后，你可能需要配置端口或启用 [TLS](#root/_help_l2VkvOwUNfZj) 等设置。配置通过 Trilium 的 `config.ini` 文件进行管理，该文件默认位于[数据目录（data directory）](#root/_help_tAassRL4RSQL)中。要开始自定义设置，请将随附的 `config-sample.ini` 文件（包含默认值）复制为 `config.ini`。

你也可以查看[配置（configuration）](#root/_help_Gzjqa934BdH4)文件，将所有的 `config.ini` 值作为环境变量提供。

### 配置位置

默认情况下，`config.ini`、[数据库（database）](#root/_help_wX4HbRucYSDD)和其他重要的 Trilium 数据文件存储在[数据目录（data directory）](#root/_help_tAassRL4RSQL)中。如果你更喜欢其他位置，可以通过设置 `TRILIUM_DATA_DIR` 环境变量来更改：

```
export TRILIUM_DATA_DIR=/home/myuser/data/my-trilium-data
```

### 禁用身份验证

请参阅<a class="reference-link" href="#root/_help_0hzsNCP31IAB">身份验证（Authentication）</a>。

## 反向代理设置

要为 Trilium 配置反向代理，你可以使用 **nginx** 或 **Apache**。你也可以查看反向代理文件夹中存储的文档。

### nginx

将以下配置添加到你的 `nginx` 设置中，以将请求代理到 Trilium：

```
location /trilium/ {
    proxy_pass http://127.0.0.1:8080/;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection 'upgrade';
    proxy_set_header Host $host;
    proxy_cache_bypass $http_upgrade;
}
```

为避免限制负载大小，请在 `server {}` 块中包含以下内容：

```
# 设置为 0 表示无限制。默认为 1M。
client_max_body_size 0;
```

### Apache

对于 Apache 设置，请参阅 [Apache 代理设置](#root/_help_fDLvzOx29Pfg)指南。""",

"Tt0BN2uUso39": """笔记（Note）是 Trilium 中的核心实体。笔记的主要属性是[标题（title）](#root/_help_l7pigIGu01K2)和内容。

### 笔记类型

主要的笔记类型是一种称为<a class="reference-link" href="#root/_help_iPIMuisry3hd">文本（Text）</a>的富文本笔记类型。对于图表和绘图，有<a class="reference-link" href="#root/_help_grjYqerjn243">画布（Canvas）</a>和<a class="reference-link" href="#root/_help_s1aBHPd79XYj">Mermaid 图表（Mermaid Diagrams）</a>。

还有一些更复杂的笔记类型，例如<a class="reference-link" href="#root/_help_m523cpzocqaD">已保存搜索（Saved Search）</a>、<a class="reference-link" href="#root/_help_HcABDtFCkbFN">渲染笔记（Render Note）</a>，它们通常与<a class="reference-link" href="#root/_help_CdNpE2pqjmI6">脚本（Scripting）</a>配合使用。

在 Trilium 中没有特定的"文件夹"笔记类型。任何笔记都可以有子笔记，因此可以充当文件夹。

### 根笔记

有一个特殊的笔记称为"根笔记（root note）"，它是笔记树的根。所有其他笔记都位于它下面的结构中。

### 树结构

重要的是，笔记本身不携带其在笔记树中位置的信息。详情请参阅<a class="reference-link" href="#root/_help_IakOLONlIfGI">克隆笔记（Cloning Notes）</a>。

笔记的树结构可以类似于文件系统——但与文件系统相比，Trilium 中的笔记可以同时充当文件和目录——这意味着笔记既可以有自己的内容，也可以有子笔记。"叶笔记（Leaf note）"是指没有任何子笔记的笔记。

### 删除 / 恢复删除笔记

当你在 Trilium 中删除笔记时，实际上只是标记为删除（软删除）——实际内容、标题、属性等并不会被删除，只是被隐藏。

在（默认）7 天内，可以恢复这些软删除的笔记——打开<a class="reference-link" href="#root/_help_r5JGHN99bVKn">最近更改（Recent Changes）</a>对话框，你会看到所有已修改笔记的列表，包括已删除的笔记。可恢复的笔记带有恢复链接。这类似于例如 Windows 中已知的"回收站"功能。

点击恢复将恢复笔记及其内容和属性——笔记应该与删除前完全一样。此操作还会恢复在同一操作中被删除的笔记子笔记。

要能够恢复笔记，已删除笔记的父笔记必须处于未删除状态（否则没有可恢复的位置）。当你连续删除多个笔记时，这可能成为一个问题——解决方案是按删除的相反顺序恢复。

7 天（可配置）之后，笔记将被"清除"——它们的标题、内容、修订和属性将被清除，并且将无法再恢复（除非你恢复[备份（Backup）](#root/_help_ODY7qQn5m2FT)）。

## 另请参阅

*   <a class="reference-link" href="#root/_help_CoFPLs3dRlXc">只读笔记（Read-Only Notes）</a>""",

"R3myDXkvEDLx": """<figure class="image"><img style="aspect-ratio:1990/1168;" src="1_Right Sidebar_image.png" width="1990" height="1168"></figure>

右侧边栏为当前笔记显示特定内容。侧边栏分为多个选项卡：

*   <a class="reference-link" href="#root/_help_2dcGDhp9WFKb">大纲选项卡（Outline tab）</a>，根据笔记类型显示目录、高亮和其他与导航相关的部分。
*   <a class="reference-link" href="#root/_help_uNtgN0cUKEBc">属性选项卡（Attributes tab）</a>，为当前笔记的<a class="reference-link" href="#root/_help_zEY4DaJG4YT5">属性（Attributes）</a>提供可视化编辑器。
*   <a class="reference-link" href="#root/_help_LrKznOtw9cQD">AI 对话选项卡（AI chat tab）</a>，使用<a class="reference-link" href="#root/_help_GBBMSlVSOIGP">AI</a>集成进行对话，可选择访问当前笔记。
*   <a class="reference-link" href="#root/_help_KRlZFoIH0j6p">连接选项卡（Connections tab）</a>，将笔记图、笔记路径、反向链接和相似笔记分组在一起。

> [!WARNING]
> 目前有两种类型的侧边栏；本文档仅指<a class="reference-link" href="#root/_help_IjZS7iK5EXtb">新布局（New Layout）</a>中定义的那种。旧布局有不同的侧边栏机制，只显示目录和高亮。

## 切换右侧边栏

有两种切换右侧边栏的方法：

*   通过标题栏附近的 <img src="Right Sidebar_image.png" width="25" height="21"> 图标，用于切换侧边栏是否停靠。
*   通过_切换右侧面板（Toggle right pane）_[键盘快捷键](#root/_help_A9Oc6YKKc65v)，该快捷键默认未分配。

## 偷看侧边栏

在应用右边缘附近有一个显示箭头图标的小条。

*   当侧边栏隐藏时，按下边缘将_偷看（peek）_侧边栏。
    *   在此模式下，侧边栏会临时显示在内容上方，不会移动笔记的内容。
    *   在偷看模式下点击侧边栏外部将直接隐藏侧边栏。X 按钮和点击右边缘也是如此。
    *   按下图钉图标将停靠侧边栏。
*   当侧边栏停靠时，按下边缘将隐藏侧边栏（类似于顶部的切换图标）。""",

"g0sMuS8IdnE9": """虽然 Trilium 得到积极维护且稳定，但仍有可能遇到 bug。

## 常规快速修复

故障排查的第一步通常是重启。

如果你遇到 UI 问题，前端可能已进入不一致状态。按 <kbd>Ctrl</kbd> + <kbd>R</kbd> 重新加载应用程序。这将重新加载前端。

如果问题仍然存在或似乎是后端问题，请重启整个应用程序。对于桌面（Electron）版本，只需关闭并重新打开窗口。如果你使用的是 Docker 版本，请重启容器。

## 损坏的笔记导致 Trilium 崩溃

某些问题（例如渲染带有错误脚本的笔记）可能导致 Trilium 崩溃。如果 Trilium 在重启时尝试重新加载有问题的笔记，它将继续崩溃。

要解决此问题，请使用 `TRILIUM_START_NOTE_ID` 环境变量将打开的选项卡重置为单个指定的笔记 ID（例如 `root`）。在 Linux 中，你可以这样设置：

```
TRILIUM_START_NOTE_ID=root ./trilium
```

## 损坏的脚本阻止应用程序启动

如果自定义脚本导致 Trilium 崩溃，并且它被设置为启动脚本或在活动的[自定义小组件（custom widget）](#root/_help_MgibgPcfeuGz)中，请以"安全模式"启动 Trilium，以防止任何自定义脚本执行：

```
TRILIUM_SAFE_MODE=true ./trilium
```

根据你的 Trilium 发行版，你可能已有现成的脚本可用：`trilium-safe-mode.bat` 和 `trilium-safe-mode.sh`。

Trilium 启动后，找到并修复或删除有问题的笔记。

## 同步与一致性检查

Trilium 会定期验证数据库的逻辑一致性（例如确保每个笔记都有父笔记）。如果检测到不一致，你会通过 UI 收到通知。

在这种情况下，请提交错误报告，并在必要时附上[匿名化数据库（anonymized database）](#root/_help_x59R8J8KV5Bp)。

## 恢复备份

Trilium 会定期自动备份。如果问题变得严重，你可以[从备份中恢复（restore from a backup）](#root/_help_ODY7qQn5m2FT)。

## 忘记密码

请参阅<a class="reference-link" href="#root/_help_lwAX5SvbrK2g">重置密码（Resetting your password）</a>。

## 报告错误

报告错误非常有价值。以下是一些提示：

*   使用 GitHub issues 进行报告：[https://github.com/TriliumNext/Trilium/issues](https://github.com/TriliumNext/Trilium/issues)
*   参阅[错误日志（error logs）](#root/_help_qzNzp9LYQyPT)页面，了解提供必要详细信息的方法。""",
}
