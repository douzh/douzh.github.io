# -*- coding: utf-8 -*-
# 安装章节叶子双语→纯中文 批次1（桌面安装5篇 + 服务器安装7篇）
TRANS = {

"aIwhuIM42UXZ": """有时，部署[专用服务器安装](#root/_help_WOcw2SLH6tbX)并不可行。桌面应用默认自带一个功能完整的服务器实例。

出于安全原因，该功能**默认未启用**。要启用它，请在<a class="reference-link" href="#root/_help_fiHicjpHjIRJ">安全（Security）</a>中启用_网络访问（Network access）_。

启用网络访问并重启应用后，你可以在本机通过访问 [http://localhost:37840/login](http://localhost:37840/login) 打开该 Web 界面。

> [!NOTE]
> 桌面应用内嵌的服务器只在桌面应用运行时才运行。关闭应用也会关闭服务器。要解决此问题，可以尝试将应用隐藏到系统托盘。

## 移动界面

默认情况下，即使在移动设备上也会显示桌面用户界面。要切换到移动版，只需进入<a class="reference-link" href="#root/_help_x3i7MxGccDuM">全局菜单（Global menu）</a>并选择"切换到移动版"。

## 在 Windows 上通过 Windows Defender 防火墙开放外部端口

首先，在本地终端运行 `ipconfig` 查出桌面服务器的 IP。然后在另一台设备上尝试访问 `http://<ip>:37840/login`。如果访问不了，很可能是操作系统防火墙拦截了该端口。

如果你使用的是 Windows Defender 防火墙：

1.  进入 Windows 开始菜单，搜索"高级安全 Windows Defender 防火墙"。
2.  在左侧树中进入"入站规则"，然后在右侧"操作"侧栏中选择"新建规则"。
3.  选择"端口"并点击"下一步"。
4.  在"特定本地端口"中输入 `37840`，然后点击"下一步"。
5.  保持勾选"允许连接"并点击"下一步"。
6.  配置要应用的网络（不确定则全部勾选），然后点击"下一步"。
7.  为规则添加合适的名称（如"Trilium Notes"）并点击"完成"。

> [!WARNING]
> 自 v0.104.0 起，Trilium 使用的端口仅在 `localhost` 上可访问。如需在本地网络中启用，请参见<a class="reference-link" href="#root/_help_swSFivWk6KkA">网络访问（Network Access）</a>。""",

"a3TKVvk1Jcuq": """Trilium 桌面版支持全部三种主流操作系统：

*   Windows
    *   官方支持 Windows 11。
    *   也支持 Windows on ARM。
*   Linux：
    *   支持大多数现代发行版，包括 NixOS。
    *   支持 `aarch64` 架构的 ARM（不支持 ARM v7）。
*   macOS
    *   最低支持的操作系统：macOS Monterey
    *   同时支持 Intel 和 Apple Silicon 设备。""",

"KENrlyFaUbZN": """在 v0.104.0 之前，[桌面应用](#root/_help_poXkQfguuA0U)会开放一个网络端口，以便提供对<a class="reference-link" href="#root/_help_pgxEVkzLl1OP">ETAPI（REST API）</a>的访问，甚至将其用作 Web 服务器（参见<a class="reference-link" href="#root/_help_nRqcgfTb97uV">将桌面应用用作服务器</a>）。

为缩小攻击面，Trilium 现在只在本机（如 `localhost`）启用这些服务，而不再通过局域网对外提供。

为更好理解受影响的范围，请参考下表：

| 功能 | 网络访问关闭 | 网络访问开启 |
| --- | --- | --- |
| <a class="reference-link" href="#root/_help_MtPxeAWVAzMg">网页剪藏（Web Clipper）</a> | 🔒️ 仅 `localhost`（同一设备上的浏览器扩展仍可访问） | 🌐 `localhost` + 局域网 |
| <a class="reference-link" href="#root/_help_pgxEVkzLl1OP">ETAPI（REST API）</a> | 🔒️ 仅 `localhost` | 🌐 `localhost` + 局域网 |
| [LLM MCP](#root/_help_GBBMSlVSOIGP)（仅在设置中启用时） | 🔒️ 仅 `localhost`（需鉴权） | 🌐 `localhost` + 局域网（需鉴权） |
| [网页应用](#root/_help_nRqcgfTb97uV) | ❌️ 完全禁用（403），仅桌面应用可用。 | 🌐 `localhost` + 局域网 |
| <a class="reference-link" href="#root/_help_R9pX4DGra2Vt">分享（Sharing）</a>笔记 | ❌️ 完全禁用（403）；若你在使用<a class="reference-link" href="#root/_help_cbkrhQjrkKrh">同步（Synchronization）</a>，该功能仍可作为[服务器](#root/_help_WOcw2SLH6tbX)的一部分正常工作。 | 🌐 `localhost` + 局域网 |""",

"3GTPwnM0LXzq": """> [!NOTE]
> 自动启动与更好的系统托盘集成自 v0.104.0 起引入。此前的版本只有系统托盘选项，可在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _其他（Others）_ 中找到。

## 托盘图标

<figure class="image image-style-align-right"><img style="aspect-ratio:332/71;" src="Tray icon &amp; automatic startup_image.png" width="332" height="71"></figure>

桌面应用在所有操作系统上都与系统托盘进行了原生集成。

托盘图标默认启用，可在<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _桌面（Desktop）_ 中开关。

托盘图标具有以下功能：

*   单击时，最近的窗口会被隐藏（最小化到托盘图标）。再次单击将重新显示该窗口。
*   右键单击显示以下选项：
    *   每个窗口可按其当前激活的笔记进行识别，并可单独显示或隐藏。
    *   可以打开新窗口。
    *   可以直接创建新笔记，会创建在<a class="reference-link" href="#root/_help_S0Fos78ZfcY7">笔记收件箱（Note Inbox）</a>中（如果没有收件箱，则创建在<a class="reference-link" href="#root/_help_l0tKav7yLHGF">日记（Day Notes）</a>）。
    *   可以打开今天的每日笔记。
    *   书签与最近笔记显示在子菜单中，点击即可跳转到对应笔记。
    *   退出应用。

### 关闭到系统托盘

这是一个默认未启用的选项，它允许一种特殊行为：当最后一个窗口关闭时不再退出应用，而是将最后一个窗口隐藏，同时托盘图标保持可用。

此选项要求托盘图标已启用，否则不生效。

## 自动启动

<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _启动（Startup）_ 中有两个选项控制自动启动功能：

*   启用_登录时启动（Launch on startup）_后，应用会在当前用户登录时自动启动。
    *   请注意，在 Linux 上该支持取决于桌面环境。如有任何问题，欢迎[提交报告](#root/_help_wy8So3yZZlH9)。
*   如果同时启用_最小化到托盘启动（Start minimized to tray）_，应用将在后台启动，并可从托盘图标重新显示。
    *   这仅在启用_登录时启动_时生效，手动启动不受此选项影响。""",

"I9TEAj9A5NIv": """## Trilium 只显示空白窗口

Trilium 使用基于 Chromium（Google Chrome 的开源替代品）的 Electron；为提高性能，它使用了_GPU 加速_。在某些情况下，GPU 不兼容（尤其是在较老型号的集成显卡上）会导致什么都无法渲染，最终表现为空白窗口。

要绕过此问题，请尝试使用以下命令行参数运行 Trilium：

```
trilium --disable-gpu
```

如果仍不行，也请尝试以下命令：

*   `trilium --disable-gpu-compositing`
*   `ELECTRON_OZONE_PLATFORM_HINT=x11 trilium`（这条会在 Wayland 上强制使用 X11 渲染）

如果这样 Trilium 能正常渲染，Trilium 还提供了内置选项来禁用 GPU 加速：进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _外观（Appearance）_，找到_性能（Performance）_部分，然后取消勾选_硬件加速（GPU）（Hardware acceleration (GPU)）_。""",

"sPaAXjVcvZZl": """这本质上就是把 Trilium 源码 + node 模块 + node.js 运行时打包进一个 7z 文件。

## 步骤

*   登录你的服务器
*   使用 `wget`（或 `curl`）在你的服务器上下载最新的 `TriliumNotes-Server-[VERSION]-linux-x64.tar.xz`（从[发布页](https://github.com/TriliumNext/Trilium/releases)复制链接，注意 `-Server` 后缀）。
*   解压归档，例如使用 `tar -xf -d TriliumNotes-Server-[VERSION]-linux-x64.tar.xz`
*   `cd trilium-linux-x64-server`
*   `./trilium.sh`
*   打开浏览器访问 http://\[你的服务器主机名\]:8080，你应该会看到 Trilium 初始化页面

上述步骤的问题在于，一旦你关闭 SSH 连接，Trilium 进程就会终止。要避免这一点，你有两个选择：

*   先结束它（例如用 <kbd>Ctrl</kbd> + <kbd>C</kbd>），再这样运行：`nohup ./trilium.sh &`。（nohup 让进程在后台持续运行，`&` 也使其在后台运行）
*   配置 systemd，让 Trilium 在每次开机时自动在后台运行

## 用 systemd 配置 Trilium 开机自启

*   下载后，解压并移动 Trilium：

```
tar -xvf TriliumNotes-Server-[VERSION]-linux-x64.tar.xz
sudo mv trilium-linux-x64-server /opt/trilium
```

*   创建服务：

```
sudo nano /etc/systemd/system/trilium.service
```

*   将以下内容粘贴到文件中（按需替换 user 和 group）：

```
[Unit]
Description=Trilium Daemon
After=syslog.target network.target

[Service]
User=xxx
Group=xxx
Type=simple
ExecStart=/opt/trilium/trilium.sh
WorkingDirectory=/opt/trilium/

TimeoutStopSec=20
# KillMode=process leads to error, according to https://www.freedesktop.org/software/systemd/man/systemd.kill.html
Restart=always

[Install]
WantedBy=multi-user.target
```

*   保存文件（CTRL-S）并退出（CTRL-X）
*   启用并启动服务：

```
sudo systemctl enable --now -q trilium
```

*   现在你可以打开浏览器访问 http://\[你的服务器主机名\]:8080，应该会看到 Trilium 初始化页面。

## 服务器的简单自动更新

以 Trilium 运行时的同一用户运行

如果你以 root 运行，请从命令中移除 'sudo'

需要 "jq" `apt install jq`

它会停止上面的服务、覆盖所有内容（我假设没有 config.ini），然后启动服务。它还会在 Trilium 目录中创建版本文件，因此只会更新到更新的版本

```
#!/bin/bash

# Configuration
REPO="TriliumNext/Trilium"
PATTERN="TriliumNotes-Server-.*-linux-x64.tar.xz"
DOWNLOAD_DIR="/var/tmp/trilium_download"
OUTPUT_DIR="/opt/trilium"
SERVICE_NAME="trilium"
VERSION_FILE="$OUTPUT_DIR/version.txt"

# Ensure dependencies are installed
command -v curl >/dev/null 2>&1 || { echo "Error: curl is required"; exit 1; }
command -v jq >/dev/null 2>&1 || { echo "Error: jq is required"; exit 1; }
command -v tar >/dev/null 2>&1 || { echo "Error: tar is required"; exit 1; }

# Create download directory
mkdir -p "$DOWNLOAD_DIR" || { echo "Error: Cannot create $DOWNLOAD_DIR"; exit 1; }

# Get the latest release version
LATEST_VERSION=$(curl -sL https://api.github.com/repos/$REPO/releases/latest | jq -r '.tag_name')
if [ -z "$LATEST_VERSION" ]; then
  echo "Error: Could not fetch latest release version"
  exit 1
fi

# Check current installed version (from version.txt or existing tarball)
CURRENT_VERSION=""
if [ -f "$VERSION_FILE" ]; then
  CURRENT_VERSION=$(cat "$VERSION_FILE")
elif [ -f "$DOWNLOAD_DIR/TriliumNotes-Server-$LATEST_VERSION-linux-x64.tar.xz" ]; then
  CURRENT_VERSION="$LATEST_VERSION"
fi

# Compare versions
if [ "$CURRENT_VERSION" = "$LATEST_VERSION" ]; then
  echo "Latest version ($LATEST_VERSION) is already installed"
  exit 0
fi

# Download the latest release
LATEST_URL=$(curl -sL https://api.github.com/repos/$REPO/releases/latest | jq -r ".assets[] | select(.name | test(\"$PATTERN\")) | .browser_download_url")
if [ -z "$LATEST_URL" ]; then
  echo "Error: No asset found matching pattern '$PATTERN'"
  exit 1
fi

FILE_NAME=$(basename "$LATEST_URL")
FILE_PATH="$DOWNLOAD_DIR/$FILE_NAME"

# Download if not already present
if [ -f "$FILE_PATH" ]; then
  echo "Latest release $FILE_NAME already downloaded"
else
  curl -LO --output-dir "$DOWNLOAD_DIR" "$LATEST_URL" || { echo "Error: Download failed"; exit 1; }
  echo "Downloaded $FILE_NAME to $DOWNLOAD_DIR"
fi

# Extract the tarball
EXTRACT_DIR="$DOWNLOAD_DIR/extracted"
mkdir -p "$EXTRACT_DIR"
tar -xJf "$FILE_PATH" -C "$EXTRACT_DIR" || { echo "Error: Extraction failed"; exit 1; }

# Find the extracted directory (e.g., TriliumNotes-Server-0.97.2-linux-x64)
INNER_DIR=$(find "$EXTRACT_DIR" -maxdepth 1 -type d -name "TriliumNotes-Server-*-linux-x64" | head -n 1)
if [ -z "$INNER_DIR" ]; then
  echo "Error: Could not find extracted directory matching TriliumNotes-Server-*-linux-x64"
  exit 1
fi

# Stop the trilium-server service
if systemctl is-active --quiet "$SERVICE_NAME"; then
  echo "Stopping $SERVICE_NAME service..."
  sudo systemctl stop "$SERVICE_NAME" || { echo "Error: Failed to stop $SERVICE_NAME"; exit 1; }
fi

# Copy contents to /opt/trilium, overwriting existing files
echo "Copying contents from $INNER_DIR to $OUTPUT_DIR..."
sudo mkdir -p "$OUTPUT_DIR"
sudo cp -r "$INNER_DIR"/* "$OUTPUT_DIR"/ || { echo "Error: Copy failed"; exit 1; }
echo "$LATEST_VERSION" | sudo tee "$VERSION_FILE" >/dev/null
echo "Files copied to $OUTPUT_DIR"

# Start the trilium-server service
echo "Starting $SERVICE_NAME service..."
sudo systemctl start "$SERVICE_NAME" || { echo "Error: Failed to start $SERVICE_NAME"; exit 1; }

# Clean up
rm -rf "$EXTRACT_DIR"
echo "Cleanup complete. Trilium updated to $LATEST_VERSION."
```

## 常见问题

### glibc 过旧

```
Error: /usr/lib64/libstdc++.so.6: version `GLIBCXX_3.4.21' not found (required by /var/www/virtual/.../node_modules/@mlink/scrypt/build/Release/scrypt.node)
    at Object.Module._extensions..node (module.js:681:18)
    at Module.load (module.js:565:32)
    at tryModuleLoad (module.js:505:12)
```

如果你遇到这样的错误，需要升级 glibc（通常通过升级到最新的发行版版本），或改用其他[服务器安装](#root/_help_WOcw2SLH6tbX)方式。

## TLS

别忘了[配置 TLS](#root/_help_l2VkvOwUNfZj)，这是安全使用所必需的！""",

"5HwfHJFwczl9": """官方 Docker 镜像发布在 docker hub 上，提供 **AMD64**、**ARMv7** 和 **ARM64/v8** 架构：[https://hub.docker.com/r/triliumnext/trilium/](https://hub.docker.com/r/triliumnext/trilium/)

## 前置要求

确保你的系统上已安装 Docker。

如果需要安装 Docker 方面的帮助，请参考 [Docker 安装文档](https://docs.docker.com/engine/install/)。

**注意：** Trilium 的 Docker 容器需要 root 权限才能正常运行。

> [!WARNING]
> 如果你使用 SMB/CIFS 共享或文件夹作为 Trilium 数据目录，[你需要](https://github.com/TriliumNext/Notes/issues/415#issuecomment-2344824400)在挂载 SMB 共享时添加 `nobrl` 和 `noperm` 挂载选项。

## 使用 Docker Compose 运行

### 获取最新的 docker-compose.yml：

```
wget https://raw.githubusercontent.com/TriliumNext/Trilium/master/docker-compose.yml
```

可选地，编辑 `docker-compose.yml` 文件，在启动前配置容器设置。除非另行配置，数据目录将是 `~/trilium-data`，容器将在 8080 端口上可访问。

### 启动容器：

运行以下命令在后台启动容器：

```
docker compose up -d
```

## 不使用 Docker Compose 运行 / 进一步配置

### 拉取 Docker 镜像

要拉取镜像，使用以下命令，将 `[VERSION]` 替换为所需版本或标签，例如 `v0.91.6` 或直接 `latest`。（参见 [https://hub.docker.com/r/triliumnext/trilium/tags](https://hub.docker.com/r/triliumnext/trilium/tags) 上发布的标签名称。）：

```
docker pull triliumnext/trilium:v0.91.6
```

**警告：** 避免使用 "latest" 标签，因为它可能会自动将你的实例升级到新的小版本，可能破坏同步设置或引起其他问题。

### 准备数据目录

Trilium 需要在主机系统上有一个目录来存储其数据。该目录必须挂载到 Docker 容器中，并具有写入权限。

### 运行 Docker 容器

#### 仅本机访问

运行容器使其只能从 localhost 访问。此设置适合测试或在使用 Nginx 或 Apache 等代理服务器时使用。

```
sudo docker run -t -i -p 127.0.0.1:8080:8080 -v ~/trilium-data:/home/node/trilium-data triliumnext/trilium:[VERSION]
```

1.  使用 `docker ps` 验证容器正在运行。
2.  在 Web 浏览器中通过 `127.0.0.1:8080` 访问 Trilium。

#### 本地网络访问

要使容器仅在你的本地网络上可访问，先创建一个新的 Docker 网络：

```
docker network create -d macvlan -o parent=eth0 --subnet 192.168.2.0/24 --gateway 192.168.2.254 --ip-range 192.168.2.252/27 mynet
```

然后，使用网络设置运行容器：

```
docker run --net=mynet -d -p 127.0.0.1:8080:8080 -v ~/trilium-data:/home/node/trilium-data triliumnext/trilium:latest
```

要为保存的数据设置不同的用户 ID（UID）和组 ID（GID），请使用 `USER_UID` 和 `USER_GID` 环境变量：

```
docker run --net=mynet -d -p 127.0.0.1:8080:8080 -e "USER_UID=1001" -e "USER_GID=1001" -v ~/trilium-data:/home/node/trilium-data triliumnext/trilium:latest
```

使用 `docker inspect [container_name]` 查找本地 IP 地址，并从本地网络上的设备访问该服务。

```
docker ps
docker inspect [container_name]
```

#### 全局访问

要允许从任何 IP 地址访问，按如下方式运行容器：

```
docker run -d -p 0.0.0.0:8080:8080 -v ~/trilium-data:/home/node/trilium-data triliumnext/trilium:[VERSION]
```

使用 `docker stop <CONTAINER ID>` 停止容器，容器 ID 可通过 `docker ps` 获取。

### 自定义数据目录

如需自定义数据目录，请使用：

```
-v ~/YourOwnDirectory:/home/node/trilium-data triliumnext/trilium:[VERSION]
```

如果你想以非默认方式运行实例，请按如下方式使用卷切换：`-v ~/YourOwnDirectory:/home/node/trilium-data triliumnext/trilium:<VERSION>`。重要的是要了解 Docker 卷的工作方式：冒号前的路径是你自己的路径，冒号后的路径是容器内的路径。更多细节可在 [Docker 卷文档](https://docs.docker.com/storage/volumes/)中找到。

## 反向代理

1.  [Nginx](#root/_help_ud6MShXL4WpO)
2.  [Apache](#root/_help_fDLvzOx29Pfg)

### 关于 --user 指令的说明

`--user` 指令不受支持。请改用 `USER_UID` 和 `USER_GID` 环境变量来设置适当的用户和组 ID。

### 关于时区的说明

如果你遇到时区问题且未使用 docker-compose，可能需要添加带[本地时区的 TZ 标识符](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)的 `TZ` 环境变量。

## 无 root 的 Docker 镜像

> [!NOTE]
> 请注意，数据目录在 `/home/trilium/trilium-data`，而不是常见的 `/home/node/trilium-data`。这是因为在无 root 容器中创建并使用了一个新用户来运行 Trilium。

如果你希望无需以 `root` 身份运行 Docker 容器即可运行 Trilium，可以使用提供的 Debian（默认）和基于 Alpine 的镜像，带 `rootless` 标签。

_*如果你不确定，请坚持使用上面提到的 "rootful" Docker 镜像。*_

以下是一些拉取无 root 镜像的命令：

```
# 基于 Debian 的镜像
docker pull triliumnext/trilium:rootless

# 基于 Alpine 的镜像
docker pull triliumnext/trilium:rootless-alpine
```

### 为什么使用无 root？

以非 root 用户运行容器是一种安全最佳实践，可以降低容器被攻破时的潜在影响。如果攻击者设法逃出容器，他们只会拥有非 root 用户的权限，而不是对主机的完全 root 访问权限。

### 工作原理

无 root 的 Trilium 镜像：

1.  在构建时创建一个非 root 用户（`trilium`）
2.  配置应用程序以该非 root 用户身份运行
3.  允许通过 Docker 的 `--user` 标志在运行时自定义用户的 UID/GID
4.  不需要单独的 Docker `entrypoint` 脚本

### 使用方法

#### **使用 docker-compose（推荐）**

```
# 使用默认 UID/GID (1000:1000) 运行
docker-compose -f docker-compose.rootless.yml up -d

# 使用自定义 UID/GID（例如，匹配你的主机用户）
TRILIUM_UID=$(id -u) TRILIUM_GID=$(id -g) docker-compose -f docker-compose.rootless.yml up -d

# 指定自定义数据目录
TRILIUM_DATA_DIR=/path/to/your/data TRILIUM_UID=$(id -u) TRILIUM_GID=$(id -g) docker-compose -f docker-compose.rootless.yml up -d
```

#### **使用 Docker CLI**

```
# 构建镜像
docker build -t triliumnext/trilium:rootless -f apps/server/Dockerfile.rootless .

# 使用默认 UID/GID (1000:1000) 运行
docker run -d --name trilium -p 8080:8080 -v ~/trilium-data:/home/trilium/trilium-data triliumnext/trilium:rootless

# 使用自定义 UID/GID 运行
docker run -d --name trilium -p 8080:8080 --user $(id -u):$(id -g) -v ~/trilium-data:/home/trilium/trilium-data triliumnext/trilium:rootless
```

### 环境变量

*   `TRILIUM_UID`：容器进程使用的 UID（传递给 Docker 的 `--user` 标志）
*   `TRILIUM_GID`：容器进程使用的 GID（传递给 Docker 的 `--user` 标志）
*   `TRILIUM_DATA_DIR`：容器内数据目录的路径（默认：`/home/node/trilium-data`）

有关完整的环境变量配置列表（网络设置、身份验证、同步等），请参见<a class="reference-link" href="#root/_help_Gzjqa934BdH4">配置（config.ini 或环境变量）</a>。

### 卷权限

如果遇到数据卷权限问题，请确保：

1.  主机目录对你使用的 UID/GID 具有适当的权限
2.  你同时设置了 `TRILIUM_UID` 和 `TRILIUM_GID`，使其与主机目录的所有者匹配

```
# 例如，如果你的数据目录由 UID 1001 和 GID 1001 拥有：
TRILIUM_UID=1001 TRILIUM_GID=1001 docker-compose -f docker-compose.rootless.yml up -d
```

### 注意事项

*   容器以特定的 UID/GID 启动，可在运行时自定义
*   与传统的设置不同，此方法不使用带 `usermod`/`groupmod` 命令的单独入口点脚本
*   容器无法在运行时修改自己的 UID/GID，这是无 root 容器的一个安全特性

### 可用的无 root 镜像

提供两种无 root 变体：

1.  **基于 Debian**（默认）：使用 Debian Bullseye Slim 基础镜像
    *   Dockerfile：`apps/server/Dockerfile.rootless`
    *   推荐给大多数用户
2.  **基于 Alpine**：使用 Alpine 基础镜像，体积更小
    *   Dockerfile：`apps/server/Dockerfile.alpine.rootless`
    *   镜像体积更小，但可能与某些系统存在兼容性问题

### 构建自定义无 root 镜像

如果你愿意，也可以在构建时自定义 UID/GID：

```
# 使用自定义 UID/GID 构建基于 Debian 的镜像
docker build --build-arg USER=myuser --build-arg UID=1001 --build-arg GID=1001 \
  -t triliumnext/trilium:rootless-custom -f apps/server/Dockerfile.rootless .

# 使用自定义 UID/GID 构建基于 Alpine 的镜像
docker build --build-arg USER=myuser --build-arg UID=1001 --build-arg GID=1001 \
  -t triliumnext/trilium:alpine-rootless-custom -f apps/server/Dockerfile.alpine.rootless .
```

可用的构建参数：

*   `USER`：非 root 用户的用户名（默认：trilium）
*   `UID`：非 root 用户的用户 ID（默认：1000）
*   `GID`：非 root 用户的组 ID（默认：1000）""",

"SEYvbzIDbgtV": """本页介绍如何在 NixOS 上配置 Trilium 模块。

## 要求

已安装 [NixOS](https://nixos.org/)。

## 配置

在 `configuration.nix` 中添加以下内容：

```
services.trilium-server.enable = true;

# 默认数据目录：/var/lib/trilium
#services.trilium-server.dataDir = "/var/lib/trilium-sync-server";

# 默认绑定地址：127.0.0.1，端口 8080
#services.trilium-server.host = "0.0.0.0";
#services.trilium-server.port = 12783;
```

取消注释任何你想修改的选项。

更多选项（包括 nginx 反向代理配置）请参见 [NixOS 选项列表](https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=trilium-server)。""",

"fEvxRDgByx6J": """> [!WARNING]
> 本页介绍如何在服务器上手动安装 Trilium。**请注意，这不是受良好支持的安装方式，可能会出现各种问题，此处提供的信息也相当过时。建议使用** <a class="reference-link" href="#root/_help_rWX5eY045zbE">Docker 服务器安装</a> **或**<a class="reference-link" href="#root/_help_3tW6mORuTHnB">打包服务器安装</a>**。**

## 要求

Trilium 是一个 node.js 应用程序。受支持（已测试）的 node.js 版本是最新的 14.X.X 和 16.X.X。Trilium 也可能在更旧版本上运行。

你可以使用以下命令检查 node 版本（需要已安装 node.js）：

```
node --version
```

如果你的 Linux 发行版只有过时的 node.js 版本，可以查看 node.js 网站上的安装说明，其中覆盖了大多数流行的发行版。

### 依赖项

有一些必需的依赖项。以下是 Debian 及其衍生版（如 Ubuntu）的命令：

```
sudo apt install libpng16-16 libpng-dev pkg-config autoconf libtool build-essential nasm libx11-dev libxkbfile-dev
```

## 安装

### 下载

你可以从 [https://github.com/TriliumNext/Trilium/releases/latest](https://github.com/TriliumNext/Trilium/releases/latest) 下载源码 zip/tar。

要获取包含测试版的最新版本，请使用以下命令从 `main` **分支**克隆 Git 仓库：

```
git clone -b main https://github.com/triliumnext/trilium.git
```

## 安装

```
cd trilium

# 下载所有 node 依赖
npm install

# 确保 better-sqlite3 二进制文件存在
npm rebuild

# 打包并压缩前端 JavaScript
npm run webpack
```

## 运行

```
cd trilium

# 使用 nohup 确保用户注销后 trilium 仍在运行
nohup TRILIUM_ENV=dev node src/www &
```

应用默认在 8080 端口启动，因此你可以打开浏览器访问 [http://localhost:8080](http://localhost:8080) 来使用 Trilium（将 "localhost" 替换为你的主机名）。

## TLS

别忘了[配置 TLS](#root/_help_l2VkvOwUNfZj)，这是安全使用所必需的！""",
}
