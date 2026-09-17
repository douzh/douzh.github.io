# -*- coding: utf-8 -*-
# 安装章节叶子双语→纯中文 批次2（服务器安装12篇）
TRANS = {

"aFYvSvzqySw1": """由于 Trilium 可以在 Docker 中运行，因此也可以部署在 Kubernetes 中。你可以使用我们的 Helm chart、社区 Helm chart，或自行搭建 Kubernetes 部署。

推荐的方式是使用 Helm chart。

## root 权限

> [!NOTE]
> Trilium 容器目前需要以 root 权限运行。不过它在执行后会切换到 UID 和 GID `1000:1000` 来运行 `node` 进程，因此主进程不会以 root 权限运行。

Trilium docker 容器需要以 root 权限运行。容器内的 node 进程会在一些初始化逻辑后以降低的权限（uid:gid 1000:1000）启动。请确保不要使用会更改用户 ID 的安全上下文（PodSecurityContext）。如需为文件存储和应用使用不同的 uid:gid，请使用 `USER_UID` 和 `USER_GID` 环境变量。

docker 镜像还会修复 `/home/node` 的权限，因此你不必使用 init 容器。

## Helm Charts

TriliumNext 的[官方 Helm chart](https://github.com/TriliumNext/helm-charts)。[ohdearaugustin](https://github.com/ohdearaugustin) 的社区 Helm chart：[https://github.com/ohdearaugustin/charts](https://github.com/ohdearaugustin/charts)

## 添加 Helm 仓库

以下是一个示例：

```
helm repo add trilium https://triliumnext.github.io/helm-charts
"trilium" has been added to your repositories
```

## 如何安装 chart

在查看 Helm chart 的 [`values.yaml`](https://github.com/TriliumNext/helm-charts/blob/main/charts/trilium/values.yaml)、按需修改并创建你自己的之后：

```
helm install --create-namespace --namespace trilium trilium trilium/trilium -f values.yaml
```

更多关于使用 Helm 的信息，请参阅 Helm 文档，或在 TriliumNext GitHub 组织中发起 Discussion。""",

"fupEm5MLR0eF": """Trilium 不支持多用户。为了让两个或更多人有各自独立的笔记集合，必须设置多个服务器实例。也无法使用多个[同步](#root/_help_cbkrhQjrkKrh)服务器。

要在同一台物理服务器上运行多个服务器实例：

*   对于<a class="reference-link" href="#root/_help_3tW6mORuTHnB">Linux 打包版本</a>或<a class="reference-link" href="#root/_help_J1Bb6lVlwU5T">手动安装</a>，如果手动启动服务器，只需为每个实例指定不同的端口和数据目录：

    ```
    TRILIUM_NETWORK_PORT=8080 TRILIUM_DATA_DIR=/path/to/your/data-dir-A /opt/trilium/trilium.sh
    ```

    对于第二个实例：

    ```
    TRILIUM_NETWORK_PORT=8081 TRILIUM_DATA_DIR=/path/to/your/data-dir-B /opt/trilium/trilium.sh
    ```

    如果使用 `systemd`，则在[服务配置中设置环境变量](https://serverfault.com/questions/413397/how-to-set-environment-variable-in-systemd-service)。
*   对于<a class="reference-link" href="#root/_help_rWX5eY045zbE">使用 Docker</a>，只需使用两个不同的容器，各自绑定自己的端口和数据目录。
*   对于<a class="reference-link" href="#root/_help_moVgBcoxE3EK">在 NixOS 上</a>，唯一可行的方法是使用 Docker OCI 容器，或至少一个带自己服务定义的 NixOS 容器。

如需支持或更多背景信息，请参见相关的 [GitHub Discussion](https://github.com/orgs/TriliumNext/discussions/1642#discussioncomment-12768808)。""",

"O4yjkaVH3YxV": """Trilium 通常通过**反向代理**暴露到互联网，反向代理负责 TLS 终止、提供单一公共入口点，并且（可选地）让你可以在一个域名下托管多个实例。Trilium 本身在其本地端口上以普通 HTTP 运行；代理位于其前面。

本节汇集了特定提供商的指南，以及无论使用哪种代理都适用的配置。

## 提供商指南

*   <a class="reference-link" href="#root/_help_ud6MShXL4WpO">Nginx</a>
*   <a class="reference-link" href="#root/_help_5ERVJb9s4FRD">Traefik</a>
*   <a class="reference-link" href="#root/_help_fDLvzOx29Pfg">Apache using Docker</a>

## 适用于每个代理

无论你选择哪个代理，请确保以下事项：

*   **转发 WebSocket 升级。** Trilium 使用 WebSocket 进行同步和实时 UI 更新。你的代理必须透传 `Upgrade` 和 `Connection` 头，否则客户端看起来已加载，但永远不会收到更新。
*   **配置受信任的代理。** 这样 Trilium 才能看到真实的客户端 IP（用于速率限制、日志和安全 cookie），而不是代理的地址。参见<a class="reference-link" href="#root/_help_LLzSMXACKhUs">受信任的代理（Trusted proxy）</a>。
*   **在代理处终止 TLS。** 参见<a class="reference-link" href="#root/_help_l2VkvOwUNfZj">HTTPS (TLS)</a>。

## 在 Trilium 前放置认证代理

某些设置会在代理处放置一个**前向认证 / SSO 层**（Authelia、Authentik、oauth2-proxy 或 OIDC 插件），使_每个_请求都必须先通过交互式登录。

这会**破坏同步和初始设置。** 同步是机器对机器的过程，在后台无人值守运行——它没有浏览器，无法完成交互式登录挑战。当代理拦截同步请求时，它返回的是自己的 HTML 登录页而不是预期的 JSON，客户端会报错，例如：

```
Sync failed: Request to GET https://trilium.example.com/api/setup/status failed,
error: Unexpected token '<', "<!doctype "... is not valid JSON

```

有两种方法可以解决：

*   **推荐做法，使用 Trilium 的内置认证。** 不要在代理处对 Trilium 做任何拦截。Trilium 可以自己认证 Web 界面，包括通过 [OpenID Connect](#root/_help_x1lQNKQBqLcA)。这样同步端点可以按设计的方式自行认证。
*   **将同步端点从代理的认证中排除。** 如果必须把认证放在代理处，请将这些路径从前向认证检查中排除：
    *   `/api/setup/*`
    *   `/api/login/sync`
    *   `/api/sync/*`

> [!NOTE]
> 这是安全的：Trilium 自己认证这些端点。同步使用基于共享密钥的无挑战 HMAC 握手，一次性设置种子请求受密码保护——因此在代理处排除它们不会暴露对你的数据的未授权访问。

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

    # 其他所有请求都经过前向认证。
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

"o5byGpiYwkAf": """配置 Nginx 代理与 HTTPS。本文以 Ubuntu 操作系统为例。

## 安装 Nginx

下载 Nginx 并移除 Apache2

```
sudo apt-get install nginx
sudo apt-get remove apache2
```

## 构建配置文件

1.  首先，创建配置文件：

    ```
    cd /etc/nginx/conf.d
    vim default.conf
    ```
2.  将下面所示内容填入文件，其中部分设置需要修改。然后你就能享受强制 HTTPS 与代理的 Web 服务了。

    ```
    # This part configures, where your Trilium server is running
    upstream trilium {
      zone trilium 64k;
      server 127.0.0.1:8080; # change it to a different hostname and port if non-default is used
      keepalive 2;
    }

    # This part is for proxy and HTTPS configure
    server {
        listen 443 ssl;
        server_name trilium.example.net; #change trilium.example.net to your domain without HTTPS or HTTP.
        ssl_certificate /etc/ssl/note/example.crt; #change /etc/ssl/note/example.crt to your path of crt file.
        ssl_certificate_key /etc/ssl/note/example.net.key; #change /etc/ssl/note/example.net.key to your path of key file.
        ssl_session_cache builtin:1000 shared:SSL:10m;
        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        ssl_ciphers HIGH:!aNULL:!eNULL:!EXPORT:!CAMELLIA:!DES:!MD5:!PSK:!RC4;
        ssl_prefer_server_ciphers on;
        access_log /var/log/nginx/access.log; #check the path of access.log, if it doesn't fit your file, change it

        location / {
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_pass http://trilium;
            proxy_read_timeout 90;
        }
    }

    # This part is for HTTPS forced
    server {
        listen 80;
        server_name trilium.example.net; # change to your domain
        return 301 https://$server_name$request_uri;
    }
    ```

## 在不同路径下提供服务

或者，如果你想在另一个路径下提供服务（例如同时服务多个实例），请这样更新 location 块：

*   将 location 更新为你想要的路径（如果你的 `proxy_pass` 不以斜杠结尾，请确保不要留下尾部斜杠 "/"）
*   添加带相同路径的 `proxy_cookie_path` 指令：这允许你在多个实例中同时保持登录状态。

```
    location /trilium/instance-one {
        rewrite /trilium/instance-one/(.*) /$1  break;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_pass http://trilium;
        proxy_cookie_path / /trilium/instance-one
        proxy_read_timeout 90;
    }
```

## 配置受信任的代理

设置反向代理后，请确保配置<a class="reference-link" href="#root/_help_LLzSMXACKhUs">受信任的代理（Trusted proxy）</a>。""",

"74oHxA6XDX7Z": """本教程假设你已经为 `trilium.yourdomain.com` 创建了 DNS A 记录，用于你的 Trilium 服务器。

## Docker 设置

下载 docker 镜像并创建容器

```
 docker pull triliumnext/trilium:[VERSION]
 docker create --name trilium -t -p 127.0.0.1:8080:8080 -v ~/trilium-data:/home/node/trilium-data triliumnext/trilium:[VERSION]
```

## 配置 Apache 代理

1.  启用 apache 代理模块

    ```
     a2enmod ssl
     a2enmod proxy
     a2enmod proxy_http
     a2enmod proxy_wstunnel
    ```
2.  创建新的 let's encrypt 证书

    ```
     sudo certbot certonly -d trilium.mydomain.com
    ```

    选择 standalone（2）并记下所创建证书的位置（通常在 /etc/letsencrypt/live/...）
3.  为 apache 创建新的虚拟主机文件（你可能想用 `apachectl -S` 确定服务器根位置，我的是 /etc/apache2）

    ```
     sudo nano /etc/apache2/sites-available/trilium.yourdomain.com.conf
    ```

    将以下文本粘贴（并自定义）到配置文件中

    ```
     
         ServerName http://trilium.yourdomain.com
         RewriteEngine on
             RewriteRule ^ https://%{SERVER_NAME}%{REQUEST_URI} [END,QSA,R=permanent]
     
     
         ServerName https://trilium.yourdomain.com
         RewriteEngine On
         RewriteCond %{HTTP:Connection} Upgrade [NC]
         RewriteCond %{HTTP:Upgrade} websocket [NC]
         RewriteRule /(.*) ws://localhost:8080/$1 [P,L]
         AllowEncodedSlashes NoDecode
         ProxyPass / http://localhost:8080/ nocanon
         ProxyPassReverse / http://localhost:8080/
         SSLCertificateFile /etc/letsencrypt/live/trilium.yourdomain.com/fullchain.pem
         SSLCertificateKeyFile /etc/letsencrypt/live/trilium.yourdomain.com/privkey.pem
         Include /etc/letsencrypt/options-ssl-apache.conf
     
    ```
4.  使用 `sudo a2ensite trilium.yourdomain.com.conf` 启用虚拟主机
5.  使用 `sudo systemctl reload apache2` 重新加载 apache2

## 配置受信任的代理

设置反向代理后，请确保配置<a class="reference-link" href="#root/_help_LLzSMXACKhUs">受信任的代理（Trusted proxy）</a>。

## 设置 systemd 服务以启动服务器

创建并启用一个 systemd 服务，让 docker 容器在开机时启动

1.  创建名为 `/lib/systemd/system/trilium.service` 的新空文件，内容如下

    ```
     [Unit]
     Description=Trilium Server
     Requires=docker.service
     After=docker.service
    
     [Service]
     Restart=always
     ExecStart=/usr/bin/docker start -a trilium
     ExecStop=/usr/bin/docker stop -t 2 trilium
    
     [Install]
     WantedBy=local.target
    ```
2.  安装、启用并启动服务

    ```
     sudo systemctl daemon-reload
     sudo systemctl enable trilium.service
     sudo systemctl start trilium.service
    ```""",

"9H8FKhPVYLTX": """如果你在[反向代理](#root/_help_vcjrb3VVYPZI)下运行 Trilium 服务器，将其配置为受信任的代理非常重要，这样应用程序才能正确识别客户端的真实 IP 地址（用于身份验证和速率限制）。

要做到这一点，只需修改<a class="reference-link" href="#root/_help_Gzjqa934BdH4">配置（config.ini 或环境变量）</a>并设置：

```
[Network]
trustedReverseProxy=true
```

这将使用 `X-Forwarded-For` 头中最左边的 IP。或者，也可以不用 `true`，而是使用反向代理的 IP 地址或 Express.js 快捷方式，例如：

```
loopback(127.0.0.1/8, ::1/128), linklocal(169.254.0.0/16, fe80::/10), uniquelocal(10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, fc00::/7)
```

更多信息请查阅 [Express behind proxies](https://expressjs.com/en/guide/behind-proxies.html)。""",

"mWpKWRZZLu3R": """本文的目标是配置 Traefik 代理和 HTTPS。参见 [#7768](https://github.com/TriliumNext/Trilium/issues/7768#issuecomment-3539165814) 作为参考。

## Traefik 3.6.4 中的破坏性变更

Traefik 3.6.4 引入了一个关于 URL 中百分号编码字符处理的[破坏性变更](https://doc.traefik.io/traefik/migrate/v3/#encoded-characters-in-request-path)。具体来说，Trilium 使用的某些 URL（例如 `search/%23workspace%20%23!template`）会被 Traefik 自动拒绝，导致 HTTP 400 错误。

要解决这个问题，必须修改 Traefik 的[**静态**配置](https://doc.traefik.io/traefik/getting-started/configuration-overview/#the-install-configuration)以允许这些字符：

```yaml
entryPoints:
  web:
    http:
      encodedCharacters:
        allowEncodedSlash: true
        allowEncodedHash: true
```

> [!TIP]
> 如果仍有问题，取决于 Trilium 的使用方式（尤其是搜索），你可能需要启用更多编码字符组。更多信息请参见[相关 GitHub issue](https://github.com/TriliumNext/Trilium/issues/7968)；欢迎报告你的发现。

### 构建 docker-compose 文件

将 Traefik 设置为反向代理需要设置以下标签：

```yaml
    labels:
      - traefik.enable=true
      - traefik.http.routers.trilium.entrypoints=https
      - traefik.http.routers.trilium.rule=Host(`trilium.mydomain.tld`)
      - traefik.http.routers.trilium.tls=true
      - traefik.http.routers.trilium.service=trilium
      - traefik.http.services.trilium.loadbalancer.server.port=8080
      # scheme must be HTTP instead of the usual HTTPS because Trilium listens on HTTP internally
      - traefik.http.services.trilium.loadbalancer.server.scheme=http
      - traefik.docker.network=proxy
      # forward HTTP to HTTPS
      - traefik.http.routers.trilium.middlewares=trilium-headers@docker
      - traefik.http.middlewares.trilium-headers.headers.customrequestheaders.X-Forwarded-Proto=https
```

### 设置所需的环境变量

设置反向代理后，请确保配置<a class="reference-link" href="#root/_help_LLzSMXACKhUs">受信任的代理（Trusted proxy）</a>。

### 示例 `docker-compose.yaml`

```yaml
services:
  trilium:
    image: triliumnext/trilium
    container_name: trilium
    networks:
      - traefik-proxy
    environment:
      - TRILIUM_NETWORK_TRUSTEDREVERSEPROXY=my-traefik-host-ip # e.g., 172.18.0.0/16
    volumes:
      - /path/to/data:/home/node/trilium-data
      - /etc/timezone:/etc/timezone:ro
      - /etc/localtime:/etc/localtime:ro
    labels:
      - traefik.enable=true
      - traefik.http.routers.trilium.entrypoints=https
      - traefik.http.routers.trilium.rule=Host(`trilium.mydomain.tld`)
      - traefik.http.routers.trilium.tls=true
      - traefik.http.routers.trilium.service=trilium
      - traefik.http.services.trilium.loadbalancer.server.port=8080
      # scheme must be HTTP instead of the usual HTTPS because of how trilium works
      - traefik.http.services.trilium.loadbalancer.server.scheme=http
      - traefik.docker.network=traefik-proxy
      # Tell Trilium the original request was HTTPS
      - traefik.http.routers.trilium.middlewares=trilium-headers@docker
      - traefik.http.middlewares.trilium-headers.headers.customrequestheaders.X-Forwarded-Proto=https

networks:
  traefik-proxy:
    external: true
```""",

"3jBts3KWOAAP": """配置 TLS 对 Trilium 的[服务器安装](#root/_help_WOcw2SLH6tbX)至关重要。本指南详细介绍了在 Trilium 内部设置 TLS 的步骤。

> [!TIP]
> 虽然 Trilium 本身支持 HTTPS，但通常更好的做法是使用带 TLS 终止的[反向代理](#root/_help_vcjrb3VVYPZI)。你可以参考[这样的指南](https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-20-04)进行此类设置。

## 获取 TLS 证书

获取 TLS 证书有两种选择：

*   **推荐**：获取由根证书颁发机构签名的 TLS 证书。个人使用的话，[Let's Encrypt](https://letsencrypt.org) 是极好的选择。它免费、自动化且简单直接。Certbot 可以方便地实现自动 TLS 设置。
*   生成自签名证书。不推荐此选项，因为需要将证书导入所有连接到服务器的机器，增加了复杂性。

## 修改 `config.ini`

拿到证书后，修改[数据目录](#root/_help_tAassRL4RSQL)中的 `config.ini` 文件，让 Trilium 使用它：

```
[Network]
port=8080
# Set to true for TLS/SSL/HTTPS (secure), false for HTTP (insecure).
https=true
# Path to the certificate (run "bash bin/generate-cert.sh" to generate a self-signed certificate).
# Relevant only if https=true
certPath=/[username]/.acme.sh/[hostname]/fullchain.cer
keyPath=/[username]/.acme.sh/[hostname]/example.com.key
```

你也可以查看[配置](#root/_help_Gzjqa934BdH4)文件，将所有 `config.ini` 值改为环境变量形式。例如，你可以使用环境变量配置 TLS：

```
export TRILIUM_NETWORK_HTTPS=true
export TRILIUM_NETWORK_CERTPATH=/path/to/cert.pem
export TRILIUM_NETWORK_KEYPATH=/path/to/key.pem
```

上面的示例展示了在证书由 Let's Encrypt 的 ACME 工具生成的环境中如何设置。你的路径可能不同。对于 Docker 安装，请确保这些路径位于 Docker 容器可访问的卷或其他目录中，例如 `/home/node/trilium-data/[DIR IN DATA DIRECTORY]`。

配置 `config.ini` 后，重启 Trilium 并使用 "https" 访问主机名。

## 自签名证书

如果你选择为服务器实例使用自签名证书，请注意桌面实例默认不会信任它。

要绕过此问题，请设置以下环境变量来禁用证书验证（适用于 Linux）：

```
export NODE_TLS_REJECT_UNAUTHORIZED=0
trilium
```

Trilium 提供了以这种模式启动的脚本，例如 Windows 上的 `trilium-no-cert-check.bat`。

**警告**：禁用 TLS 证书验证是不安全的。只有在你完全理解其影响时才继续。""",

"PrgLiFypVLYw": """## 禁用身份验证

如果你只在 `localhost` 上运行 Trilium，或者身份验证由其他组件处理，可以通过在 `config.ini` 中添加以下内容来禁用 Trilium 的身份验证：

```
[General]
noAuthentication=true
```

自 v0.94.1 起，禁用身份验证将绕过甚至包括<a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">多因素身份验证（Multi-Factor Authentication）</a>。

## 理解会话的工作方式

登录 Trilium 后，应用会将登录信息存储在浏览器的 cookie 中，同时也在服务器上保存一个会话。

如果勾选了"记住我（Remember me）"，登录将在 21 天后过期。可以通过修改 `config.ini` 中的 `Session.cookieMaxAge` 值来调整该期限。例如，要让会话在一天后过期：

```
[Session]
cookieMaxAge=86400
```

当"记住我"未勾选时，行为不同。在客户端/浏览器层面，身份验证没有过期日期，但会在用户关闭浏览器时立即自动清除。不过，服务器也会在距_与应用的最近一次交互_约 24 小时后使该身份验证失效。

## 查看活动会话

登录会话现在存储在与用户数据相同的<a class="reference-link" href="#root/_help_wX4HbRucYSDD">数据库（Database）</a>中。要查看哪些会话处于活动状态，打开<a class="reference-link" href="#root/_help_YKWqdJhzi2VY">SQL 控制台（SQL Console）</a>并运行以下查询：

```
SELECT * FROM sessions
```

过期的会话由服务器定期清理，通常每小时一次。

## 另请参阅

*   <a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">多因素身份验证（Multi-Factor Authentication）</a>""",

"eaZEEY2i4YT2": """> [!IMPORTANT]
> 如果你忘记了密码：
> 
> *   受保护笔记在没有密码的情况下无法找回。
> *   未受保护的笔记可以恢复。

有两个选项，都需要访问运行你服务器的设备。

### 通过禁用身份验证

1.  参见<a class="reference-link" href="#root/_help_0hzsNCP31IAB">身份验证（Authentication）</a>了解如何禁用身份验证。
2.  重启服务器。
3.  进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → 密码 → 重置密码。
4.  按照步骤 (1) 的反向操作重新启用身份验证。
5.  重启服务器。
6.  使用新密码登录。

### 通过修改数据库

1.  访问[数据目录](#root/_help_tAassRL4RSQL)中的[数据库](#root/_help_wX4HbRucYSDD)文件。使用 SQLite 客户端（例如 [DB Browser](https://sqlitebrowser.org/)）打开 `document.db` 文件。
2.  执行以下查询：

    ```
    UPDATE options SET value = '77/twC5O00cuQgNC63VK32qOKKYwj21ev3jZDXoytVU=' WHERE name = 'passwordVerificationSalt';
    UPDATE options SET value = '710BMasZCAgibzIc07X4P9Q4TeBd4ONnqJOho+pWcBM=' WHERE name = 'passwordDerivedKeySalt';
    UPDATE options SET value = 'Eb8af1/T57b89lCRuS97tPEl4CwxsAWAU7YNJ77oY+s=' WHERE name = 'passwordVerificationHash';
    UPDATE options SET value = 'QpC8XoiYYeqHPtHKRtbNxfTHsk+pEBqVBODYp0FkPBa22tlBBKBMigdLu5GNX8Uu' WHERE name = 'encryptedDataKey';
    ```
3.  执行更改后，提交/写入更改。**这会将密码设置为** `**password**`**，让你可以重新登录。**
4.  进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码（Password）_ → _更改密码（Change password）_，替换掉不安全的密码。

## 处理受保护笔记

密码重置后，受保护笔记将永久丢失，因为它们是用你的密码加密的，Trilium 无法恢复它们。

对于已有的受保护笔记（现已无法恢复），请考虑删除它们或导出未受保护的笔记。然后，删除 `document.db` 并重新开始。""",

"NwgMJByVeFyC": """多因素身份验证（MFA）是一种安全流程，要求用户提供两个或更多验证因素才能访问系统、应用或账户。这比仅使用密码多了一层保护。

通过要求多种验证方法，MFA 有助于降低未经授权访问的风险，即使有人获得了你的密码。强烈建议用它来保护笔记中存储的敏感信息。

## 与 OpenID Connect（OIDC）的关系

多因素身份验证通过要求除密码之外的额外身份证明，让你的 Trilium 登录更安全。<a class="reference-link" href="#root/_help_x1lQNKQBqLcA">使用 OpenID Connect 登录（Signing in with OpenID Connect）</a>是另一个概念，它将你的身份验证委托给外部提供商（例如你的 Google 提供商，或自托管的 Authelia 等）。这些身份提供商可能提供自己的安全功能，如 TOTP 或 passkey。

## 基于时间的一次性密码（TOTP）

TOTP（Time-Based One-Time Password，基于时间的一次性密码）是一种安全功能，会在你的设备（如智能手机）上生成唯一的临时验证码，每 30 秒变化一次。你使用这个验证码连同密码登录账户，让其他人更难访问它们。

1.  进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_。
2.  确保_使用以下方式登录（Sign-in with）_选择了_本地密码（Local password）_。
3.  在_双因素身份验证（Two-factor authentication）_部分，按_设置（Set up）_。
4.  扫描二维码或在你的身份验证器应用中手动输入代码。
5.  输入身份验证器应用中的验证码以确认设置成功，然后按_验证并继续（Verify & continue）_。
6.  保存恢复代码，勾选确认框，然后按_完成设置（Finish setup）_。
7.  TOTP 设置完成后需要重新登录（刷新页面后）。

## 恢复代码

如果你无法访问身份验证器，可以使用恢复代码代替 TOTP。Trilium 提供 8 个不同的恢复代码，每个恢复代码只能使用一次。

要使用恢复代码，只需用密码登录，并将恢复代码作为安全令牌使用。

初始的恢复代码集在首次设置 TOTP 时生成。你可以随时进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_并按_重新生成恢复代码（Regenerate recovery codes）_按钮重新生成。这会生成一组新的恢复代码，同时禁用之前的那组。""",

"Ht8ZUwhpA1DH": """自 TriliumNext 0.94.1 起，可以使用 [Nix](https://nixos.org/) 构建桌面和服务器应用。

## 系统要求

在 Mac 或 Linux 上安装 Nix（[下载页](https://nixos.org/download/)）。大约需要 3-4 GB 的额外存储空间，用于构建产物。

## 直接运行

使用 [nix run](https://nix.dev/manual/nix/stable/command-ref/new-cli/nix3-run.html)，桌面应用可以这样启动：`nix run github:TriliumNext/Trilium/v0.95.0`

运行服务器需要明确指定所需的包：`nix run github:TriliumNext/Trilium/v0.95.0#server`

除了版本（如上面的 `v0.95.0`），你也可以指定提交哈希（或分支名）。这样便于测试开发构建。

## 在 NixOS 上安装

添加到你的 `flake.nix`：

```
{
  inputs = {
    nixpkgs.url = # ...;
    trilium-notes = {
      url = "github:TriliumNext/Trilium/v0.95.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      # ...
      trilium-notes,
      ...
    }:
    {
      nixosConfigurations = {
        "nixos" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./configuration.nix
          ];
          specialArgs = {
            inherit
              trilium-notes
              ;
          };
        };
      };
    };
}

```

添加到你的 `configuration.nix`：

```
{
  # ...
  trilium-notes,
  ...
}:

{
  # ...

  services.trilium-server.package = trilium-notes.packages.x86_64-linux.server;

  environment.systemPackages = [
    trilium-notes.packages.x86_64-linux.desktop
  ];
}
```

该 flake 旨在兼容最新的 NixOS 稳定版和不稳定版。""",
}
