# -*- coding: utf-8 -*-
TRANS = {
"zimAIUMnhVS8": """Trilium 支持通过名为 `config.ini` 的文件和环境变量进行配置。本文档提供所有配置选项的全面参考。

## 配置文件的位置

配置文件与应用不在同一目录。相反，`config.ini` 位于<a class="reference-link" href="#root/_help_tAassRL4RSQL">数据目录（Data directory）</a>中。因此，配置文件只有在启动应用并创建数据库后才可用。

## 配置优先级

配置值按以下优先级顺序加载（从高到低）：

1.  **环境变量**（首先检查）
2.  **config.ini 文件值**
3.  **默认值**

## 环境变量模式

Trilium 支持多种环境变量模式以提高灵活性。主要模式是：`TRILIUM_[SECTION]_[KEY]`

其中：

*   `SECTION` 是 INI 节名称的大写形式
*   `KEY` 是 camelCase 配置键转换为大写（例如 `instanceName` → `INSTANCENAME`）

此外，常见配置还有更短的别名（请参阅下面的替代变量章节）。

## 环境变量参考

### 常规（General）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_GENERAL_INSTANCENAME` | string | "" | 用于 API 标识的实例名称 |
| `TRILIUM_GENERAL_NOAUTHENTICATION` | boolean | false | 禁用身份验证（仅服务器） |
| `TRILIUM_GENERAL_NOBACKUP` | boolean | false | 禁用自动备份 |
| `TRILIUM_GENERAL_NODESKTOPICON` | boolean | false | 禁用桌面图标创建 |
| `TRILIUM_GENERAL_READONLY` | boolean | false | 启用只读模式 |

### 网络（Network）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_NETWORK_HOST` | string | "0.0.0.0" | 服务器主机绑定 |
| `TRILIUM_NETWORK_PORT` | string | "8080" | 服务器端口 |
| `TRILIUM_NETWORK_HTTPS` | boolean | false | 启用 HTTPS |
| `TRILIUM_NETWORK_CERTPATH` | string | "" | SSL 证书路径 |
| `TRILIUM_NETWORK_KEYPATH` | string | "" | SSL 密钥路径 |
| `TRILIUM_NETWORK_TRUSTEDREVERSEPROXY` | boolean/string | false | 反向代理信任设置 |
| `TRILIUM_NETWORK_CORSALLOWORIGIN` | string | "" | CORS 允许的来源 |
| `TRILIUM_NETWORK_CORSALLOWMETHODS` | string | "" | CORS 允许的方法 |
| `TRILIUM_NETWORK_CORSALLOWHEADERS` | string | "" | CORS 允许的头 |
| `TRILIUM_NETWORK_CORSRESOURCEPOLICY` | string | same-origin | CORS 资源策略，允许 same-origin/same-site/cross-origin 值，否则会报错 |

### 会话（Session）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_SESSION_COOKIEMAXAGE` | integer | 1814400 | 会话 cookie 最大时长（秒）（21 天） |

### 同步（Sync）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_SYNC_SYNCSERVERHOST` | string | "" | 同步服务器主机 URL |
| `TRILIUM_SYNC_SYNCSERVERTIMEOUT` | string | "120000" | 同步服务器超时（毫秒） |
| `TRILIUM_SYNC_SYNCPROXY` | string | "" | 同步代理 URL |

### 多因素认证（MultiFactorAuthentication）节

请参阅<a class="reference-link" href="#root/_help_x1lQNKQBqLcA">使用 OpenID Connect 登录（Signing in with OpenID Connect）</a>。

### 日志（Logging）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_LOGGING_RETENTIONDAYS` | integer | 90 | 保留日志文件的天数 |

### 开发（Development）节

| 环境变量 | 类型 | 默认值 | 描述 |
| --- | --- | --- | --- |
| `TRILIUM_MANUAL_DB_MIGRATION` | boolean | false | 如果为 `true`，当有待处理的自动数据库迁移且会修改数据库模式时，应用将不会启动。 |

## 替代环境变量

以下替代环境变量名称也受支持，其作用与对应的长名称完全相同：

### 网络 CORS 变量

*   `TRILIUM_NETWORK_CORS_ALLOW_ORIGIN`（替代 `TRILIUM_NETWORK_CORSALLOWORIGIN`）
*   `TRILIUM_NETWORK_CORS_ALLOW_METHODS`（替代 `TRILIUM_NETWORK_CORSALLOWMETHODS`）
*   `TRILIUM_NETWORK_CORS_ALLOW_HEADERS`（替代 `TRILIUM_NETWORK_CORSALLOWHEADERS`）
*   `TRILIUM_NETWORK_CORS_RESOURCE_POLICY`（替代 `TRILIUM_NETWORK_CORSRESOURCEPOLICY`）

### 同步变量

*   `TRILIUM_SYNC_SERVER_HOST`（替代 `TRILIUM_SYNC_SYNCSERVERHOST`）
*   `TRILIUM_SYNC_SERVER_TIMEOUT`（替代 `TRILIUM_SYNC_SYNCSERVERTIMEOUT`）
*   `TRILIUM_SYNC_SERVER_PROXY`（替代 `TRILIUM_SYNC_SYNCPROXY`）

### OAuth/MFA 变量

*   `TRILIUM_OAUTH_BASE_URL`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHBASEURL`）
*   `TRILIUM_OAUTH_CLIENT_ID`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTID`）
*   `TRILIUM_OAUTH_CLIENT_SECRET`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTSECRET`）
*   `TRILIUM_OAUTH_ISSUER_BASE_URL`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERBASEURL`）
*   `TRILIUM_OAUTH_ISSUER_NAME`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERNAME`）
*   `TRILIUM_OAUTH_ISSUER_ICON`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERICON`）
*   `TRILIUM_OAUTH_HTTP_TIMEOUT`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHHTTPTIMEOUT`）
*   `TRILIUM_OAUTH_SCOPE`（替代 `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHSCOPE`）

### 日志变量

*   `TRILIUM_LOGGING_RETENTION_DAYS`（替代 `TRILIUM_LOGGING_RETENTIONDAYS`）

## 布尔值

布尔环境变量接受以下值：

*   **True（真）**：`"true"`、`"1"`、`1`
*   **False（假）**：`"false"`、`"0"`、`0`
*   任何其他值默认为 `false`

## 使用环境变量

两种命名模式都完全受支持，可以互换使用：

*   较长格式遵循节/键模式，与 INI 文件结构保持一致
*   较短替代格式为常见配置提供便利
*   你可以使用任何一种你喜欢的格式——两者同样有效

## 示例

### Docker Compose 示例

```yaml
services:
  trilium:
    image: triliumnext/trilium
    environment:
      # 使用完整格式
      TRILIUM_GENERAL_INSTANCENAME: "My Trilium Instance"
      TRILIUM_NETWORK_PORT: "8080"
      TRILIUM_NETWORK_CORSALLOWORIGIN: "https://myapp.com"
      TRILIUM_SYNC_SYNCSERVERHOST: "https://sync.example.com"
      TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHBASEURL: "https://auth.example.com"
      
      # 或者使用较短的替代格式（同样有效）
      # TRILIUM_NETWORK_CORS_ALLOW_ORIGIN: "https://myapp.com"
      # TRILIUM_SYNC_SERVER_HOST: "https://sync.example.com"
      # TRILIUM_OAUTH_BASE_URL: "https://auth.example.com"
```

### Shell 导出示例

```
# 使用任一格式
export TRILIUM_GENERAL_NOAUTHENTICATION=false
export TRILIUM_NETWORK_HTTPS=true
export TRILIUM_NETWORK_CERTPATH=/path/to/cert.pem
export TRILIUM_NETWORK_KEYPATH=/path/to/key.pem
export TRILIUM_LOGGING_RETENTIONDAYS=30

# 启动 Trilium
npm start
```

## config.ini 参考

关于所有配置选项及其 INI 文件格式的完整列表，请查看 Trilium 仓库中的 [config-sample.ini](https://github.com/TriliumNext/Trilium/blob/main/apps/server/src/assets/config-sample.ini) 文件""",

"g0OYZmaGK1v8": """OpenID 是一种标准化方式，允许你使用另一个服务（如 Google 或 Authelia）的账户登录网站以验证你的身份。

启用 OpenID 后，Trilium 中基于密码的身份验证会被一个使用你的提供商连接的按钮所取代。这意味着<a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">使用 TOTP 的多因素认证（Multi-factor authentication with TOTP）</a>的配置不再生效，因为你的提供商必须处理任何多因素认证。

## 设置

使用 OpenID Connect 设置身份验证是一个两步过程：

1.  首先，必须为 Trilium 服务器配置你的身份验证提供商信息，如 URL、客户端 ID 和密钥。
2.  其次，用户必须从选项中连接，以在提供商的账户和 Trilium 的账户之间创建链接。

### 配置身份验证提供商

1.  首先，确保身份验证提供商（例如 Google、Authelia）配置正确。具体示例请参阅<a class="reference-link" href="#root/_help_IgkTHSaFfq6A">使用各种提供商进行设置（Setting up with various providers）</a>。
    
    1.  Trilium 的重定向 URL 是 `https://<your-trilium-domain>/callback`。
    2.  你应该获得基础 URL、客户端 ID 和客户端密钥。
2.  使用<a class="reference-link" href="#root/_help_Gzjqa934BdH4">配置（Configuration (config.ini or environment variables)）</a>设置以下信息：
    
    | 配置 | `config.ini` 中的 `[MultiFactorAuthentication]` 节 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | 基础 URL\* | `oauthBaseUrl` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHBASEURL` | 你的 Trilium 实例的 URL（例如 `https://example.com`）。 |
    | 客户端 ID\* | `oauthClientId` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTID` | 来自你的提供商配置的客户端 ID。 |
    | 客户端密钥\* | `oauthClientSecret` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTSECRET` | 来自你的提供商配置的客户端密钥。 |
    | 客户端认证方法 | `oauthClientAuthMethod` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTAUTHMETHOD` | 令牌端点认证方法：`client_secret_basic` 或 `client_secret_post`。留空自动检测。仅在登录失败并出现 `WWW-Authenticate` 或 `invalid_client` 错误时需要。 |
    | ID 令牌算法 | `oauthIdTokenSigningAlg` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHIDTOKENSIGNINGALG` | 你的提供商对 ID 令牌签名的算法，例如 `RS256`、`EdDSA`、`ES256`。留空从提供商自动检测。仅在登录失败并出现 `unexpected JWT alg` 错误时需要。 |
    
    星号（\*）标记必填字段
3.  默认的 OAuth 颁发者是 Google。要使用 Authentik 或 Auth0 等其他服务，你可以通过 `config.ini` 文件中的 `oauthIssuerBaseUrl`、`oauthIssuerName` 和 `oauthIssuerIcon` 配置设置。或者，这些值可以使用环境变量设置：
    
    | 配置 | `config.ini` 中的 `[MultiFactorAuthentication]` 节 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | 颁发者基础 URL | `oauthIssuerBaseUrl` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERBASEURL` | 可以是颁发者本身（`https://auth.example.com`）或完整的发现 URL（`https://auth.example.com/.well-known/openid-configuration`）。  <br>  <br>包含 `/.well-known/` 的 URL 将按原样使用，当你的提供商给你一个发现 URL 复制时很方便。尾部斜杠可以包含或省略；Trilium 会匹配提供商公布的任何拼写。  <br>  <br>当你的提供商公布的颁发者与提供其发现文档的路径不同时（例如 Authentik 的"全局"颁发者模式），请使用完整形式。 |
    | 颁发者名称 | `oauthIssuerName` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERNAME` | 你的身份验证提供商的名称，用于登录屏幕和设置中的参考。默认是"Google"。 |
    | 颁发者图标 | `oauthIssuerIcon` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERICON` | 可选，提供商标志的 URL。默认情况下它会尝试从网站获取 favicon，所以是可选的。 |
    
    这里的所有字段都是可选的，因为默认的 OAuth 颁发者是 Google。
4.  可选地，调整以下高级设置。所有都是可选的，并有合理的默认值：
    
    | 配置 | `config.ini` 中的 `[MultiFactorAuthentication]` 节 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | HTTP 超时 | `oauthHttpTimeout` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHHTTPTIMEOUT` | 提供商 HTTP 请求（发现、令牌交换、用户信息）的超时（毫秒）。默认为 `30000`（30 秒）。如果你的提供商冷启动缓慢且偶尔被要求登录两次，请提高此值。最小可接受值为 `500`。 |
    | 作用域 | `oauthScope` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHSCOPE` | 登录时请求的 OIDC 作用域空格分隔列表。默认为 `openid profile email`。`openid` 是必需的，如果你省略它会自动前置。 |
    
    OIDC 会话生命周期会自动绑定到 Trilium 自身的会话 cookie 生命周期（`[Session]` 节中的 `cookieMaxAge` 设置，默认 21 天），因此单次登录的有效期与你的 Trilium 会话一样长，而不会在 OIDC 库较短的内部默认值上提前过期。
5.  重启服务器以应用更改。

> [!NOTE]
> 也支持旧版环境变量：`TRILIUM_OAUTH_BASE_URL`、`TRILIUM_OAUTH_CLIENT_ID`、`TRILIUM_OAUTH_CLIENT_SECRET`，以及用于自定义提供商的：`TRILIUM_OAUTH_ISSUER_BASE_URL`、`TRILIUM_OAUTH_ISSUER_NAME`、`TRILIUM_OAUTH_ISSUER_ICON`、`TRILIUM_OAUTH_HTTP_TIMEOUT`、`TRILIUM_OAUTH_SCOPE`、`TRILIUM_OAUTH_CLIENT_AUTH_METHOD`、`TRILIUM_OAUTH_ID_TOKEN_SIGNING_ALG`。

## 连接到身份验证提供商

服务器在上一步配置完成后，下一步是在你的身份验证提供商账户和 Trilium 实例之间创建链接。这确保只有你能访问 Trilium 实例，而不是任何其他有效账户。

为此：

1.  转到<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_。
2.  在_登录方式（Sign-in with）_字段中，选择_OpenID Connect 提供商（OpenID Connect provider）_。
3.  在_OpenID Connect_部分，查找_连接账户（Connect account）_按钮。
4.  这会重定向到你的身份验证提供商，你可以在那里登录或根据需要确认操作。
5.  认证完成后，你会被重定向回 Trilium 应用程序。

## 登出

登出 Trilium 时，会向身份验证提供商发出请求，也从那里登出。此功能取决于身份验证提供商，因此可能不被遵守（已知 Google 和 Authelia 不尊重登出功能）。

## 切换提供商

切换提供商时（例如从 Google 切换到 Authelia），务必执行以下步骤：

1.  转到<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_。
2.  在_OpenID Connect_部分，按_断开连接（Disconnect）_按钮。
3.  等待该部分指示你已断开连接。
4.  更改指向新提供商的配置。
5.  重启你的服务器。
6.  重复正常的连接到身份验证提供商的步骤。

切换提供商前未断开连接可能会暂时将你锁定，因为你将无法登录（凭据不匹配）。如果发生这种情况：

1.  再次将服务器配置修改为你的旧提供商。
2.  重启服务器并按照上面的断开连接说明操作。
3.  再次将服务器配置修改为你的新提供商。
4.  再次重启服务器。

## 暂时停用 OpenID Connect

要禁用 OpenID Connect 身份验证并暂时依赖本地密码，你必须：

1.  修改 `config.ini` 或环境变量（取决于你如何设置提供商信息），并通过将 `[MultiFactorAuthentication]` 重命名为其他名称（例如 `[MultiFactorAuthentication.bak]`）来暂时停用多因素认证节。
2.  重启服务器以使更改生效。

## 故障排除

### 设置失败并出现 `WWW-Authenticate` 或 `invalid_client` 错误

你的提供商与 Trilium 在客户端凭据应如何发送到令牌端点的问题上存在分歧。将 `oauthClientAuthMethod` 设置为 `client_secret_post`（如果已设置为 post，则设置为 `client_secret_basic`）并重启。提供商各不相同：有些直接拒绝 `client_secret_post`，因为该方法在客户端注册时已固定（例如 Authelia），所以如果一个值不起作用，请尝试另一个。

### 设置失败并出现 `invalid user`

如果你在[反向代理](#root/_help_vcjrb3VVYPZI)后面运行，缓冲区溢出也可能导致此问题。这是<a class="reference-link" href="#root/_help_ud6MShXL4WpO">Nginx</a>的示例修复：

```
proxy_buffer_size 128k;
proxy_buffers 4 256k;
proxy_busy_buffers_size 256k;
```

### 设置失败并出现 `unexpected JWT alg` 错误

根据你的版本，消息会显示 `unexpected JWT "alg" header parameter` 或 `unexpected JWT alg received, expected RS256, got: EdDSA`。两者含义相同：你的提供商使用 RS256 以外的算法对 ID 令牌签名。Pocket ID (Ed25519) 和 Kanidm (ES256) 就是这样。

从 v0.104.2 起，Trilium 会自动从你的提供商检测算法，日志会记录结果（`OAuth: the issuer does not sign ID tokens with RS256; expecting ES256`）。如果检测无法到达提供商，它会回退到 RS256，所以先检查颁发者 URL 是否正确且可达。如果仍然失败，请将 `oauthIdTokenSigningAlg` 显式设置为你提供商使用的算法。

### 设置失败并出现 `OAUTH_RESPONSE_IS_NOT_CONFORM`

确保基础 URL 正确，并且身份提供商确实支持 OpenID Connect。某些提供商（如 GitHub）只提供 OAuth 2.0。""",
}
