# -*- coding: utf-8 -*-
# 安装章节叶子双语→纯中文 批次3（服务器安装6篇）
TRANS = {

"g0OYZmaGK1v8": """OpenID 是一种标准化方式，让你可以使用另一个服务（如 Google 或 Authelia）的账户登录网站以验证你的身份。

当 OpenID 被激活时，Trilium 中基于密码的身份验证会被替换为一个使用你的提供商连接的按钮。这意味着<a class="reference-link" href="#root/_help_7DAiwaf8Z7Rz">使用 TOTP 的多因素身份验证（Multi-factor authentication with TOTP）</a>的配置不再生效，因为多因素身份验证必须由你的提供商处理。

## 设置

使用 OpenID Connect 设置身份验证是一个两步过程：

1.  首先，必须用你的身份提供商的信息（如 URL、客户端 ID 和密钥）配置 Trilium 服务器。
2.  其次，用户必须从选项中进行连接，以在提供商的账户和 Trilium 的账户之间建立关联。

### 配置身份提供商

1.  首先，确保身份提供商（如 Google、Authelia）配置正确。具体示例参见<a class="reference-link" href="#root/_help_IgkTHSaFfq6A">使用各种提供商进行设置（Setting up with various providers）</a>。
    
    1.  Trilium 的重定向 URL 是 `https://<your-trilium-domain>/callback`。
    2.  你应该获取基础 URL、客户端 ID 和客户端密钥。
2.  使用<a class="reference-link" href="#root/_help_Gzjqa934BdH4">配置（config.ini 或环境变量）</a>设置以下信息：
    
    | 配置 | `config.ini` 中 `[MultiFactorAuthentication]` 部分 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | 基础 URL\* | `oauthBaseUrl` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHBASEURL` | 你的 Trilium 实例的 URL（例如 `https://example.com`）。 |
    | 客户端 ID\* | `oauthClientId` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTID` | 提供商配置中的客户端 ID。 |
    | 客户端密钥\* | `oauthClientSecret` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTSECRET` | 提供商配置中的客户端密钥。 |
    | 客户端认证方法 | `oauthClientAuthMethod` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHCLIENTAUTHMETHOD` | 令牌端点认证方法：`client_secret_basic` 或 `client_secret_post`。留空自动检测。仅当登录失败并出现 `WWW-Authenticate` 或 `invalid_client` 错误时才需要。 |
    | ID 令牌算法 | `oauthIdTokenSigningAlg` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHIDTOKENSIGNINGALG` | 你的提供商用于签署 ID 令牌的算法，例如 `RS256`、`EdDSA`、`ES256`。留空则从提供商自动检测。仅当登录失败并出现 `unexpected JWT alg` 错误时才需要。 |
    
    星号（\*）标记必填字段
3.  默认的 OAuth 签发者是 Google。要使用 Authentik 或 Auth0 等其他服务，你可以通过 `config.ini` 文件中的 `oauthIssuerBaseUrl`、`oauthIssuerName` 和 `oauthIssuerIcon` 配置设置。或者，这些值也可以使用环境变量设置：
    
    | 配置 | `config.ini` 中 `[MultiFactorAuthentication]` 部分 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | 签发者基础 URL | `oauthIssuerBaseUrl` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERBASEURL` | 要么是签发者本身（`https://auth.example.com`），要么是完整的发现 URL（`https://auth.example.com/.well-known/openid-configuration`）。<br><br>包含 `/.well-known/` 的 URL 会按原样使用，当你的提供商给你一个发现 URL 直接复制时很方便。末尾斜杠可有可无；Trilium 会匹配提供商公布的形式。<br><br>当提供商公布的签发者与提供其发现文档的路径不同时（如 Authentik 的 "global" 签发者模式），请使用完整形式。 |
    | 签发者名称 | `oauthIssuerName` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERNAME` | 你的身份提供商的名称，用于登录屏幕和设置中的引用。默认是"Google"。 |
    | 签发者图标 | `oauthIssuerIcon` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHISSUERICON` | 可选，提供商标识的 URL。默认会尝试从网站获取 favicon，因此是可选的。 |
    
    这里的所有字段都是可选的，因为默认的 OAuth 签发者是 Google。
4.  可选地，调整以下高级设置。所有都是可选的，并有合理的默认值：
    
    | 配置 | `config.ini` 中 `[MultiFactorAuthentication]` 部分 | 环境变量 | 描述 |
    | --- | --- | --- | --- |
    | HTTP 超时 | `oauthHttpTimeout` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHHTTPTIMEOUT` | 提供商 HTTP 请求（发现、令牌交换、userinfo）的超时时间（毫秒）。默认 `30000`（30 秒）。如果你的提供商冷启动较慢且偶尔要求你登录两次，请调高此值。最小可接受值为 `500`。 |
    | 作用域 | `oauthScope` | `TRILIUM_MULTIFACTORAUTHENTICATION_OAUTHSCOPE` | 登录时请求的以空格分隔的 OIDC 作用域列表。默认为 `openid profile email`。`openid` 是必需的，如果你省略它会自动添加。 |
    
    OIDC 会话生命周期会自动绑定到 Trilium 自己的会话 cookie 生命周期（`[Session]` 部分中的 `cookieMaxAge` 设置，默认 21 天），因此一次登录的有效期与你的 Trilium 会话一样长，而不会在 OIDC 库较短的内部默认值下提前过期。
5.  重启服务器以使更改生效。

> [!NOTE]
> 也支持旧版环境变量：`TRILIUM_OAUTH_BASE_URL`、`TRILIUM_OAUTH_CLIENT_ID`、`TRILIUM_OAUTH_CLIENT_SECRET`，以及用于自定义提供商：`TRILIUM_OAUTH_ISSUER_BASE_URL`、`TRILIUM_OAUTH_ISSUER_NAME`、`TRILIUM_OAUTH_ISSUER_ICON`、`TRILIUM_OAUTH_HTTP_TIMEOUT`、`TRILIUM_OAUTH_SCOPE`、`TRILIUM_OAUTH_CLIENT_AUTH_METHOD`、`TRILIUM_OAUTH_ID_TOKEN_SIGNING_ALG`。

## 连接到身份提供商

服务器在上一步配置完成后，下一步是在身份提供商的账户和 Trilium 实例之间建立关联。这可以确保只有你能访问 Trilium 实例，而不是任何其他有效账户。

具体操作：

1.  进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_。
2.  在_使用以下方式登录（Sign-in with）_字段中，选择 _OpenID Connect 提供商（OpenID Connect provider）_。
3.  在 _OpenID Connect_ 部分，找到_连接账户（Connect account）_按钮。
4.  这会将你重定向到身份提供商，你可以在那里登录或在需要时确认操作。
5.  认证完成后，你会被重定向回 Trilium 应用。

## 退出登录

退出 Trilium 时，会向身份提供商发出请求，要求其也退出登录。此功能取决于身份提供商，因此可能不被执行（已知 Google 和 Authelia 不遵守退出登录功能）。

## 切换提供商

切换提供商时（例如从 Google 切换到 Authelia），务必执行以下步骤：

1.  进入<a class="reference-link" href="#root/_help_4TIF1oA4VQRO">选项（Options）</a> → _密码与认证（Password & Auth）_。
2.  在 _OpenID Connect_ 部分，按_断开连接（Disconnect）_按钮。
3.  等待该部分显示你已断开连接。
4.  更改指向新提供商的配置。
5.  重启你的服务器。
6.  重复正常步骤连接到身份提供商。

如果切换提供商前没有断开连接，可能会暂时被锁住，因为无法登录（凭据不匹配）。如果发生这种情况：

1.  再次将服务器配置修改回旧提供商。
2.  重启服务器并按照上面的断开连接说明操作。
3.  再次将服务器配置修改为新提供商。
4.  再次重启。

## 暂时停用 OpenID Connect

要暂时禁用 OpenID Connect 身份验证并改用本地密码，你必须：

1.  修改 `config.ini` 或环境变量（取决于你如何设置提供商信息），通过将 `[MultiFactorAuthentication]` 重命名为其他名称（例如 `[MultiFactorAuthentication.bak]`）来暂时停用多因素身份验证部分。
2.  重启服务器使更改生效。

## 故障排除

### 设置失败并出现 `WWW-Authenticate` 或 `invalid_client` 错误

你的提供商与 Trilium 在客户端凭据应如何发送到令牌端点的问题上意见不一致。将 `oauthClientAuthMethod` 设置为 `client_secret_post`（如果已设置为 post，则改为 `client_secret_basic`）并重启。提供商各不相同：有些提供商直接拒绝 `client_secret_post`，因为该方法在客户端注册时已固定（例如 Authelia），所以如果一个值不起作用，请尝试另一个。

### 设置失败并出现 `invalid user`

如果你在[反向代理](#root/_help_vcjrb3VVYPZI)后面运行，缓冲区溢出也可能导致此问题。以下是 <a class="reference-link" href="#root/_help_ud6MShXL4WpO">Nginx</a> 的示例修复：

```
proxy_buffer_size 128k;
proxy_buffers 4 256k;
proxy_busy_buffers_size 256k;
```

### 设置失败并出现 `unexpected JWT alg` 错误

根据你的版本，消息显示为 `unexpected JWT "alg" header parameter` 或 `unexpected JWT alg received, expected RS256, got: EdDSA`。两者意思相同：你的提供商用 RS256 以外的算法签署 ID 令牌。Pocket ID (Ed25519) 和 Kanidm (ES256) 就是这种情况。

从 v0.104.2 起，Trilium 会自动从提供商检测算法，日志会记录结果（`OAuth: the issuer does not sign ID tokens with RS256; expecting ES256`）。如果检测无法到达提供商，它会回退到 RS256，因此请先检查签发者 URL 是否正确且可达。如果仍然失败，请显式将 `oauthIdTokenSigningAlg` 设置为你提供商使用的算法。

### 设置失败并出现 `OAUTH_RESPONSE_IS_NOT_CONFORM`

确保基础 URL 正确，并且身份提供商确实支持 OpenID Connect。某些提供商（如 GitHub）只提供 OAuth 2.0。""",

"v14qii09xWVc": """> [!NOTE]
> 本页包含如何将<a class="reference-link" href="#root/_help_x1lQNKQBqLcA">使用 OpenID Connect 登录（Signing in with OpenID Connect）</a>与各种提供商（如 Authelia、GitLab、Google）配合设置的说明。请注意，虽然配置参考是正确且最新的，但创建 OAuth 应用的步骤可能因提供商更改其 UI 而略有不同。

## Authelia

1.  生成客户端密钥：

    ```
    authelia crypto hash generate pbkdf2 --variant sha512 --random --random.length 72 --random.charset rfc3986
    
    ```
    
    `Random Password` 填入 Trilium，`Digest`（`$pbkdf2-sha512$…`字符串）填入 Authelia。
2.  在 Authelia 的 `configuration.yml` 的 `identity_providers.oidc.clients` 下添加一个客户端，将 `<server>` 替换为你的 Trilium 实例的 URL：

    ```yaml
    identity_providers:
      oidc:
        clients:
          - client_id: 'trilium'
            client_name: 'Trilium'
            client_secret: '<Digest>'
            public: false
            authorization_policy: 'two_factor'
            redirect_uris:
              - 'https://<server>/callback'
            scopes:
              - 'openid'
              - 'profile'
              - 'email'
    ```
    
    与 GitLab 不同，`client_id` 是你自己选择的名字，而不是提供商生成的。
3.  重启 Authelia。

调整 `config.ini`，使用步骤 1 中的 `Random Password`：

```
[MultiFactorAuthentication]
oauthBaseUrl=https://<server>
oauthClientId=<ClientId>
oauthClientSecret=<RandomPassword>
oauthIssuerBaseUrl=https://<server>
oauthIssuerName=Authelia
```

> [!IMPORTANT]
> 不要为 Authelia 设置 `oauthClientAuthMethod`。它默认对机密客户端使用 `client_secret_basic`，并会以 `invalid_client` 拒绝任何其他方法，这也是 Trilium 已经使用的默认值。这与下面的 GitLab 情况相反。

## GitLab（自托管或云）

1.  进入 [gitlab.com](https://gitlab.com/-/user_settings/applications) 或你自己自托管实例上的用户设置。
2.  按_添加新应用（Add new application）_。
3.  给它一个名字（例如 Trilium）。
4.  将_重定向 URI（Redirect URI）_设置为 `https://<server>/callback`
5.  确保勾选_机密（Confidential）_并取消勾选_设备授权授权（Device authorization grant）_。
6.  在作用域下，勾选 _openid_、_profile_ 和 _email_（它们应该在接近末尾的位置）。
7.  保存应用并复制_应用程序 ID（Application ID）_和_密钥（Secret）_。

按如下方式调整 `config.ini`，将 `<ApplicationId>` 和 `<Secret>` 替换为上一步的值，并将 `<server>` 替换为你的 Trilium 实例的 URL。

```
[MultiFactorAuthentication]
oauthBaseUrl=https://<server>
oauthClientId=<ApplicationId>
oauthClientSecret=<Secret>
oauthIssuerBaseUrl=https://gitlab.com
oauthIssuerName=GitLab
oauthClientAuthMethod=client_secret_post

```

> [!IMPORTANT]
> 如果你使用自托管的 GitLab 实例，请确保同时更新 `oauthIssuerBaseUrl`，并保留上面的 `oauthClientAuthMethod` 行。
> 
> GitLab 的令牌端点不会解码通过 HTTP Basic 发送的凭据，因此如果没有 `oauthClientAuthMethod`，登录会失败并出现 `server responded with a challenge in the WWW-Authenticate HTTP Header` 错误。Trilium 会自动为 `gitlab.com` 应用此设置，但无法检测自托管的签发者 URL。

## Kanidm

1.  创建客户端、添加重定向 URL 并授予作用域，将 `<server>` 替换为你的 Trilium 实例的 URL，将 `<group>` 替换为成员可以登录的现有 Kanidm 组：

    ```
    kanidm system oauth2 create trilium "Trilium" "https://<server>"
    kanidm system oauth2 add-redirect-url trilium "https://<server>/callback"
    kanidm system oauth2 update-scope-map trilium <group> openid profile email
    ```
    
    `openid` 作用域是必需的；没有它，Kanidm 会将该客户端视为普通 OAuth 2.0。
2.  读取客户端密钥：

    ```
    kanidm system oauth2 show-basic-secret trilium
    ```

按如下方式调整 `config.ini`，将 `<idm>` 替换为你的 Kanidm 实例的 URL，将 `<BasicSecret>` 替换为步骤 2 中的值：

```
[MultiFactorAuthentication]
oauthBaseUrl=https://<server>
oauthClientId=trilium
oauthClientSecret=<BasicSecret>
oauthIssuerBaseUrl=https://<idm>/oauth2/openid/trilium
oauthIssuerName=Kanidm
```

你在步骤 1 中选择的客户端名称既是客户端 ID，也是签发者 URL 的最后一段路径，因此两者必须匹配。

> [!IMPORTANT]
> Kanidm 使用 ES256 而非 RS256 签署 ID 令牌。Trilium 在 v0.105.0 及以后版本会从提供商检测到这一点，因此无需额外配置。不要运行 `kanidm system oauth2 warning-enable-legacy-crypto`，它会将客户端降级为 RS256 并完全禁用 ES256。在较旧的 Trilium 版本上，登录会失败并出现 `unexpected JWT alg` 错误。

## Pocket ID

> [!IMPORTANT]
> Pocket ID 是无密码的，登录需要 passkey。在将 Trilium 切换到 OpenID Connect 之前，请先注册一个 passkey 并确认你能登录 Pocket ID 本身，否则你可能会发现两端都无法认证。

1.  以管理员身份登录 Pocket ID，进入 _OIDC 客户端（OIDC Clients）_ → _添加 OIDC 客户端（Add OIDC Client）_。
2.  给它一个名字（例如 Trilium）。
3.  将_回调 URL（Callback URLs）_设置为 `https://<server>/callback`，将 `<server>` 替换为你的 Trilium 实例的 URL。
4.  保存，然后复制_客户端 ID（Client ID）_、_客户端密钥（Client Secret）_和 _OIDC 发现 URL（OIDC Discovery URL）_。

按如下方式调整 `config.ini`，使用步骤 4 中的值：

```
[MultiFactorAuthentication]
oauthBaseUrl=https://<server>
oauthClientId=<ClientID>
oauthClientSecret=<ClientSecret>
oauthIssuerBaseUrl=<OIDCDiscoveryURL>
oauthIssuerName=Pocket ID
```

> [!NOTE]
> Pocket ID 会给你一个以 `/.well-known/openid-configuration` 结尾的 _OIDC 发现 URL_。在 v0.105.0 及以后版本，你可以按原样粘贴。在更早的版本上，请移除该后缀，只使用它之前的部分。

> [!IMPORTANT]
> Pocket ID 在首次启动时生成一个 RS256 签名密钥，无需进一步配置即可工作。如果你将其轮换为 Ed25519（`pocket-id key-rotate --alg EdDSA --crv Ed25519`），ID 令牌将改用 EdDSA 签署。Trilium 在 v0.105.0 及以后版本会自动检测；更旧的版本会失败并出现 `unexpected JWT alg received, expected RS256, got: EdDSA`。

## GitHub

GitHub 不能用作身份提供商，因为它只是纯 OAuth 2.0 而非 OpenID Connect，所以会失败并出现 `OAUTH_RESPONSE_IS_NOT_CONFORM`。

## Google

1.  进入 [Google Cloud 的客户端（Clients）](https://console.cloud.google.com/auth/clients)仪表板并选择_创建客户端（Create client）_。
2.  对于_应用类型（Application type）_，选择_Web 应用（Web application）_。
3.  在_授权重定向 URI（Authorized redirect URIs）_中，设置 `https://<server>/callback`。
4.  按_创建（Create）_并复制_客户端 ID（Client ID）_和_客户端密钥（Client secret）_。

按如下方式调整 `config.ini`，将 `<ClientID>` 和 `<ClientSecret>` 替换为上一步的值，并将 `<server>` 替换为你的 Trilium 实例的 URL。

```
[MultiFactorAuthentication]
oauthBaseUrl=https://<server>
oauthClientId=<ClientID>
oauthClientSecret=<ClientSecret>
```""",

"z2cMreTyvWVz": """作为[托管你自己的 Trilium 实例](#root/_help_Dgg7bR3b6K9j)的替代方案，有两个服务提供对 Trilium 的开箱即用支持。

> [!IMPORTANT]
> **免责声明**：Trilium Notes 项目和维护者与这两个项目没有直接关联。
> 
> PikaPods 已善意地为我们提供了免费额度用于测试。

## 云实例提供商

### PikaPods

1.  前往 [pikapods.com](https://www.pikapods.com) 并注册。
2.  在"可用应用（Available Apps）"部分，查找 "TriliumNext" 并选择"运行你自己的（Run your own）"。
3.  按照屏幕上的说明设置你自己的云托管实例。

PikaPods 通常会在新版本发布后的两周间隔内将其 Trilium 实例更新到最新版本。

## 让你的版本与云实例匹配

请注意，一旦你在云实例和[桌面](#root/_help_poXkQfguuA0U)客户端之间设置了<a class="reference-link" href="#root/_help_cbkrhQjrkKrh">同步（Synchronization）</a>，桌面应用的版本与服务器的版本匹配就很重要。

设置云实例时，最好通过 Web 浏览器访问服务器并进入 _关于（About）_部分来检查服务器的版本。桌面和服务器最好具有相同的_应用版本（App version）_；不过，如果桌面与服务器具有相同的_同步版本（Sync version）_，将桌面更新到比服务器更新的版本通常也是可以的。

如果服务器与桌面应用之间的_同步版本（Sync version）_不匹配，同步将无法工作。""",

"6OOBk6hUKjLW": """*   使用 Docker，服务器可以在 Windows、Linux 和 macOS 设备上运行。
*   为 Linux x64 和 ARM（`aarch64`）提供了原生二进制文件。

## 旧版 ARM 支持

Docker 构建还提供 `linux/arm/v7` 和 `linux/arm/v8` 平台。这些平台被认为是旧版，因为 Trilium 使用的 Node.js 版本 24 已[官方降级](https://github.com/nodejs/node/commit/6682861d6f)了对这些平台的支持为"实验性"。

因此，Trilium 需要为这些版本使用 Node.js 22。一旦 Node.js 22 不再兼容，对 `armv7` 和 `armv8` 的支持将被完全移除。

无论上游支持如何，这些平台都是尽最大努力支持的，并非由 Trilium 开发团队官方支持。接受错误报告，但不会优先处理；欢迎贡献代码。""",
}
