# openclaw

## 安装

下面给你 **OpenClaw 完整安装+初始化+验证** 步骤（按系统分，官方推荐方式，小白也能跑）。

一、先看系统要求（必须满足）

- **Node.js ≥ 22**（安装器会自动装，不用自己搞）
- **内存 ≥ 4GB**（本地跑 LLM 建议 8GB+）
- **系统**：
  - macOS：原生支持
  - Linux：原生支持
  - Windows：**强烈建议用 WSL2（Ubuntu）**，原生 PowerShell 兼容性一般

二、安装方式（3种，选一种）

方式1：一键安装脚本（最推荐，3分钟搞定）

macOS / Linux / WSL2（Ubuntu）

```bash
curl -fsSL https://openclaw.ai/install.sh | bash
```
- 自动：检测系统 → 装 Node 22 → 全局装 `openclaw` → 启动初始化向导
- 升级/重装：直接再跑一遍脚本即可

Windows（原生 PowerShell，不推荐，优先 WSL2）

```powershell
# 先开管理员 PowerShell
Set-ExecutionPolicy RemoteSigned -Scope CurrentUser
iwr -useb https://openclaw.ai/install.ps1 | iex
```

方式2：手动 npm 安装（已有 Node 22+）

```bash
# 全局安装
npm install -g openclaw@latest
# 或用 pnpm（更快）
pnpm add -g openclaw@latest

# 验证安装
openclaw --version
```

方式3：源码安装（开发者/改代码）

```bash
git clone https://github.com/openclaw/openclaw.git
cd openclaw
pnpm install
pnpm ui:build
pnpm build
pnpm link --global

# 验证
openclaw --version
```

三、初始化配置（最关键：onboard 向导）

安装完**必须跑向导**，配置模型、API、网关、聊天入口：
```bash
# 启动向导（自动装后台守护进程，开机自启）
openclaw onboard --install-daemon
```

向导会一步步问你：

1. **选择模型提供商**：
   - Anthropic（Claude 3.5/4，推荐）
   - OpenAI（GPT-4o）
   - Google Gemini
   - Ollama（本地模型）
2. **输入 API Key**（去对应平台申请）
3. **设置 Gateway 端口**：默认 `18789`
4. **配置聊天渠道**：Telegram / Discord / WhatsApp / iMessage 等（选一个先测）
5. **安装后台服务**：macOS（launchd）、Linux（systemd）、Windows（WSL2 systemd）

四、启动与验证

1. 启动 Gateway（后台服务）

```bash
# 查看状态
openclaw gateway status
# 手动启动（服务没起来时）
openclaw gateway start
# 重启
openclaw gateway restart
```

1. 验证健康

```bash
openclaw health
# 输出：✅ All checks passed 即正常
```

1. 测试智能体（发消息）

- 用你配置的聊天渠道（如 Telegram）给 OpenClaw 发消息：
  ```
  帮我查一下北京今天天气
  ```
- 或用 CLI 直接测试：
  ```bash
  openclaw chat "你好，介绍一下自己"
  ```

五、常见问题与排错

1. **Node 版本太低**：
   ```bash
   # 用 nvm 装 Node 22
   curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
   source ~/.bashrc
   nvm install 22
   nvm use 22
   ```
2. **sharp 安装失败（macOS）**：
   ```bash
   SHARP_IGNORE_GLOBAL_LIBVIPS=1 npm install -g openclaw@latest
   ```
3. **权限问题（Linux）**：
   ```bash
   # 修复 npm 全局权限
   mkdir -p ~/.npm-global
   npm config set prefix '~/.npm-global'
   echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
   source ~/.bashrc
   ```
4. **WSL2 无法启动服务**：
   ```bash
   # 启用 WSL2 systemd
   echo "[boot]" | sudo tee -a /etc/wsl.conf
   echo "systemd=true" | sudo tee -a /etc/wsl.conf
   # 重启 WSL
   wsl --shutdown
   ```

六、一句话快速流程（复制粘贴）

```bash
# macOS/Linux/WSL2
curl -fsSL https://openclaw.ai/install.sh | bash
openclaw onboard --install-daemon
openclaw gateway start
openclaw chat "你好"
```

## Ollama 接入

