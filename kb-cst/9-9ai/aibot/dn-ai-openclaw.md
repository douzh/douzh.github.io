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

把 **Ollama 接入 OpenClaw**，核心是：**先跑 Ollama 服务 → 配置 OpenClaw 指向 Ollama → 启动 OpenClaw**。下面是完整步骤（Windows/macOS/Linux 通用）。

**一、先准备 Ollama（本地大模型服务）**

1. 安装 Ollama

- 官网下载：https://ollama.com/
- 安装后，终端/命令行可用 `ollama` 命令。

1. 拉取并运行一个模型（以 qwen2.5:7b 为例）

```bash
# 拉取模型（按需换：llama3.1:8b、qwen3:4b、gemma2:9b 等）
ollama pull qwen2.5:7b

# 启动 Ollama 服务（后台常驻，默认端口 11434）
ollama serve
# 或直接运行模型（会自动启动服务）
ollama run qwen2.5:7b
```
- 验证服务：浏览器打开 `http://localhost:11434`，显示 `Ollama is running` 即成功。

**二、接入 OpenClaw（两种方式：配置文件 / 向导）**

**方式 A：编辑配置文件（推荐，稳定可控）**

1. 找到/创建 OpenClaw 配置文件：
   - Windows：`%USERPROFILE%\.openclaw\openclaw.json`
   - macOS/Linux：`~/.openclaw/openclaw.json`
2. 写入 Ollama 配置（**关键：provider=ollama + base_url + model 与 ollama list 一致**）：
```json
{
  "provider": "ollama",
  "model": "qwen2.5:7b",
  "base_url": "http://localhost:11434",
  "gateway": {
    "enabled": true,
    "port": 3001
  },
  "webui": {
    "enabled": true,
    "port": 3000
  },
  "tools": {
    "enabled": true
  }
}
```
- 模型名必须和 `ollama list` 输出完全一致（如 `qwen2.5:7b`、`llama3.1:8b`）。
- 局域网访问：`base_url` 改为 `http://192.168.x.x:11434`（本机局域网 IP）。

**方式 B：用 `openclaw onboard` 向导（新手友好）**

```bash
# 运行配置向导
openclaw onboard
```
按提示选择/输入：
1. Model/auth provider → 选 **Custom Provider**（拉到最后）
2. API Base URL → `http://localhost:11434/v1`（Ollama OpenAI 兼容端点）
3. API Key → 随便填（如 `ollama`，Ollama 无鉴权，仅格式要求）
4. Endpoint compatibility → **OpenAI-compatible**
5. Model ID → 填你拉的模型名（如 `qwen2.5:7b`）
6. 其余默认，完成后保存。

**三、启动 OpenClaw 并验证**

```bash
# 启动 OpenClaw
openclaw start
```
- 访问 WebUI：`http://localhost:3000`
- 发送消息，能正常回复即接入成功。

**四、常见问题与排查**

1. **连接失败 / 超时**
   - 确认 `ollama serve` 正在运行，端口 11434 未被占用。
   - 配置文件 `base_url` 正确：`http://localhost:11434`（非 `/v1`，除非用 OpenAI 兼容模式）。
   - 防火墙放行 11434、3000、3001 端口。

2. **模型不存在 / 加载失败**
   - 运行 `ollama list` 核对模型名，配置文件 `model` 字段必须完全匹配。
   - 重新拉取：`ollama pull qwen2.5:7b`。

3. **性能慢 / 内存不足**
   - 换小模型：`qwen3:4b`、`llama3.1:8b`（推荐 16GB+ 内存）。
   - Ollama 自动用 GPU（NVIDIA/AMD），确保显卡驱动正常。

**五、安全提示（对应你之前的 OpenClaw 安全说明）**

- 本地个人使用：默认安全，仅本机访问。
- 多用户/共享：务必做**白名单、沙箱、最小权限**，不要暴露到公网。
- 定期审计：`openclaw security audit --deep --fix`。
