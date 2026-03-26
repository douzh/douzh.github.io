# Python 多版本管理（完美配合 uvx）
如果你本地需要**同时用 Python 3.9 / 3.10 / 3.11 / 3.12**，且不同软件要求不同版本，最干净、最简单、最通用的方案是：

## 推荐方案：uv + pyenv（Windows 用 pyenv-win）
- **pyenv**：专门**管理多个 Python 版本**（安装/切换/卸载）
- **uv / uvx**：在指定 Python 版本下**极速创建环境、运行工具**
- 组合起来 = 本地任意切换 Python 版本 + 零污染 + 速度拉满

---

## 一、安装 pyenv（多版本管理器）
### 1. macOS / Linux
```bash
# 安装 pyenv
curl https://pyenv.run | bash
```

然后把下面配置加到 `~/.zshrc` 或 `~/.bashrc`：
```bash
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
```
重启终端生效。

### 2. Windows
用 **pyenv-win**
```powershell
# PowerShell 执行
Invoke-WebRequest -UseBasicParsing -Uri "https://raw.githubusercontent.com/pyenv-win/pyenv-win/master/pyenv-win/install-pyenv-win.ps1" -OutFile "./install-pyenv-win.ps1"; &"./install-pyenv-win.ps1"
```
重启终端。

---

## 二、用 pyenv 安装多个 Python 版本
```bash
# 查看可安装版本
pyenv install --list

# 安装你需要的版本
pyenv install 3.9.20
pyenv install 3.10.15
pyenv install 3.11.11
pyenv install 3.12.8
```

### 查看已安装
```bash
pyenv versions
```

---

## 三、3 种切换方式（超实用）
### 1. 全局默认版本（整个电脑）
```bash
pyenv global 3.11.11
```

### 2. 当前文件夹版本（项目专用）
```bash
pyenv local 3.9.20
```
进入这个目录自动用 3.9.20，离开自动恢复全局。

### 3. 当前终端临时版本
```bash
pyenv shell 3.12.8
```

---

## 四、配合 uvx 使用（指定 Python 版本运行工具）
这是你最关心的：**某个软件要求 Python 3.9，怎么用 uvx 运行？**

### 方法 1：先切换版本，再用 uvx
```bash
pyenv local 3.9.20
python --version   # 确认是 3.9.20
uvx your-tool      # 自动用当前 Python 3.9 运行
```

### 方法 2：uvx 直接指定 Python 版本（最爽）
```bash
uvx --python 3.9 your-tool
uvx --python 3.10 your-tool@2.0.0
```
**不需要切换环境，一行命令指定版本运行！**

---

## 五、最简单的替代方案（只用 uv）
如果你不想装 pyenv，**uv 自己就能管理 Python 版本**：
```bash
# 让 uv 自动安装 Python 3.9
uv python install 3.9
uv python install 3.10

# 查看已安装
uv python list

# 切换默认版本
uv python pin 3.11
```

然后运行：
```bash
uvx --python 3.9 你的工具名
```

---

## 六、最佳实践总结（你直接照抄用）
```bash
# 1. 安装需要的 Python 版本
uv python install 3.9 3.10 3.11 3.12

# 2. 工具要求 Python 3.9 就运行
uvx --python 3.9 工具名

# 3. 工具要求 Python 3.10 就运行
uvx --python 3.10 工具名
```

---

### 总结
- **pyenv / uv python**：管理多版本 Python
- **uvx --python x.x**：在指定版本下运行工具
- **零污染、极速、不冲突、跨平台**