# 笔记软件

## Trilium Notes

https://github.com/TriliumNext/Trilium

结合你「程序+SQLite全放U盘、免安装即插即用」的核心需求，同时兼顾上一轮提到的MCP接入AI能力，按匹配度整理如下方案：

- **便携方式**：官方直接提供 **Portable 便携压缩包**，解压到U盘即可运行，默认数据就存放在程序目录下的 `trilium-data` 文件夹中，完全不写入系统盘和注册表，插任何电脑都能直接打开。
- **存储形式**：所有笔记、附件、配置、版本历史全部存在单个 `document.db` SQLite 数据库中，备份只需复制这一个文件。
- **核心功能**：树状层级笔记、双向链接、笔记克隆、富文本+Markdown双模式、JavaScript脚本扩展、端到端加密、完整版本历史，支持十万级笔记体量。
- **MCP扩展**：自带 REST API，可通过第三方 MCP 适配器接入AI，社区已有成熟的对接方案。

## lapisnote

https://github.com/zexadev/lapisnote

- **本地安装**：基于 Tauri 2 + Rust 构建的原生桌面应用，支持 Windows / macOS / Linux，安装包小巧、启动速度快
- **SQLite 存储**：所有笔记数据完整存储在本地 SQLite 数据库中，默认路径 `%APPDATA%/com.jdnotes.app/`，可在设置中自定义存储位置
- **MCP 集成**：应用启动时自动在 `127.0.0.1:19230` 开启 MCP Server，并自动注册到 Claude Code，AI 可直接读写笔记

**额外亮点**：
- 内置 AI 写作助手，支持 DeepSeek、OpenAI、Claude、Gemini、Ollama 本地模型，可同时配置多模型切换
- 支持 Markdown 编辑、标签系统、日历视图、全文搜索、废纸篓、提醒等完整笔记功能
- 支持导出 Markdown / PDF

## Obsidian

完全本地、纯 Markdown、可双向链接、不绑架数据的知识库

1. Local‑first 本地优先：笔记全部是本地 Markdown 文件；
2. 核心功能个人永久免费，无需注册账号；
3. 高度可扩展，开放插件 API，交给社区扩展能力。

## 思源笔记

https://github.com/siyuan-note/siyuan