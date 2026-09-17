# -*- coding: utf-8 -*-
import json
# 人工映射表: noteId -> docName
# 依据: 标题(英文括号/中文) + 树路径 + 内容特征 + docName 全集
MAPPING = {
    # ===== 顶层 =====
    'Td5XP4paGGvm': 'User Guide/User Guide/FAQ',                                  # FAQ(常见问题)
    't9tDQ3yYHSIQ': 'User Guide/User Guide/Feature Highlights',                   # 功能亮点
    '6at3wkEnLIfu': 'User Guide/User Guide/Quick Start',                          # 快速开始
    # ===== 安装与设置 =====
    'v4jDFrdyA3XU': 'User Guide/User Guide/Installation & Setup/Upgrading TriliumNext',  # 升级 TriliumNext
    'eT4hTrNrfbR0': 'User Guide/User Guide/Installation & Setup/Backup',          # 备份
    # ---- 桌面安装 ----
    'aIwhuIM42UXZ': 'User Guide/User Guide/Installation & Setup/Desktop Installation/Using the desktop application as a server',
    'a3TKVvk1Jcuq': 'User Guide/User Guide/Installation & Setup/Desktop Installation/System Requirements',
    'kBQZqj9dubVL': 'User Guide/User Guide/Installation & Setup/Desktop Installation/Nix flake',
    'KENrlyFaUbZN': 'User Guide/User Guide/Installation & Setup/Desktop Installation/Network Access',
    'I9TEAj9A5NIv': 'User Guide/User Guide/Installation & Setup/Desktop Installation/Troubleshooting',
    # ---- 服务器安装 ----
    '3jBts3KWOAAP': 'User Guide/User Guide/Installation & Setup/Server Installation/HTTPS (TLS)',
    'NwgMJByVeFyC': 'User Guide/User Guide/Installation & Setup/Server Installation/Multi-factor authentication with TOTP',
    'Ht8ZUwhpA1DH': 'User Guide/User Guide/Installation & Setup/Desktop Installation/Nix flake',  # 服务器安装下的 Nix flake（同一文档）
    'z2cMreTyvWVz': 'User Guide/User Guide/Installation & Setup/Server Installation/Third-party cloud hosting',
    '6OOBk6hUKjLW': 'User Guide/User Guide/Installation & Setup/Server Installation/System Requirements',
    # 1. Installing the server
    'sPaAXjVcvZZl': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/Packaged version for Linux',
    '5HwfHJFwczl9': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/Using Docker',
    'SEYvbzIDbgtV': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/On NixOS',
    'fEvxRDgByx6J': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/Manually',
    'aFYvSvzqySw1': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/Using Kubernetes',
    'fupEm5MLR0eF': 'User Guide/User Guide/Installation & Setup/Server Installation/1. Installing the server/Multiple server instances',
    # 2. Reverse proxy
    'o5byGpiYwkAf': 'User Guide/User Guide/Installation & Setup/Server Installation/2. Reverse proxy/Nginx',
    '74oHxA6XDX7Z': 'User Guide/User Guide/Installation & Setup/Server Installation/2. Reverse proxy/Apache using Docker',
    '9H8FKhPVYLTX': 'User Guide/User Guide/Installation & Setup/Server Installation/2. Reverse proxy/Trusted proxy',
    'mWpKWRZZLu3R': 'User Guide/User Guide/Installation & Setup/Server Installation/2. Reverse proxy/Traefik',
    # Authentication
    'eaZEEY2i4YT2': 'User Guide/User Guide/Installation & Setup/Server Installation/Authentication/Resetting your password',
    # OpenID Connect
    'v14qii09xWVc': 'User Guide/User Guide/Installation & Setup/Server Installation/Signing in with OpenID Connect/Setting up with various providers',
    # ===== 基本概念与功能 =====
    'X3YqkCaUUU4u': 'User Guide/User Guide/Basic Concepts and Features/Active content',  # 活动内容
    'lJ6ySSiTSr3j': 'User Guide/User Guide/Basic Concepts and Features/Keyboard Shortcuts',  # 键盘快捷键(顶层)
    # 界面元素
    'JHsKupgT1tda': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/Zoom',  # 缩放
    '5huozA6LMahz': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/Note types with split view',  # 支持分屏视图的笔记类型
    'Etxj5aRT3vkB': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/Content width',  # 内容宽度
    'JqFcpg0mfise': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/Note Tree/Keyboard shortcuts',  # 键盘快捷键(笔记树)
    'EWv4L9CU9Bzk': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/Right Sidebar/AI chat tab',  # AI 对话标签页
    'NTVYMnkwiZCF': 'User Guide/User Guide/Basic Concepts and Features/UI Elements/New Layout/Status bar',  # 状态栏
    # 笔记
    'C0gsLjbVjkhn': 'User Guide/User Guide/Basic Concepts and Features/Notes/Title',  # 标题
    '6FRj4QSfbL9T': 'User Guide/User Guide/Basic Concepts and Features/Notes/Attachments',  # 附件
    '8kz4JE6Llklc': 'User Guide/User Guide/Basic Concepts and Features/Notes/Cloning Notes/Branch prefix',  # 前缀
    'Aol4QMxJQ106': 'User Guide/User Guide/Basic Concepts and Features/Notes/Sorting Notes',  # 笔记排序
    'v0ST0oYz6iSM': 'User Guide/User Guide/Basic Concepts and Features/Notes/Read-Only Notes',  # 只读笔记
    'kEA3GOPRnzx7': 'User Guide/User Guide/Basic Concepts and Features/Notes/Note Inbox',  # 笔记收件箱
    'lwnKte9zowVK': 'User Guide/User Guide/Basic Concepts and Features/Notes/Restoring Deleted Notes',  # 恢复已删除的笔记
    # 导航
    'd8rJUlge3Be1': 'User Guide/User Guide/Basic Concepts and Features/Navigation/Tree Concepts',  # 树的概念
    'zSF9gd8EeVtg': 'User Guide/User Guide/Basic Concepts and Features/Navigation/Bookmarks',  # 书签
    # 主题
    'QMdizlUHF0tN': 'User Guide/User Guide/Basic Concepts and Features/Themes/Theme Gallery',  # 主题库
    # 导入与导出
    '0FBBMpFaI2jU': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Markdown/Supported syntax',  # 支持的语法
    'L0HLzREIQ8E9': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Anytype',
    '1XvW40oOLX0w': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Microsoft OneNote',
    'FCd0fUcq1TF8': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Notion',
    'WaA1iBEA7o8D': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Google Keep',
    'syL2rkuSPUXB': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Obsidian',
    'k8g7PA6MK9oa': 'User Guide/User Guide/Basic Concepts and Features/Import & Export/Importing data from other applications/Evernote',  # 按标题处理内部链接
    # ===== 笔记类型 =====
    'qfffwa20agq7': 'User Guide/User Guide/Note Types/Web View',  # 网页视图
    '2FGr3nWmquVd': 'User Guide/User Guide/Note Types/Converting between note types',  # 在笔记类型之间转换
    'GNyhAHMsfcne': 'User Guide/User Guide/Note Types/File/Office documents',  # Office 文档
    'IEeQqk3crB3k': 'User Guide/User Guide/Note Types/Mermaid Diagrams/ELK layout',  # ELK 布局
    'yzyWwxCeBHm2': 'User Guide/User Guide/Note Types/Mermaid Diagrams',  # 语法参考(Mermaid 主文档)
    # 文本
    'NQzDmfb1Rz2v': 'User Guide/User Guide/Note Types/Text/Automatic replacements',  # 自动替换
    'LhQmZX32MQ0c': 'User Guide/User Guide/Note Types/Text/Collapsible blocks',  # 可折叠块
    'Ax2xW59Ff3jb': 'User Guide/User Guide/Note Types/Text/Highlights list',  # 高亮列表(已迁移)
    'ZpqlSTz8sFPI': 'User Guide/User Guide/Note Types/Text/Spell Check',  # 拼写检查
    '4aQnfnnr9AKb': 'User Guide/User Guide/Note Types/Text/Table of contents',  # 目录(已迁移)
    '5JQ4vkZqncQO': 'User Guide/User Guide/Note Types/Text/Text Snippets',  # 文本片段
    'NsIGoRoDUTG8': 'User Guide/User Guide/Note Types/Text/Keyboard shortcuts',  # 键盘快捷键(文本)
    'pN4jKprod7oX': 'User Guide/User Guide/Note Types/Text/Premium features',  # 高级功能
    'yAM7BBICJYqk': 'User Guide/User Guide/Note Types/Text/Markdown-like formatting',  # 类 Markdown 格式
    'NRzmLnGiDb8U': 'User Guide/User Guide/Note Types/Text/Links/Internal (reference) links',  # 内部（引用）链接
    'hcZlVY6xuwTM': 'User Guide/User Guide/Note Types/Text/Links/Backlinks',  # 反向链接
    'SAlWCKOJjNBy': 'User Guide/User Guide/Note Types/Text/In-editor AI assistant/Custom AI quick actions',  # 自定义 AI 快捷操作
    # ===== 故障排除 =====
    'G6uh5oxvadSV': 'User Guide/User Guide/Troubleshooting/Reporting issues',  # 报告问题
    'GDf62xzQAPH5': 'User Guide/User Guide/Troubleshooting/Refreshing the application',  # 刷新应用
    'PFtFDf3JuA4D': 'User Guide/User Guide/Troubleshooting/Synchronization fails with 504 Gateway Timeout',  # 同步失败并报 504
    'bBQ7RKmRvKlM': 'User Guide/User Guide/Troubleshooting/Error logs/Backend (server) logs',  # 后端（服务器）日志
    # ===== 高级用法 =====
    'ArrYKBJoy7G0': 'User Guide/User Guide/Advanced Usage/Read-only database',  # 只读数据库
    'VQ3ULRNr3aRq': 'User Guide/User Guide/Advanced Usage/Nightly release',  # 夜间构建版本
    'sFWMuPR2S3QB': 'User Guide/User Guide/Advanced Usage/Safe mode',  # 安全模式
    'o672rnPvm0jD': 'User Guide/User Guide/Advanced Usage/Note ID',  # 笔记 ID
    '2P0eR6E4qLOA': 'User Guide/User Guide/Advanced Usage/Text Extraction (OCR)',  # 文本提取 (OCR)
    'OA7pdMSIVH5t': 'User Guide/User Guide/Advanced Usage/Customizing to-do task states',  # 自定义待办任务状态
    'sKi0De0UzUbQ': 'User Guide/User Guide/Advanced Usage/Custom Request Handler',  # 自定义请求处理器
    'HDOtajV3qR0i': 'User Guide/User Guide/Advanced Usage/Custom Resource Providers',  # 自定义资源提供程序
    'SJz4TOTAnudY': 'User Guide/User Guide/Advanced Usage/Default Note Title',  # 默认笔记标题
    'P5P0lLNUT9eC': 'User Guide/User Guide/Advanced Usage/Metrics',  # grafana-dashboard.json
    'pOHxWBPSPF4K': 'User Guide/User Guide/Advanced Usage/Technologies used/CKEditor',  # CKEditor
    'H6xRarZDzD4p': 'User Guide/User Guide/Advanced Usage/Database/Demo Notes',  # 演示笔记
    'VugMPu4jc7IZ': 'User Guide/User Guide/Advanced Usage/ETAPI (REST API)',  # API 参考
    'hWMN4ej0xMh3': 'User Guide/User Guide/Advanced Usage/Hidden Notes',  # 内部 API/API 参考（webView 容器）
    # 属性
    'xA1k6rzHw1jJ': 'User Guide/User Guide/Advanced Usage/Attributes/Labels',  # Labels(标签)
    'SBcn989ASCLZ': 'User Guide/User Guide/Advanced Usage/Attributes/Relations',  # 关系
    '9achoYMafjHP': 'User Guide/User Guide/Advanced Usage/Attributes/Attribute Inheritance',  # 属性继承
    # 代码片段
    'mme9Dn0AMMYr': 'User Guide/User Guide/Advanced Usage/Snippets/Markdown Snippets',  # Markdown 片段
    'vkgg8iM2mHvN': 'User Guide/User Guide/Advanced Usage/Snippets/Code Snippets',  # 代码片段
    # 分享
    'dPPVSl1OVX4A': 'User Guide/User Guide/Advanced Usage/Sharing/Exporting static HTML for web publishing',  # 导出静态 HTML 用于网页发布
    'dccVjGrNHB5o': 'User Guide/User Guide/Advanced Usage/Sharing/Reverse proxy configuration',  # 反向代理配置
    'hF1RdP002030': 'User Guide/User Guide/Advanced Usage/Sharing/Custom share template',  # 自定义分享模板
    # Configuration
    'nrdIPDm87oD3': 'User Guide/User Guide/Advanced Usage/Configuration (config.ini or environment variables)/Trilium instance',  # Trilium 实例
    'S9b1TAsJZF1u': 'User Guide/User Guide/Advanced Usage/Configuration (config.ini or environment variables)/Cross-Origin Resource Sharing (CORS)',  # CORS
    # ===== 脚本 =====
    'TNBOr62S7Osb': 'User Guide/User Guide/Scripting/Security',  # 安全
    '5uFWhktAIRyu': 'User Guide/User Guide/Scripting/Common concepts/Script bundles',  # 脚本包
    'WLwB4rf7Dqu4': 'User Guide/User Guide/Scripting/Frontend Basics/Frontend Events',  # 前端事件
    # 自定义小组件
    'R6SqwFdNe39J': 'User Guide/User Guide/Scripting/Frontend Basics/Custom Widgets/Widget Basics',  # 小组件基础
    'J2vVDbQtQZtM': 'User Guide/User Guide/Scripting/Frontend Basics/Custom Widgets/Note context aware widget',  # 感知笔记上下文的小组件
    'KNgNIbGyOwdC': 'User Guide/User Guide/Scripting/Frontend Basics/Custom Widgets/Right pane widget',  # 右侧面板小组件
    '79rSvQaP5JCE': 'User Guide/User Guide/Scripting/Frontend Basics/Custom Widgets/CSS',  # CSS(CSS)
    'KXp59dH02JDZ': 'User Guide/User Guide/Scripting/Frontend Basics/Custom Widgets/Troubleshooting',  # 故障排除
    # 示例
    'cNDws8XZlCoV': 'User Guide/User Guide/Scripting/Frontend Basics/Examples/New Task launcher button',  # "新建任务"启动按钮
    'stmJLtrW8j2H': 'User Guide/User Guide/Scripting/Frontend Basics/Examples/Downloading responses from Google Forms',  # 从 Google 表单下载回复
    # Preact
    '4hBLB6wTZ1Zv': 'User Guide/User Guide/Scripting/Frontend Basics/Preact/CSS',  # CSS
    '7cAfhPfEAIrM': 'User Guide/User Guide/Scripting/Frontend Basics/Preact/Hooks',  # Hooks
    'xhnTlJGRE7Te': 'User Guide/User Guide/Scripting/Frontend Basics/Preact/Component libraries',  # 组件库
    'iyj7sZ20qUw8': 'User Guide/User Guide/Scripting/Frontend Basics/Preact/Built-in components',  # 小组件展示
    # 后端脚本
    '1dZhMvJmxKPn': 'User Guide/User Guide/Scripting/Backend scripts/Backend Events',  # 后端事件
    'uqfol1gEV8BT': 'User Guide/User Guide/Scripting/Backend scripts/Server-side imports',  # 服务端导入
    # 脚本 API
    'F695Rxy3UEeT': 'User Guide/User Guide/Scripting/Script API/Day.js',  # Day.js
    'jFXOYH8BO8zS': 'User Guide/User Guide/Scripting/Script API',  # 后端 API
    '20c3yUXh3jcZ': 'User Guide/User Guide/Scripting/Script API',  # Electron API
    'BROicBbRhY1u': 'User Guide/User Guide/Scripting/Script API',  # FNote（前端 API）
    # 破坏性变更
    '8ea9bUjqQm46': 'User Guide/User Guide/Scripting/Breaking changes/v0.102.0 Upgrade to jQuery 4.0.0',  # v0.102.0
    'MvkvDstoBpsO': 'User Guide/User Guide/Scripting/Breaking changes/v0.103.0 Removal of axios',  # v0.103.0 axios
    'mf5d77kGDVvQ': 'User Guide/User Guide/Scripting/Breaking changes/v0.103.0 `cheerio` is now deprecated',  # v0.103.0 cheerio
    'HqE8ndZ8OVlv': 'User Guide/User Guide/Scripting/Breaking changes/v0.104.0 Disabling the Node integration in Electron',  # v0.104.0
    # ===== 主题开发 =====
    'ShThQsAzq8H9': 'User Guide/User Guide/Theme development/Reference',  # 参考
    'ip1BHr5q3vY4': 'User Guide/User Guide/Theme development/Creating an icon pack',  # 创建图标包
    # ===== 杂项 =====
    'NoLYuQ9qUQg7': 'User Guide/User Guide/Miscellaneous/Privacy Policy',  # 隐私政策
    'tJxRbIWRmEp8': 'User Guide/User Guide/Miscellaneous/License',  # License(许可证)
    # ===== AI =====
    '7Tc9Zf7xh8Xs': 'User Guide/User Guide/AI/MCP',  # MCP
    'yekICwqk31HM': 'User Guide/User Guide/AI/Providers',  # 提供程序
    'G85N2AiEbbuW': 'User Guide/User Guide/AI/Privacy',  # 隐私
    # ===== 杂项/个人知识模式 =====
    'Bgf8cWUPyF9q': 'User Guide/User Guide/Miscellaneous/Patterns of personal knowledge',  # 个人知识模式（已在 docPath? 检查）
}
json.dump(MAPPING, open('_manual_mapping.json','w',encoding='utf-8'), ensure_ascii=False, indent=1)
print('映射条目:', len(MAPPING))
