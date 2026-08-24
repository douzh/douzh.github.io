# Trilium 可编程性说明

[返回档案](README.md) | [数据库设计](db-design.md)

一句话:**笔记即程序**——脚本是笔记、界面组件是笔记、REST 端点是笔记、数据模型也是笔记,全部住在同一棵树里,用属性声明行为。

## 一、可编程性总览:五层能力

| 层 | 机制 | 能做什么 | 门槛 |
| --- | --- | --- | --- |
| L1 声明式 | 属性 + 模板 | 不写代码:promoted 属性自动生成表单、模板继承、看板/日历视图 | 低 |
| L2 前端脚本 | JS(`env=frontend`) | 改 UI:自定义组件、快捷键、工具栏按钮 | 中 |
| L3 后端脚本 | JS(`env=backend`) | 改数据:定时任务、事件响应、批量处理、爬虫 | 中高 |
| L4 SQL 控制台 | 直接查 SQLite | 任意查询 15 张表,聚合统计 | 中(懂 SQL) |
| L5 开放接口 | ETAPI(REST)/ customRequest | 外部程序读写笔记库、自定义 HTTP 端点 | 高 |

## 二、脚本笔记:核心机制

### 2.1 脚本就是普通笔记

创建方式三步:新建笔记 → 类型选 `Code` → MIME 指定运行环境:

```
application/javascript;env=frontend   ← 前端脚本(渲染进程,能操作 UI)
application/javascript;env=backend    ← 后端脚本(Node.js 服务端,能碰数据库)
```

### 2.2 触发方式:`#run` 属性

| 属性值 | 触发时机 |
| --- | --- |
| (无) | 手动:笔记工具栏 ▶️ 按钮执行 |
| `#run=frontendStartup` | 前端启动时(桌面/浏览器打开) |
| `#run=backendStartup` | 后端启动时 |
| `#run=hourly` / `daily` | 定时任务(小时/每天) |
| `#customRequestHandler=xxx` | 成为自定义 HTTP 端点 `/custom/xxx` 的处理器 |
| 挂 `#widget` 属性 | 作为 UI 组件注入界面(见 4) |
| 挂 `~renderNote` 关系 | 作为笔记内嵌迷你应用(见 5) |

### 2.3 两套 API 的分工

**后端(BackendScriptApi,`src/services/backend_script_api.js`)**:

```js
// 笔记 CRUD + 搜索 + 事务
const {note} = api.createTextNote(parentNoteId, "标题", "<p>内容</p>");
const results = api.searchForNotes("#book AND #作者=@鲁迅");
note.setLabel("status", "done");
api.transactional(() => { /* 批量原子操作 */ });
api.log("日志");            // 写入后端日志
// 还有:日期工具、定时器、cheerio(HTML解析)、http 请求、文件系统访问
```

**前端(FrontendScriptApi)**:

```js
const note = await api.getActiveNote();        // 当前打开的笔记
api.showMessage("完成");                       // toast 提示
api.addButtonToToolbar({...});                 // 加工具栏按钮
api.addContextMenuItem({...});                 // 加右键菜单
api.bindGlobalShortcut("ctrl+alt+k", fn);      // 全局快捷键
api.activateNoteById("xxx");                   // 程序化跳转
```

前后端可互相通信(`api.sendMessageToConsole` / 消息总线),典型分工:后端管数据加工,前端管交互呈现。

## 三、属性即程序:声明式层(不写代码)

这是 Trilium 最容易被低估的可编程性——**属性系统本身是一套声明式语言**:

| 属性 | 编程语义 |
| --- | --- |
| `#template`(可继承) | 类型继承:子笔记自动获得模板的属性结构 |
| `~template` 关系 | 引用模板:实例化一个"类" |
| `#promoted` | 属性自动升级为表单字段(UI 生成) |
| `#inheritable` | 属性沿树继承(类 CSS) |
| `#label:xxx` / `#relation:xxx` | 类型约束(schema 校验) |
| `#keyboardShortcut` | 启动器绑定快捷键 |
| `#pageSize` / `#viewType` | 控制集合/表格视图行为 |

你库里的启动器模板(`_lbTplLauncherScript`)就是典型:模板定义 `~relation:script [promoted]` + `#label:keyboardShortcut [promoted,text]`,**新建一个启动器笔记 = 实例化一个可配置的脚本入口**,无需碰代码。

## 四、自定义组件(Custom Widgets)

前端脚本 + `#widget` 属性 = 往界面里注入自己的面板/按钮/悬浮窗:

- 官方 demo 的"时钟"、"问候语"就是 widget
- 你的库里有 2 个 widget 脚本(kRq4syhU4BbS、GXTTqy9LJCJw)
- 官方很多内置功能(关系图、最近笔记、日历)本身就是用同一套 widget API 写的——**内置功能与用户脚本同权**

## 五、Render Note:笔记内嵌迷你应用

**最强的玩法**:任何笔记可以声明 `~renderNote` 关系指向一个前端脚本,该笔记的显示内容就变成脚本的渲染输出——**笔记变成一个运行中的小程序**。

```
┌─────────────────────────────┐
│ 笔记 "我的图表"              │
│  ~renderNote @renderPieChart│   ← 属性
├─────────────────────────────┤
│  [脚本渲染出的交互式饼图]     │   ← 打开笔记时执行
└─────────────────────────────┘
```

- 你的库里有 **8 个 renderNote 应用**:renderPieChart(饼图)、renderTable(表格)、JSX 组件(FormElements)等,内容笔记(如 xCVky4eJnA0d)通过关系挂接渲染器
- 模板笔记(text/html)+ JS 脚本(env=frontend)成对出现,HTML 是骨架、JS 是行为
- 数据与渲染分离:同一渲染器可挂到多篇数据笔记上——**等于内置了一个组件框架**
- 官网的话:"从一行微调到完整的自定义应用"

## 六、自定义 HTTP 端点(customRequest)

后端脚本 + `#customRequestHandler=xxx` = 笔记库多出一个 REST 端点 `POST /custom/xxx`,外部程序可直接调用。

你的库里就有一个:`Custom request handler`(YuDMKFaGMJPv)
- `#customRequestHandler=create-note`
- `~targetNote` 关系指向具体笔记
- 效果:外部 POST 到 `/custom/create-note` 即可在库内建笔记——**无需 ETAPI token 的轻量集成通道**

## 七、SQL 控制台

应用内直接对 SQLite 执行任意查询(15 张表,见 [db-design.md](db-design.md)):

- 统计聚合:`SELECT COUNT(*) FROM notes WHERE isDeleted=0 GROUP BY type`
- 关系审计:查孤儿 relation、统计 internalLink 网络
- 搜索引擎查不到的复杂条件(如"列出有 3 个以上反链但无正文空笔记")
- 后端脚本中也能用 `api.sql()` 同样能力

## 八、ETAPI:对外 REST 接口

- 内置 REST API,覆盖笔记/分支/属性/附件/搜索完整 CRUD
- Token 认证(你库里的 `mcp` token 就是走这条路)
- MCP 服务器、同步工具、外部自动化都经此接入
- 详见 [README.md 第四节](README.md)

## 九、v0.104+ 安全变化(重要)

2026-07 的 v0.104(史上最大更新)对可编程性做了**默认收紧**:

| 变化 | 原因 | 恢复方式 |
| --- | --- | --- |
| **后端脚本默认禁用** | 后端脚本有危险权限(如完整文件系统访问),是最大攻击面 | 修改 config.ini 重新启用 |
| **SQL 控制台默认禁用** | 同上,任意 SQL 可读写全库 | config.ini 重启 |
| 桌面 Electron 加固 | 防 RCE;依赖 Electron remote / Node 集成的老前端脚本可能需适配 | — |
| 桌面版默认不开局域网端口 | 影响桌面 ETAPI/桌面互相同步 | Options → Security 开启 |
| ZIP 导入默认"安全模式" | 导入的笔记带脚本会被静默禁用,脚本失效 | 导入对话框取消勾选安全模式 |
| 前端脚本不受影响 | 沙箱权限有限 | — |

**实务建议**:升级 0.104+ 后若脚本/SQL 失效,先查这两项默认值;从别人那导入含脚本的库,记得关安全模式(导入后逐个审查脚本再放行)。

## 十、你的库的实际组成(实测)

| 可编程元素 | 数量 | 实例 |
| --- | --- | --- |
| 代码笔记 | 15+ | 含 `env=backend` / `env=frontend` 两类 |
| renderNote 迷你应用 | 8 | renderPieChart、renderTable、JSX FormElements + 3 个 HTML 模板 |
| 自定义 widget | 2 | 注入 UI 的前端脚本 |
| 启动器 | 5 | builtinWidget / command / customWidget / note / script 各 1 |
| 自定义 HTTP 端点 | 1 | `create-note`(customRequestHandler) |
| 属性模板 | 4+ | 启动器模板、代码片段模板等 |

即:你的库已是一个"笔记 + 数据 + 程序"三合一的运行时——这正是 Trilium 与所有 Notion 系工具(AFFiNE/AppFlowy/Anytype)的根本区别:**后者最多给你插件市场,Trilium 给你一个图灵完备的开发环境**。

## 十一、能力边界与代价

**做不到/不擅长的**:
- 前端脚本跑在受限沙箱,不能随意调 OS API(0.104 加固后更严)
- 没有官方插件市场,脚本靠社区仓库(github awesome-trilium)自取
- 同步到服务端后,移动端浏览器不执行后端脚本定时任务(跑在服务端的才生效)
- 每次写脚本都是新笔记,库里会积累"程序笔记"与"内容笔记"混杂(需建 Scripts 目录隔离)

**代价**:学习曲线(JS + 属性系统 + API 文档);脚本失效的排障成本(安全模式/版本兼容)。

## 参考

- 官网 Scripting 章节:https://triliumnotes.org
- 源码 API:`src/services/backend_script_api.js`、`docs/frontend_api/FrontendScriptApi.html`
- v0.104 发布说明(安全默认值变更):https://github.com/TriliumNext/Trilium/releases/tag/v0.104.0
