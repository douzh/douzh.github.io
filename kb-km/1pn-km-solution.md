---
title: 知识库落地方案
date: 2018-01-01
type: kbase
tags: 知识管理/永久笔记
---
# 知识库落地方案

要求：

1. 工具开源、跨平台
2. 支持所见即所得标记语言
3. 支持类似Latex公式
4. 有反向链接支持

综合编辑器和扩展对比，优先vscode，次优emacs

## 编辑器对比

| 特色       | vscode               | emacs                    | vim      | 备注     |
| ---------- | -------------------- | ------------------------ | -------- | -------- |
| 交互体验   | 优                   | 良                       |          |          |
| 标识语言   | markdown             | org                      | markdown |          |
| Latex公式  | 支持实时预览         | 预览支持不太好           |          | 重点功能 |
| TAG        | 优                   | 优                       |          | 重点功能 |
| 多级TAG    | 支持                 | 不支持                   |          |          |
| 块引用     | `![[filename]]`    | 不支持                   |          | 重点功能 |
| backlink   | 支持Foam             | 支持Roam                 |          | 重点功能 |
| 链接图谱   | 良 foam              | 优 roam-ui               |          | 重点功能 |
| 链接方式   | 文件名               | 标题id属性               |          |          |
| 链接名     | 文件内一级标题名     | 自己定义                 |          |          |
| 文件内标题 | 用#分割的多级标题    | 每个标题都可设置ID       |          |          |
| 链接更新   | 文件名改变可自动更新 | ID不能变，变需要手工更新 |          | 重点功能 |
| 属性       | 不支持               | 优                       |          |          |
| TODO       | 差                   | 优                       |          |          |
| 扩展语言   | javascipt            | emacs-lisp               |          |          |
| 总结       | 最前最佳选择         | 次佳选择                 |          |          |

整体看，emacs的org标记语言比markdown更灵活，但emacs编辑器较vscode体验差些。

## VS Code方案

### 链接方案

路径链接形式：`[name](./doc/filename.md)`

- 文件名修改：支持，相关链接会自动修改
- 文件位置修改：支持，相关路径会自动修改
- 文件夹改名：不支持，但在placeholders侧边中会显示无效链接

wiki链接形式：`[[filename]]`

- 文件名修改：支持，相关链接会自动修改
- 文件位置修改：支持，本身与路径无关
- 文件夹改名：支持，本身与路径无关

wiki链接形式markdown默认不支持，只有VS的Foam可预览；路径链接形式为正常形式，但改文件夹不支持。

wiki链接的支持方案：

修改Foma配置：

```
"foam.edit.linkReferenceDefinitions": "withExtensions"
```

这会在文件底部自动生成对应的引用链接，由于wiki链接是双中括号，引用链接是用单括号解析，链接最终显示效果用被中括号包裹

### Tag支持

Foam通过 `YAML Front Matter`配置TAG,在YFM中配置的tags可以在TAG EXPLORER中查看。

```yaml
---
title: 知识库落地方案
date: 2018-01-01
type: kbase
tags: 永久笔记, 知识库
---
```

`tags` 属性定义这条笔记的标签。多个标签之间用空格或半角逗号分隔。另外也可以通过在笔记正文中使用 `#tag` 来添加标签。

Foam 支持多级标签即 `#tag/sub-tag`，但笔者并不使用，个人认为这还是在按树状结构来组织笔记。

### 扩展和配置

安装扩展

- Project Manager: 管理工作空间
- Markdown All in One：markdown扩展
- Office Viewer(Markdown Editor)：markdown所见所得扩展
- Foam：反向链接扩展

修改settings: [[1dn-ide-vscode-setting]]

- markdown编辑器默认用 Markdown All in One
- 图片保存位置修改 `assets/${fileName}-${now}.png`

```js
    "workbench.editorAssociations": {
        "*.md": "default"
    },
    "vscode-office.pasterImgPath": "assets/${fileName}-${now}.png"
```

如果使用vim扩展，windows需要设置一下关闭vim的Crtl相关快捷键，否则会和复制粘贴等快捷键冲突，添加如下配置：

```js
     "vim.useCtrlKeys": false
```

列模式使用vscode的`Shift+Alt+（拖动鼠标）或上下箭头`

## emacs方案

- 知识搜索：如何搜索和展示知识？使用agenda
  + 按标签组合搜索: C-r b t invest+stock+
  + 按属性值搜索: C-r b t id={dn.*}
  + 标签和属性综合搜索: C-r b t invest+stock+id={dn.*}
  + 按内容搜索
  + 按backlink搜索：使用org-roam相关功能 C-r b t
- 知识可示化：如何展示知识的网状结构？
  + 使用org-roam-ui相关功能
- ID命名: 参考文件全名加headline关键词，用“-”分隔
- TAG: 文件头按文件名规则添加FILETAGS, headline 添加专人TAG


