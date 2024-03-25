
# VS Code配置settings.json

## settings.json

VS Code存在两种设置 settings.json 的方式：

- User Settings 用户设置：应用于该用户打开的所有工程
- Workspace Settings 工作区设置：工作区文件夹下创建一个名为 .vscode 的隐藏文件夹，文档位置为 根目录/.vscode/settings.json，设置仅适用于当前工作区，且覆盖用户设置。

打开方式：

- 使用 `Ctrl+`或点左下角齿轮图标菜单中“设置”，进入设置界面，找“在settings.json中编辑”打开
- 使用 `Ctrl+Shift+P`或者点击左下角齿轮图标，选择命令面板。然后输入 settings.json 来搜索
  - Open User Settings 会打开UI设置界面
  - Open Workspace Settings 也会打开UI设置界面
  - Open Settings (JSON) 会打开用户设置 settings.json 文件
  - Open Workspace Settings (JSON) 会打开工作区设置 settings.json 文件

## extensions.json

工作区文件夹.vscode 的文件夹

``` json
{
  // See http://go.microsoft.com/fwlink/?LinkId=827846
  // for the documentation about the extensions.json format
  "recommendations": [
    // Foam's own extension
    "foam.foam-vscode",

    // Tons of markdown goodies (lists, tables of content, so much more)
    "yzhang.markdown-all-in-one",

    // Prettier for auto formatting code
    "esbenp.prettier-vscode",

    // Understated grayscale theme (light and dark variants)
    "philipbe.theme-gray-matter"
  ]
}

```

## keybindings.json

使用 `Ctrl+Shift+P`,搜`keyboard shortcuts`

## 修改扩展配置

- 使用 `Ctrl+`或点左下角齿轮图标菜单中“设置”，进入设置界面，扩展类别有所有安装扩展
- 使用 `Ctrl+Shift+X`或扩展图标，找到安装的扩展，在卸载边上的齿轮图标有扩展设置的菜单



