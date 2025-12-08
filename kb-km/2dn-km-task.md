# 知识库任务配置

## windows定时



**创建脚本**

因为要隐藏任务执行时的命令窗口，所以用vb调用bat文件的形式。

run_task.vbs

``` vb
Set WshShell = CreateObject("WScript.Shell")
' 第 2 个参数 0 = 隐藏窗口，1 = 显示窗口（这里固定 0）
WshShell.Run "cmd /c ""D:\mycloud\task\task.cmd""", 0, False
```

创建批处理文件: `D:\mycloud\task\task.cmd`

``` bat
@echo off

:: 强制使用GBK编码（Windows CMD默认，彻底杜绝乱码）
chcp 936 >nul 2>&1

:: ===================== 基础配置（简化+绝对路径，避免解析错误） =====================
set "TASK_NAME=dzhtask"
set "LOG_FILE=D:\mycloud\task\tasklog.txt"
set "LOG_DIR=D:\mycloud\task"

:: ===================== 终极方案：用PowerShell命令获取标准时间（避免%date%格式差异） =====================
:: 调用PowerShell获取ISO格式时间（YYYY-MM-DD HH:mm:ss），批处理直接接收结果，无需解析
for /f "delims=" %%a in ('powershell -Command "(Get-Date).ToString('yyyy-MM-dd HH:mm:ss')"') do set "CURRENT_DATETIME=%%a"

:: ===================== 日志目录创建（容错处理） =====================
if not exist "%LOG_DIR%" (
    md "%LOG_DIR%" >nul 2>&1
    echo create dir: %LOG_DIR% >> "%LOG_FILE%"
)

:: ===================== 写入开始日志（语法严格，变量必用%包裹） =====================
echo ====================================================== >> "%LOG_FILE%"
echo task start  %CURRENT_DATETIME% >> "%LOG_FILE%"
echo ====================================================== >> "%LOG_FILE%"
echo. >> "%LOG_FILE%"

:: ===================== 你的实际任务逻辑（替换为自己的命令） =====================
:: 示例：执行程序/脚本，建议加上2>&1捕获错误输出到日志
:: "D:\Program Files\XXX.exe" >> "%LOG_FILE%" 2>&1

cd  /d  D:\mycloud\0pnbase\kb-main\
git pull
git add -A
git commit -m "commit"
git push >> "%LOG_FILE%" 2>&1
echo kb-main sync end >> "%LOG_FILE%"

:: ===================== 获取结束时间 =====================
for /f "delims=" %%a in ('powershell -Command "(Get-Date).ToString('yyyy-MM-dd HH:mm:ss')"') do set "CURRENT_DATETIME_END=%%a"

:: ===================== 写入结束日志 =====================
echo ====================================================== >> "%LOG_FILE%"
echo task  end - %CURRENT_DATETIME_END% >> "%LOG_FILE%"
echo ====================================================== >> "%LOG_FILE%"
echo. >> "%LOG_FILE%"

@echo on
```

**配置任务**

「任务计划程序」有 3 种快速打开方式，任选其一：
- 按下 Win + R 键，输入 taskschd.msc，回车直接打开；
- 控制面板 → 系统和安全 → 管理工具 → 任务计划程序；
- 开始菜单搜索「任务计划程序」，点击打开。

触发器

- 程序 / 脚本：wscript.exe（系统自带，无需安装）；
- 添加参数："D:\mycloud\task\run_task.vbs"（VBS 文件的完整路径，英文引号包裹）；
- 起始于：D:\mycloud\task（VBS 和 CMD 所在目录）。

原理：wscript.exe 本身是无窗口宿主，通过它启动 cmd.exe 并隐藏窗口，全程不会显示任何窗口（包括闪烁）。