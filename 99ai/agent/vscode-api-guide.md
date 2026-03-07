# VS Code Agent 接口完整指南

本文档详细介绍 VS Code 提供给 AI Agent 调用的所有 API 接口，用于操作外部世界（文件系统、终端、进程等）。

---

## 一、概述

### 1.1 Agent 如何通过 VS Code 操作外部世界

```
┌─────────────────┐
│   AI Agent      │  ← 理解意图、生成计划
└────────┬────────┘
         │ (内部调用)
         ↓
┌─────────────────┐
│  VS Code API    │  ← 执行实际操作
└────────┬────────┘
         │ (系统调用)
         ↓
┌─────────────────┐
│   操作系统       │  ← 文件、进程、网络
└─────────────────┘
```

### 1.2 接口分类

| 类别 | 功能 | 示例 |
|------|------|------|
| **文件系统** | 读写、创建、删除文件 | `workspace.fs.readFile()` |
| **终端管理** | 执行命令、创建终端 | `window.createTerminal()` |
| **进程管理** | 任务执行、进程控制 | `tasks.executeTask()` |
| **代码分析** | AST 解析、符号查找 | `languages.getFoldingRanges()` |
| **调试工具** | 启动调试、断点管理 | `debug.startDebugging()` |
| **窗口 UI** | 显示消息、创建面板 | `window.showInformationMessage()` |
| **配置管理** | 读取/修改设置 | `workspace.getConfiguration()` |

---

## 二、文件系统接口

### 2.1 基础文件操作

#### **读取文件**

```typescript
import * as vscode from 'vscode';

/**
 * 读取文件内容
 * @param filePath 文件绝对路径
 */
async function readFile(filePath: string): Promise<string> {
    const uri = vscode.Uri.file(filePath);
    const bytes = await vscode.workspace.fs.readFile(uri);
    return new TextDecoder().decode(bytes);
}

// 使用示例
const content = await readFile('/path/to/file.txt');
console.log(content);
```

#### **写入文件**

```typescript
/**
 * 写入文件内容
 * @param filePath 文件绝对路径
 * @param content 文件内容
 */
async function writeFile(filePath: string, content: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const bytes = new TextEncoder().encode(content);
    await vscode.workspace.fs.writeFile(uri, bytes);
}

// 使用示例
await writeFile('/path/to/file.txt', 'Hello World');
```

#### **创建文件**

```typescript
/**
 * 创建空文件
 * @param filePath 文件绝对路径
 */
async function createFile(filePath: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    // 创建空文件
    await vscode.workspace.fs.writeFile(uri, new Uint8Array());
}

// 使用示例
await createFile('/path/to/new-file.txt');
```

#### **删除文件**

```typescript
/**
 * 删除文件
 * @param filePath 文件绝对路径
 * @param recursive 是否递归删除（用于目录）
 */
async function deleteFile(filePath: string, recursive: boolean = false): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    await vscode.workspace.fs.delete(uri, { recursive });
}

// 使用示例
await deleteFile('/path/to/file.txt');
await deleteFile('/path/to/folder', true); // 递归删除目录
```

#### **复制/移动文件**

```typescript
/**
 * 复制文件
 * @param source 源路径
 * @param destination 目标路径
 */
async function copyFile(source: string, destination: string): Promise<void> {
    const sourceUri = vscode.Uri.file(source);
    const destUri = vscode.Uri.file(destination);
    await vscode.workspace.fs.copy(sourceUri, destUri);
}

/**
 * 移动文件（重命名）
 * @param source 源路径
 * @param destination 目标路径
 */
async function moveFile(source: string, destination: string): Promise<void> {
    const sourceUri = vscode.Uri.file(source);
    const destUri = vscode.Uri.file(destination);
    await vscode.workspace.fs.rename(sourceUri, destUri);
}

// 使用示例
await copyFile('/src/file.txt', '/backup/file.txt');
await moveFile('/old-name.txt', '/new-name.txt');
```

---

### 2.2 目录操作

#### **列出目录内容**

```typescript
/**
 * 列出目录内容
 * @param dirPath 目录绝对路径
 * @returns 文件和子目录列表
 */
async function listDirectory(dirPath: string): Promise<{name: string, type: FileType}[]> {
    const uri = vscode.Uri.file(dirPath);
    const entries = await vscode.workspace.fs.readDirectory(uri);
    
    return entries.map(([name, type]) => ({
        name,
        type: getFileName(type)
    }));
}

enum FileType {
    Unknown = 0,
    File = 1,
    Directory = 2,
    SymbolicLink = 64
}

function getFileName(type: FileType): string {
    switch (type) {
        case FileType.File: return 'file';
        case FileType.Directory: return 'directory';
        case FileType.SymbolicLink: return 'symlink';
        default: return 'unknown';
    }
}

// 使用示例
const files = await listDirectory('/path/to/project');
console.log(files);
// [
//   { name: 'src', type: 'directory' },
//   { name: 'README.md', type: 'file' },
//   { name: 'package.json', type: 'file' }
// ]
```

#### **创建目录**

```typescript
/**
 * 创建目录（包括父目录）
 * @param dirPath 目录绝对路径
 */
async function createDirectory(dirPath: string): Promise<void> {
    const uri = vscode.Uri.file(dirPath);
    await vscode.workspace.fs.createDirectory(uri);
}

// 使用示例
await createDirectory('/path/to/nested/directory');
```

---

### 2.3 文件信息

#### **获取文件元数据**

```typescript
/**
 * 获取文件元数据
 * @param filePath 文件绝对路径
 */
async function getFileStats(filePath: string): Promise<FileStat> {
    const uri = vscode.Uri.file(filePath);
    const stat = await vscode.workspace.fs.stat(uri);
    
    return {
        type: stat.type,
        createTime: stat.ctime, // 创建时间（毫秒时间戳）
        modifyTime: stat.mtime, // 修改时间（毫秒时间戳）
        size: stat.size         // 文件大小（字节）
    };
}

interface FileStat {
    type: number;
    createTime: number;
    modifyTime: number;
    size: number;
}

// 使用示例
const stats = await getFileStats('/path/to/file.txt');
console.log(`文件大小：${stats.size} 字节`);
console.log(`最后修改：${new Date(stats.modifyTime)}`);
```

#### **检查文件是否存在**

```typescript
/**
 * 检查文件或目录是否存在
 * @param path 文件或目录路径
 */
async function exists(path: string): Promise<boolean> {
    try {
        const uri = vscode.Uri.file(path);
        await vscode.workspace.fs.stat(uri);
        return true;
    } catch (error) {
        return false;
    }
}

// 使用示例
if (await exists('/path/to/file.txt')) {
    console.log('文件存在');
} else {
    console.log('文件不存在');
}
```

---

### 2.4 文本编辑操作

#### **在编辑器中打开文件**

```typescript
/**
 * 在编辑器中打开文件
 * @param filePath 文件路径
 * @param viewColumn 打开位置（默认当前列）
 */
async function openFile(
    filePath: string, 
    viewColumn: vscode.ViewColumn = vscode.ViewColumn.Active
): Promise<vscode.TextEditor> {
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document, viewColumn);
    return editor;
}

// 使用示例
const editor = await openFile('/path/to/file.js');
```

#### **编辑文件内容**

```typescript
/**
 * 编辑文件内容
 * @param filePath 文件路径
 * @param edits 编辑操作列表
 */
async function editFile(
    filePath: string, 
    edits: Array<{range: [number, number, number, number], newText: string}>
): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document);
    
    // 执行编辑
    await editor.edit(editBuilder => {
        edits.forEach(edit => {
            const range = new vscode.Range(...edit.range);
            editBuilder.replace(range, edit.newText);
        });
    });
}

// 使用示例
await editFile('/path/to/file.js', [
    {
        range: [0, 0, 0, 5], // 第 1 行，第 1-5 列
        newText: 'function'
    }
]);
```

#### **插入文本**

```typescript
/**
 * 在指定位置插入文本
 * @param filePath 文件路径
 * @param position 插入位置 [line, character]
 * @param text 要插入的文本
 */
async function insertText(
    filePath: string, 
    position: [number, number], 
    text: string
): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document);
    
    const pos = new vscode.Position(position[0], position[1]);
    await editor.edit(editBuilder => {
        editBuilder.insert(pos, text);
    });
}

// 使用示例
await insertText('/path/to/file.js', [0, 0], '// 文件头注释\n');
```

---

## 三、终端和命令执行接口

### 3.1 创建和管理终端

#### **创建终端**

```typescript
import * as vscode from 'vscode';

/**
 * 创建新终端
 * @param options 终端配置选项
 */
function createTerminal(options?: {
    name?: string;           // 终端名称
    shellPath?: string;      // Shell 路径
    shellArgs?: string[];    // Shell 参数
    cwd?: string;            // 工作目录
    env?: {[key: string]: string}; // 环境变量
}): vscode.Terminal {
    return vscode.window.createTerminal(options);
}

// 使用示例
const terminal = createTerminal({
    name: 'My Terminal',
    cwd: '/path/to/project'
});
terminal.show();
```

#### **发送命令到终端**

```typescript
/**
 * 发送命令到终端
 * @param terminal 终端实例
 * @param command 要执行的命令
 * @param shouldEcho 是否在终端显示命令
 */
function sendCommand(terminal: vscode.Terminal, command: string, shouldEcho: boolean = true): void {
    terminal.sendText(command, shouldEcho);
}

// 使用示例
sendCommand(terminal, 'npm install');
sendCommand(terminal, 'echo "Hello"', false); // 不显示命令本身
```

#### **执行命令并获取输出**

```typescript
/**
 * 执行命令并获取输出（简化版）
 * @param command 要执行的命令
 * @param cwd 工作目录
 * @param timeout 超时时间（毫秒）
 */
async function executeCommand(
    command: string, 
    cwd?: string, 
    timeout: number = 30000
): Promise<{stdout: string, stderr: string, code: number}> {
    return new Promise((resolve, reject) => {
        const terminal = vscode.window.createTerminal({
            name: 'Command Runner',
            cwd
        });
        
        let output = '';
        let errorOutput = '';
        
        // 监听终端输出
        const disposable = vscode.window.onDidWriteTerminalData(e => {
            if (e.terminal === terminal) {
                output += e.data;
            }
        });
        
        // 发送命令
        terminal.show();
        terminal.sendText(command, true);
        
        // 超时处理
        setTimeout(() => {
            terminal.dispose();
            disposable.dispose();
            
            resolve({
                stdout: output,
                stderr: errorOutput,
                code: 0 // 实际实现需要监听退出码
            });
        }, timeout);
    });
}

// 使用示例
const result = await executeCommand('git status', '/path/to/repo');
console.log(result.stdout);
```

---

### 3.2 任务执行接口

#### **VS Code 任务概念说明**

**重要：任务（Task）≠ 定时任务**

VS Code 的**Task（任务）**是指**预定义的、配置化的一次性命令**，不是定时任务。

**核心特点：**
- ✅ 在 `tasks.json` 中预先配置的命令模板
- ✅ 用于构建、测试、清理、部署等重复性工作
- ✅ 支持事件监听（开始、结束、进程状态）
- ✅ 可以用快捷键触发
- ❌ **不是**按时间自动执行的定时任务（Cron Job）

**典型用途：**
```typescript
// 常见任务类型
- 构建项目：mvn clean install, npm run build
- 运行测试：npm test, mvn test
- 启动服务：node server.js, npm run dev
- 代码检查：eslint ., tsc --noEmit
- 清理工作：rm -rf dist, npm run clean
- 部署发布：git push && npm publish
```

---

#### **执行预定义任务

#### **执行预定义任务**

```typescript
/**
 * 执行 VS Code 任务
 * @param taskDefinition 任务定义
 */
async function executeTask(taskDefinition: {
    type: string;
    label: string;
    command: string;
    args?: string[];
    options?: {
        cwd?: string;
        env?: {[key: string]: string};
    };
}): Promise<void> {
    const task = new vscode.Task(
        { type: taskDefinition.type },
        vscode.TaskScope.Workspace,
        taskDefinition.label,
        'Custom',
        new vscode.ShellExecution(taskDefinition.command, taskDefinition.args || [], taskDefinition.options)
    );
    
    await vscode.tasks.executeTask(task);
}

// 使用示例
await executeTask({
    type: 'shell',
    label: 'Build Project',
    command: 'mvn',
    args: ['clean', 'install'],
    options: {
        cwd: '/path/to/project'
    }
});
```

#### **监听任务事件**

```typescript
/**
 * 监听任务执行事件
 */
function watchTaskEvents(): vscode.Disposable {
    // 任务开始
    const onStart = vscode.tasks.onDidStartTask(e => {
        console.log('Task started:', e.execution.task.name);
    });
    
    // 任务结束
    const onEnd = vscode.tasks.onDidEndTask(e => {
        console.log('Task ended:', e.execution.task.name);
    });
    
    // 任务进程开始
    const onProcessStart = vscode.tasks.onDidStartTaskProcess(e => {
        console.log('Process started:', e.processId);
    });
    
    // 任务进程结束
    const onProcessEnd = vscode.tasks.onDidEndTaskProcess(e => {
        console.log('Process ended:', e.exitCode);
    });
    
    return {
        dispose: () => {
            onStart.dispose();
            onEnd.dispose();
            onProcessStart.dispose();
            onProcessEnd.dispose();
        }
    };
}

// 使用示例
const watcher = watchTaskEvents();
// ... 稍后调用 watcher.dispose() 停止监听
```

---

#### **任务 vs 终端命令的区别**

很多人会混淆“任务”和“终端命令”，它们的区别：

| 特性 | **任务（Task）** | **终端命令（Terminal）** |
|------|-----------------|------------------------|
| **定义方式** | 在 `tasks.json` 中预定义 | 直接发送字符串到终端 |
| **可复用性** | ✅ 高，配置一次多次使用 | ❌ 低，每次都要写命令 |
| **事件监听** | ✅ 完善的事件系统 | ❌ 需要自己解析输出 |
| **问题匹配** | ✅ 自动识别错误并显示 | ❌ 需要手动处理 |
| **快捷键** | ✅ 可以用快捷键触发 | ❌ 不行 |
| **后台任务** | ✅ 支持后台运行 | ⚠️ 需要特殊处理 |
| **灵活性** | ⚠️ 较低，需要预定义 | ✅ 非常高，任意命令 |

---

#### **tasks.json 配置文件示例**

一个典型的 `tasks.json` 配置文件：

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Maven Build",
      "type": "shell",
      "command": "mvn",
      "args": [
        "clean",
        "package",
        "-DskipTests"
      ],
      "group": {
        "kind": "build",
        "isDefault": true
      },
      "problemMatcher": ["$maven"],
      "detail": "使用 Maven 构建项目"
    },
    {
      "label": "Run Unit Tests",
      "type": "shell",
      "command": "mvn",
      "args": ["test"],
      "group": "test",
      "problemMatcher": ["$maven"]
    },
    {
      "label": "Start Dev Server",
      "type": "shell",
      "command": "npm",
      "args": ["run", "dev"],
      "isBackground": true,
      "problemMatcher": {
        "pattern": {
          "regexp": "^.*Server started.*$",
          "file": 1,
          "location": 2,
          "message": 3
        },
        "background": {
          "activeOnStart": true,
          "beginsPattern": "Server starting",
          "endsPattern": "Server started"
        }
      }
    },
    {
      "label": "Deploy to Production",
      "type": "shell",
      "command": "./deploy.sh",
      "args": ["prod"],
      "group": "deploy",
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    }
  ]
}
```

**配置说明：**
- `label`: 任务名称，用于识别
- `type`: 任务类型（shell、process）
- `command`: 要执行的命令
- `args`: 命令参数
- `group`: 任务分组（build、test、deploy 等）
- `problemMatcher`: 问题匹配器，自动识别编译错误
- `isBackground`: 是否为后台任务
- `presentation`: 控制终端显示方式

---

#### **实际应用示例**

##### **示例 1：一键构建和测试（Agent 自动化工作流）**

```typescript
import * as vscode from 'vscode';

/**
 * Agent 自动化工作流：构建并测试
 */
async function buildAndTest(): Promise<void> {
    try {
        // 步骤 1: 执行构建任务
        vscode.window.showInformationMessage('开始构建项目...');
        
        const buildTask = await getTaskByLabel('Maven Build');
        if (!buildTask) {
            throw new Error('未找到构建任务');
        }
        
        await vscode.tasks.executeTask(buildTask);
        
        // 等待构建完成（通过事件监听）
        const buildSuccess = await waitForTaskCompletion();
        
        if (!buildSuccess) {
            vscode.window.showErrorMessage('构建失败，停止后续操作');
            return;
        }
        
        // 步骤 2: 执行测试任务
        vscode.window.showInformationMessage('开始运行测试...');
        
        const testTask = await getTaskByLabel('Run Unit Tests');
        await vscode.tasks.executeTask(testTask);
        
        // 等待测试完成
        const testSuccess = await waitForTaskCompletion();
        
        if (testSuccess) {
            vscode.window.showInformationMessage('✅ 构建和测试完成!');
        } else {
            vscode.window.showWarningMessage('⚠️ 构建成功，但测试失败');
        }
        
    } catch (error) {
        vscode.window.showErrorMessage(`❌ 失败：${error}`);
    }
}

/**
 * 辅助函数：根据名称查找任务
 */
async function getTaskByLabel(label: string): Promise<vscode.Task | undefined> {
    const tasks = await vscode.tasks.fetchTasks();
    return tasks.find(t => t.name === label);
}

/**
 * 辅助函数：等待任务完成
 */
function waitForTaskCompletion(): Promise<boolean> {
    return new Promise((resolve) => {
        const disposable = vscode.tasks.onDidEndTaskProcess(e => {
            disposable.dispose();
            resolve(e.exitCode === 0);
        });
        
        // 超时处理（5 分钟）
        setTimeout(() => {
            disposable.dispose();
            resolve(false);
        }, 300000);
    });
}
```

---

##### **示例 2：智能重试机制**

```typescript
/**
 * 带重试的任务执行
 */
async function executeWithRetry(
    taskName: string, 
    maxRetries: number = 3
): Promise<boolean> {
    
    for (let i = 0; i < maxRetries; i++) {
        try {
            const task = await getTaskByLabel(taskName);
            if (!task) {
                throw new Error(`未找到任务：${taskName}`);
            }
            
            vscode.window.showInformationMessage(
                `正在执行任务（第 ${i + 1}/${maxRetries} 次尝试）...`
            );
            
            await vscode.tasks.executeTask(task);
            
            // 等待并检查结果
            const success = await waitForTaskSuccess();
            
            if (success) {
                vscode.window.showInformationMessage(`✅ 任务成功完成！`);
                return true;
            }
            
            // 失败，准备重试
            vscode.window.showWarningMessage(
                `任务失败，${i < maxRetries - 1 ? '准备重试...' : '已达到最大重试次数'}`
            );
            
            // 等待 2 秒后重试
            await delay(2000);
            
        } catch (error) {
            if (i === maxRetries - 1) {
                vscode.window.showErrorMessage(
                    `任务执行失败，已重试 ${maxRetries} 次：${error}`
                );
                throw error;
            }
        }
    }
    
    return false;
}

// 辅助函数
function delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function waitForTaskSuccess(): Promise<boolean> {
    return new Promise((resolve) => {
        const disposable = vscode.tasks.onDidEndTaskProcess(e => {
            disposable.dispose();
            resolve(e.exitCode === 0);
        });
    });
}
```

---

##### **示例 3：并行执行多个任务**

```typescript
/**
 * 并行执行多个独立任务
 */
async function executeParallelTasks(taskNames: string[]): Promise<void> {
    const results = await Promise.allSettled(
        taskNames.map(async name => {
            const task = await getTaskByLabel(name);
            if (!task) {
                throw new Error(`未找到任务：${name}`);
            }
            
            vscode.window.showInformationMessage(`开始执行：${name}`);
            await vscode.tasks.executeTask(task);
            
            // 等待该任务完成
            return new Promise<string>((resolve, reject) => {
                const disposable = vscode.tasks.onDidEndTaskProcess(e => {
                    disposable.dispose();
                    if (e.exitCode === 0) {
                        resolve(name);
                    } else {
                        reject(new Error(`${name} 失败，退出码：${e.exitCode}`));
                    }
                });
            });
        })
    );
    
    // 统计结果
    const succeeded = results.filter(r => r.status === 'fulfilled').length;
    const failed = results.filter(r => r.status === 'rejected').length;
    
    if (failed === 0) {
        vscode.window.showInformationMessage(
            `✅ 所有任务完成！成功：${succeeded}, 失败：${failed}`
        );
    } else {
        vscode.window.showWarningMessage(
            `⚠️ 部分任务完成。成功：${succeeded}, 失败：${failed}`
        );
    }
}

// 使用示例
executeParallelTasks([
    'Build Frontend',
    'Build Backend',
    'Run Linter'
]);
```

---

##### **示例 4：动态创建并执行临时任务**

```typescript
/**
 * 动态创建临时任务（不需要在 tasks.json 中预定义）
 */
async function executeDynamicTask(
    command: string,
    args: string[],
    cwd: string,
    taskName: string = 'Dynamic Task'
): Promise<number> {
    return new Promise((resolve) => {
        // 创建 ShellExecution
        const execution = new vscode.ShellExecution(command, args, {
            cwd,
            env: {}
        });
        
        // 创建任务
        const task = new vscode.Task(
            { type: 'shell' },           // 任务类型
            vscode.TaskScope.Workspace,  // 作用域
            taskName,                    // 任务名称
            'dynamic',                   // 来源
            execution,                   // 执行配置
            ['$msCompile']               // 问题匹配器
        );
        
        // 执行并监听
        vscode.tasks.executeTask(task).then(execution => {
            const disposable = vscode.tasks.onDidEndTaskProcess(e => {
                if (e.execution === execution) {
                    disposable.dispose();
                    resolve(e.exitCode);
                }
            });
        });
    });
}

// 使用示例
const exitCode = await executeDynamicTask(
    'npm',
    ['run', 'build'],
    '/path/to/project',
    '临时构建任务'
);

if (exitCode === 0) {
    console.log('动态任务执行成功');
} else {
    console.log(`动态任务失败，退出码：${exitCode}`);
}
```

---

### 3.3 Shell 执行

#### **Shell Execution API**

```typescript
/**
 * 使用 ShellExecution 执行命令
 */
function shellExecute(command: string, options?: {
    args?: string[];
    cwd?: string;
    env?: {[key: string]: string};
}): vscode.ShellExecution {
    return new vscode.ShellExecution(
        command,
        options?.args || [],
        {
            cwd: options?.cwd,
            env: options?.env
        }
    );
}

// 使用示例
const exec = shellExecute('npm', {
    args: ['run', 'build'],
    cwd: '/path/to/project'
});
```

---

## 四、代码分析和导航接口

### 4.1 符号和定义

#### **查找符号定义**

```typescript
import * as vscode from 'vscode';

/**
 * 查找符号的定义位置
 * @param symbolName 符号名称
 * @param filePath 文件路径
 * @param position 符号位置
 */
async function findDefinition(
    filePath: string, 
    position: vscode.Position
): Promise<vscode.Location | null> {
    const uri = vscode.Uri.file(filePath);
    const definition = await vscode.commands.executeCommand<vscode.Location[]>(
        'vscode.executeDefinitionProvider',
        uri,
        position
    );
    
    return definition && definition.length > 0 ? definition[0] : null;
}

// 使用示例
const definition = await findDefinition('/path/to/file.ts', new vscode.Position(10, 5));
if (definition) {
    console.log('定义位置:', definition.uri.fsPath, definition.range.start);
}
```

#### **查找引用**

```typescript
/**
 * 查找符号的所有引用
 * @param filePath 文件路径
 * @param position 符号位置
 */
async function findReferences(
    filePath: string, 
    position: vscode.Position
): Promise<vscode.Location[]> {
    const uri = vscode.Uri.file(filePath);
    const references = await vscode.commands.executeCommand<vscode.Location[]>(
        'vscode.executeReferenceProvider',
        uri,
        position
    );
    
    return references || [];
}

// 使用示例
const refs = await findReferences('/path/to/file.ts', new vscode.Position(10, 5));
refs.forEach(ref => {
    console.log(`引用：${ref.uri.fsPath}:${ref.range.start.line}`);
});
```

---

### 4.2 文档符号

#### **获取文档符号**

```typescript
/**
 * 获取文档中的所有符号
 * @param filePath 文件路径
 */
async function getDocumentSymbols(filePath: string): Promise<vscode.DocumentSymbol[]> {
    const uri = vscode.Uri.file(filePath);
    const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
        'vscode.executeDocumentSymbolProvider',
        uri
    );
    
    return symbols || [];
}

// 使用示例
const symbols = await getDocumentSymbols('/path/to/file.ts');
symbols.forEach(symbol => {
    console.log(`${symbol.name} (${symbol.kind}): ${symbol.range.start.line}`);
});
```

#### **获取折叠范围**

```typescript
/**
 * 获取文档的折叠范围
 * @param filePath 文件路径
 */
async function getFoldingRanges(filePath: string): Promise<vscode.FoldingRange[]> {
    const uri = vscode.Uri.file(filePath);
    const ranges = await vscode.commands.executeCommand<vscode.FoldingRange[]>(
        'vscode.executeFoldingRangeProvider',
        uri
    );
    
    return ranges || [];
}

// 使用示例
const ranges = await getFoldingRanges('/path/to/file.ts');
ranges.forEach(range => {
    console.log(`折叠：${range.start}-${range.end}`);
});
```

---

### 4.3 代码格式化

#### **格式化文档**

```typescript
/**
 * 格式化整个文档
 * @param filePath 文件路径
 */
async function formatDocument(filePath: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document);
    
    await vscode.commands.executeCommand('editor.action.formatDocument');
}

// 使用示例
await formatDocument('/path/to/file.ts');
```

#### **格式化选区**

```typescript
/**
 * 格式化指定选区
 * @param filePath 文件路径
 * @param range 选区范围
 */
async function formatRange(filePath: string, range: vscode.Range): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const document = await vscode.workspace.openTextDocument(uri);
    const editor = await vscode.window.showTextDocument(document);
    
    await editor.edit(editBuilder => {
        const edits = await vscode.commands.executeCommand<vscode.TextEdit[]>(
            'vscode.executeFormatRangeProvider',
            uri,
            range
        );
        
        if (edits) {
            edits.forEach(edit => {
                editBuilder.replace(edit.range, edit.newText);
            });
        }
    });
}

// 使用示例
const range = new vscode.Range(0, 0, 10, 0);
await formatRange('/path/to/file.ts', range);
```

---

## 五、调试接口

### 5.1 启动调试

```typescript
import * as vscode from 'vscode';

/**
 * 启动调试会话
 * @param configuration 调试配置
 */
async function startDebugging(configuration: {
    type: string;      // 调试器类型，如 'node', 'java'
    request: string;   // 请求类型，如 'launch', 'attach'
    name: string;      // 配置名称
    program?: string;  // 程序路径
    cwd?: string;      // 工作目录
    args?: any[];      // 参数
    env?: {[key: string]: string}; // 环境变量
}): Promise<boolean> {
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    
    const started = await vscode.debug.startDebugging(
        workspaceFolder,
        configuration
    );
    
    return started;
}

// 使用示例 - 启动 Node.js 调试
await startDebugging({
    type: 'node',
    request: 'launch',
    name: 'Debug App',
    program: '${workspaceFolder}/app.js',
    cwd: '${workspaceFolder}',
    args: ['--port', '3000']
});

// 使用示例 - 启动 Java 调试
await startDebugging({
    type: 'java',
    request: 'launch',
    name: 'Debug Java App',
    mainClass: 'com.example.App',
    projectName: 'my-project'
});
```

---

### 5.2 断点管理

#### **添加断点**

```typescript
/**
 * 添加条件断点
 * @param filePath 文件路径
 * @param line 行号
 * @param condition 断点条件（可选）
 */
async function addBreakpoint(
    filePath: string, 
    line: number, 
    condition?: string
): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    
    const breakpoint = new vscode.SourceBreakpoint(
        new vscode.Location(uri, new vscode.Position(line, 0)),
        condition ? true : false, // enabled
        condition,                // condition
        false                     // hitCondition
    );
    
    vscode.debug.addBreakpoints([breakpoint]);
}

// 使用示例
await addBreakpoint('/path/to/file.js', 25);
await addBreakpoint('/path/to/file.js', 50, 'x > 10'); // 条件断点
```

#### **移除断点**

```typescript
/**
 * 移除所有断点
 */
function removeAllBreakpoints(): void {
    const breakpoints = vscode.debug.breakpoints;
    vscode.debug.removeBreakpoints(breakpoints);
}

/**
 * 移除指定文件的断点
 * @param filePath 文件路径
 */
function removeBreakpointsInFile(filePath: string): void {
    const uri = vscode.Uri.file(filePath);
    const breakpointsToRemove = vscode.debug.breakpoints.filter(bp => {
        return bp instanceof vscode.SourceBreakpoint && 
               bp.location.uri.fsPath === uri.fsPath;
    });
    
    vscode.debug.removeBreakpoints(breakpointsToRemove);
}

// 使用示例
removeAllBreakpoints();
removeBreakpointsInFile('/path/to/file.js');
```

---

### 5.3 调试会话监听

```typescript
/**
 * 监听调试会话事件
 */
function watchDebugSessions(): vscode.Disposable {
    // 调试会话开始
    const onStart = vscode.debug.onDidStartDebugSession(session => {
        console.log('Debug session started:', session.name);
        console.log('Type:', session.configuration.type);
    });
    
    // 调试会话结束
    const onTerminate = vscode.debug.onDidTerminateDebugSession(session => {
        console.log('Debug session terminated:', session.name);
    });
    
    // 接收到调试自定义事件
    const onCustomEvent = vscode.debug.onDidReceiveDebugSessionCustomEvent(event => {
        console.log('Debug custom event:', event.event, event.body);
    });
    
    return {
        dispose: () => {
            onStart.dispose();
            onTerminate.dispose();
            onCustomEvent.dispose();
        }
    };
}

// 使用示例
const debugWatcher = watchDebugSessions();
```

---

## 六、窗口和用户界面接口

### 6.1 消息显示

#### **显示信息消息**

```typescript
import * as vscode from 'vscode';

/**
 * 显示信息消息
 * @param message 消息内容
 * @param items 可选的操作按钮
 */
async function showInfoMessage<T extends string>(
    message: string, 
    ...items: T[]
): Promise<T | undefined> {
    return await vscode.window.showInformationMessage(message, ...items);
}

// 使用示例
const result = await showInfoMessage(
    '任务已完成！',
    '查看结果',
    '关闭'
);

if (result === '查看结果') {
    // 用户点击了"查看结果"
    vscode.commands.executeCommand('workbench.view.explorer');
}
```

#### **显示警告消息**

```typescript
/**
 * 显示警告消息
 * @param message 消息内容
 * @param items 可选的操作按钮
 */
async function showWarningMessage<T extends string>(
    message: string, 
    ...items: T[]
): Promise<T | undefined> {
    return await vscode.window.showWarningMessage(message, ...items);
}

// 使用示例
await showWarningMessage('此操作不可撤销，确定要继续吗？', '确定', '取消');
```

#### **显示错误消息**

```typescript
/**
 * 显示错误消息
 * @param message 消息内容
 */
async function showErrorMessage(message: string): Promise<void> {
    await vscode.window.showErrorMessage(message);
}

// 使用示例
await showErrorMessage('操作失败：文件不存在');
```

---

### 6.2 输入框

#### **显示输入框**

```typescript
/**
 * 显示输入框
 * @param options 输入框配置
 */
async function showInputBox(options?: {
    prompt?: string;      // 提示文本
    placeHolder?: string; // 占位符
    value?: string;       // 默认值
    password?: boolean;   // 是否为密码模式
    validateInput?(value: string): string | undefined | null; // 验证函数
}): Promise<string | undefined> {
    return await vscode.window.showInputBox(options);
}

// 使用示例
const fileName = await showInputBox({
    prompt: '请输入文件名',
    placeHolder: 'example.txt',
    validateInput: (value) => {
        if (!value) {
            return '文件名不能为空';
        }
        if (value.includes('/')) {
            return '文件名不能包含斜杠';
        }
        return undefined;
    }
});

if (fileName) {
    console.log('用户输入的文件名:', fileName);
}
```

---

### 6.3 快速选择

#### **显示快速选择菜单**

```typescript
/**
 * 显示快速选择菜单
 * @param items 选项列表
 * @param options 配置选项
 */
async function showQuickPick<T extends vscode.QuickPickItem>(
    items: T[] | Thenable<T[]>,
    options?: {
        placeHolder?: string;
        matchOnDescription?: boolean;
        matchOnDetail?: boolean;
        canPickMany?: boolean;
    }
): Promise<T | T[] | undefined> {
    return await vscode.window.showQuickPick(items, options);
}

// 使用示例
const selected = await showQuickPick([
    { label: '选项 1', description: '描述 1' },
    { label: '选项 2', description: '描述 2' },
    { label: '选项 3', description: '描述 3' }
], {
    placeHolder: '请选择一个选项'
});

if (selected) {
    console.log('用户选择了:', selected.label);
}
```

---

### 6.4 Webview 面板

#### **创建 Webview 面板**

```typescript
/**
 * 创建 Webview 面板
 * @param id 面板 ID
 * @param title 面板标题
 * @param viewColumn 显示位置
 */
function createWebviewPanel(
    id: string,
    title: string,
    viewColumn: vscode.ViewColumn = vscode.ViewColumn.One
): vscode.WebviewPanel {
    const panel = vscode.window.createWebviewPanel(
        id,
        title,
        viewColumn,
        {
            enableScripts: true, // 启用 JavaScript
            retainContextWhenHidden: true // 隐藏时保留上下文
        }
    );
    
    return panel;
}

// 使用示例
const panel = createWebviewPanel('myPanel', '我的面板');
panel.webview.html = `
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>我的面板</title>
</head>
<body>
    <h1>Hello World!</h1>
    <button id="clickBtn">点击我</button>
    <script>
        document.getElementById('clickBtn').addEventListener('click', () => {
            // 发送消息到扩展
        });
    </script>
</body>
</html>
`;
```

---

## 七、配置和工作区接口

### 7.1 读取配置

```typescript
import * as vscode from 'vscode';

/**
 * 读取工作区配置
 * @param section 配置节名称
 * @param resource 资源 URI（可选）
 */
function getConfiguration<T>(
    section: string,
    resource?: vscode.Uri
): T | undefined {
    const config = vscode.workspace.getConfiguration(undefined, resource);
    return config.get<T>(section);
}

// 使用示例
const editorTabSize = getConfiguration<number>('editor.tabSize');
const filesExclude = getConfiguration<{[key: string]: boolean}>('files.exclude');
```

---

### 7.2 修改配置

```typescript
/**
 * 修改工作区配置
 * @param section 配置节名称
 * @param value 新值
 * @param configurationTarget 配置目标
 */
async function updateConfiguration(
    section: string,
    value: any,
    configurationTarget: vscode.ConfigurationTarget = vscode.ConfigurationTarget.Workspace
): Promise<void> {
    const config = vscode.workspace.getConfiguration();
    await config.update(section, value, configurationTarget);
}

// 使用示例
await updateConfiguration('editor.tabSize', 2);
await updateConfiguration('files.exclude', {'*.log': true});
```

---

### 7.3 工作区文件夹

```typescript
/**
 * 获取工作区文件夹列表
 */
function getWorkspaceFolders(): vscode.WorkspaceFolder[] {
    return vscode.workspace.workspaceFolders || [];
}

/**
 * 添加文件夹到工作区
 * @param folderPath 文件夹路径
 */
async function addWorkspaceFolder(folderPath: string): Promise<void> {
    const uri = vscode.Uri.file(folderPath);
    const index = vscode.workspace.workspaceFolders?.length || 0;
    
    vscode.workspace.updateWorkspaceFolders(index, 0, { uri });
}

/**
 * 从工作区移除文件夹
 * @param folder 工作区文件夹对象
 */
async function removeWorkspaceFolder(folder: vscode.WorkspaceFolder): Promise<void> {
    vscode.workspace.updateWorkspaceFolders(folder.index, 1);
}

// 使用示例
const folders = getWorkspaceFolders();
console.log('工作区文件夹:', folders.map(f => f.name));

await addWorkspaceFolder('/path/to/new-folder');
```

---

## 八、命令执行接口

### 8.1 执行内置命令

```typescript
/**
 * 执行 VS Code 命令
 * @param command 命令 ID
 * @param args 命令参数
 */
async function executeCommand<T>(
    command: string,
    ...args: any[]
): Promise<T | undefined> {
    return await vscode.commands.executeCommand<T>(command, ...args);
}

// 常用命令示例

// 打开侧边栏
await executeCommand('workbench.view.explorer');

// 打开命令面板
await executeCommand('workbench.action.showCommands');

// 格式化文档
await executeCommand('editor.action.formatDocument');

// 保存文件
await executeCommand('workbench.action.files.save');

// 查找文件
await executeCommand('workbench.action.quickOpen');

// 切换终端
await executeCommand('workbench.action.togglePanel');

// 运行测试
await executeCommand('workbench.action.tasks.runTask');

// 重启扩展
await executeCommand('workbench.action.reloadWindow');
```

---

### 8.2 注册自定义命令

```typescript
/**
 * 注册自定义命令
 * @param command 命令 ID
 * @param callback 回调函数
 */
function registerCommand(
    command: string,
    callback: (...args: any[]) => any
): vscode.Disposable {
    return vscode.commands.registerCommand(command, callback);
}

// 使用示例
const disposable = registerCommand('myExtension.sayHello', () => {
    vscode.window.showInformationMessage('Hello from my extension!');
});

// 稍后在激活函数中注册
export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(disposable);
}
```

---

## 九、安全和权限

### 9.1 安全检查

```typescript
class SecurityChecker {
    /**
     * 验证文件路径是否安全
     */
    static validatePath(path: string): boolean {
        // 1. 防止路径遍历攻击
        if (path.includes('../') || path.includes('..\\')) {
            return false;
        }
        
        // 2. 检查工作区边界
        const workspaceRoot = vscode.workspace.rootPath;
        if (workspaceRoot && !path.startsWith(workspaceRoot)) {
            return false;
        }
        
        // 3. 检查敏感文件
        const sensitiveFiles = ['.env', '.git/config', 'id_rsa'];
        if (sensitiveFiles.some(f => path.endsWith(f))) {
            return false;
        }
        
        return true;
    }
    
    /**
     * 验证命令是否安全
     */
    static validateCommand(command: string): boolean {
        const blockedPatterns = [
            /rm\s+-rf\s+\//,
            /del\s+\/f\s+\/s\s+\/q\s+C:\\/,
            /format\s+[cCdD]:/,
            /dd\s+if=\/dev\/zero/
        ];
        
        return !blockedPatterns.some(pattern => pattern.test(command));
    }
}

// 使用示例
if (SecurityChecker.validatePath('/safe/path')) {
    await readFile('/safe/path');
} else {
    throw new Error('不安全的路径');
}
```

---

## 十、完整示例

### 10.1 创建项目模板

```typescript
import * as vscode from 'vscode';
import * as path from 'path';

/**
 * 创建 Node.js 项目模板
 */
async function createNodeProject(projectName: string, projectPath: string): Promise<void> {
    // 1. 创建目录结构
    const dirs = [
        'src',
        'test',
        'dist'
    ];
    
    for (const dir of dirs) {
        await vscode.workspace.fs.createDirectory(
            vscode.Uri.file(path.join(projectPath, projectName, dir))
        );
    }
    
    // 2. 创建 package.json
    const packageJson = {
        name: projectName,
        version: '1.0.0',
        main: 'dist/index.js',
        scripts: {
            build: 'tsc',
            test: 'jest',
            start: 'node dist/index.js'
        }
    };
    
    await vscode.workspace.fs.writeFile(
        vscode.Uri.file(path.join(projectPath, projectName, 'package.json')),
        Buffer.from(JSON.stringify(packageJson, null, 2))
    );
    
    // 3. 创建 TypeScript 配置
    const tsconfig = {
        compilerOptions: {
            target: 'ES2020',
            module: 'commonjs',
            outDir: './dist',
            rootDir: './src',
            strict: true,
            esModuleInterop: true
        }
    };
    
    await vscode.workspace.fs.writeFile(
        vscode.Uri.file(path.join(projectPath, projectName, 'tsconfig.json')),
        Buffer.from(JSON.stringify(tsconfig, null, 2))
    );
    
    // 4. 创建主文件
    const mainTs = `export function hello(name: string): string {
    return \`Hello, \${name}!\`;
}

console.log(hello('World'));
`;
    
    await vscode.workspace.fs.writeFile(
        vscode.Uri.file(path.join(projectPath, projectName, 'src', 'index.ts')),
        Buffer.from(mainTs)
    );
    
    // 5. 在 VS Code 中打开项目
    const uri = vscode.Uri.file(path.join(projectPath, projectName));
    vscode.workspace.updateWorkspaceFolders(0, 0, { uri });
    
    // 6. 显示完成消息
    vscode.window.showInformationMessage(
        `项目 "${projectName}" 创建成功！`,
        '打开终端',
        '运行构建'
    ).then(selection => {
        if (selection === '打开终端') {
            const terminal = vscode.window.createTerminal({
                name: 'Build',
                cwd: uri.fsPath
            });
            terminal.show();
        } else if (selection === '运行构建') {
            vscode.tasks.executeTask(
                new vscode.Task(
                    { type: 'npm' },
                    vscode.TaskScope.Workspace,
                    'build',
                    'npm',
                    new vscode.ShellExecution('npm run build')
                )
            );
        }
    });
}

// 使用示例
createNodeProject('my-app', '/Users/username/projects');
```

---

### 10.2 自动化工作流

```typescript
/**
 * 完整的开发工作流自动化
 */
async function developmentWorkflow(): Promise<void> {
    try {
        // 1. 检查依赖
        vscode.window.showInformationMessage('正在检查依赖...');
        const hasPackageJson = await exists('package.json');
        
        if (!hasPackageJson) {
            throw new Error('未找到 package.json');
        }
        
        // 2. 安装依赖
        vscode.window.showInformationMessage('正在安装依赖...');
        await executeCommand('npm install');
        
        // 3. 运行测试
        vscode.window.showInformationMessage('正在运行测试...');
        const testResult = await executeCommand('npm test');
        
        if (testResult.stdout.includes('FAIL')) {
            vscode.window.showWarningMessage('测试失败，请修复后重试');
            return;
        }
        
        // 4. 构建项目
        vscode.window.showInformationMessage('正在构建项目...');
        await executeCommand('npm run build');
        
        // 5. 完成
        vscode.window.showInformationMessage(
            '工作流完成！✅',
            '查看构建产物'
        ).then(selection => {
            if (selection === '查看构建产物') {
                vscode.commands.executeCommand('revealInExplorer', 'dist');
            }
        });
        
    } catch (error) {
        vscode.window.showErrorMessage(
            `工作流失败：${error instanceof Error ? error.message : '未知错误'}`
        );
    }
}

// 使用示例
developmentWorkflow();
```

---

## 十一、最佳实践

### 11.1 错误处理

```typescript
/**
 * 统一的错误处理包装器
 */
async function withErrorHandling<T>(
    operation: () => Promise<T>,
    errorMessage: string
): Promise<T | undefined> {
    try {
        return await operation();
    } catch (error) {
        const message = error instanceof Error ? error.message : errorMessage;
        vscode.window.showErrorMessage(`${errorMessage}: ${message}`);
        console.error('[Agent Error]', error);
        return undefined;
    }
}

// 使用示例
const content = await withErrorHandling(
    async () => await readFile('/path/to/file.txt'),
    '读取文件失败'
);
```

### 11.2 性能优化

```typescript
/**
 * 带缓存的文件读取
 */
class CachedFileSystem {
    private cache = new Map<string, string>();
    
    async readFile(path: string): Promise<string> {
        // 检查缓存
        if (this.cache.has(path)) {
            return this.cache.get(path)!;
        }
        
        // 实际读取
        const content = await vscode.workspace.fs.readFile(
            vscode.Uri.file(path)
        ).then(bytes => new TextDecoder().decode(bytes));
        
        // 更新缓存
        this.cache.set(path, content);
        
        return content;
    }
    
    invalidateCache(path: string): void {
        this.cache.delete(path);
    }
}
```

---

**文档版本:** 1.0  
**最后更新:** 2026-03-07  
**维护团队:** One AI Team  
**参考资源:** 
- [VS Code Extension API 官方文档](https://code.visualstudio.com/api)
- [VS Code Proposals](https://code.visualstudio.com/api/references/vscode-api)
