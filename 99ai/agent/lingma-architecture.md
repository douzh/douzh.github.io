# Lingma 架构设计文档

本文档详细介绍 Lingma 智能体的技术架构、实现机制和核心组件。

---

## 一、整体架构

### 1.1 三层能力架构

Lingma 的能力分为三个层级，从纯数字到物理世界：

```
层级 1: 纯数字世界（原生支持）
- 语言理解、代码生成、文档创作
- 知识问答、逻辑推理、数学计算
- 代码分析、Bug 检测、优化建议

层级 2: 操作系统（IDE 支持）
- 文件操作、命令执行、进程管理
- 项目结构分析、代码导航
- 终端命令、脚本执行

层级 3: 物理世界（通过 MCP）
- 硬件控制、IoT 设备、工业系统
- 打印机、传感器、音视频设备
- 智能家居、机器人、自动化系统
```

**关键说明：**
- **层级 1** 是 AI 的原生能力，无需外部支持
- **层级 2** 依赖 IDE 或平台提供的工具
- **层级 3** 通过 MCP 协议连接物理设备

---

### 1.2 系统架构图

```
┌──────────────────────────────────────────┐
│            用户交互层                     │
│   (聊天窗口、IDE 集成、API 接口)             │
└──────────────┬───────────────────────────┘
               ↓
┌──────────────────────────────────────────┐
│         Lingma AI 核心引擎                 │
│  ┌────────────────────────────────────┐  │
│  │  意图识别  │  任务规划  │  决策引擎  │  │
│  │  上下文管理 │  工具匹配  │  结果生成  │  │
│  └────────────────────────────────────┘  │
└──────────────┬───────────────────────────┘
               ↓ (内部 API 调用)
┌──────────────────────────────────────────┐
│          工具抽象层                        │
│  ┌────────────────────────────────────┐  │
│  │  文件系统  │  终端执行  │  进程管理  │  │
│  │  项目分析  │  代码导航  │  调试工具  │  │
│  └────────────────────────────────────┘  │
└──────────────┬───────────────────────────┘
               ↓ (IDE API)
┌──────────────────────────────────────────┐
│        IDE 平台层 (VS Code / JetBrains)    │
│  - VS Code Extension API                  │
│  - IntelliJ Platform API                  │
│  - 文件系统 API                            │
│  - 终端模拟器 API                          │
│  - 调试器 API                              │
└──────────────┬───────────────────────────┘
               ↓ (系统调用)
┌──────────────────────────────────────────┐
│            操作系统层                      │
│  - Windows / macOS / Linux                │
│  - 文件系统                               │
│  - 进程管理                               │
│  - 网络通信                               │
└──────────────────────────────────────────┘
```

---

## 二、核心组件详解

### 2.1 AI 核心引擎

#### **职责**
- 理解用户自然语言输入
- 分析意图并生成执行计划
- 决定是否需要调用工具
- 选择合适的工具
- 格式化返回结果

#### **工作流程**

```python
class LingmaCore:
    async def process_message(self, user_message: str):
        # 步骤 1: 意图识别
        intent = self.analyze_intent(user_message)
        # 示例："帮我读取 README.md" → intent = "file.read"
        
        # 步骤 2: 参数提取
        params = self.extract_params(user_message)
        # 示例：{"path": "README.md"}
        
        # 步骤 3: 工具匹配
        if self.needs_tool(intent):
            tool = self.match_tool(intent, params)
            
            # 步骤 4: 调用工具
            result = await self.call_tool(tool, params)
            
            # 步骤 5: 处理结果
            return self.format_result(result)
        else:
            # 纯对话，直接回复
            return await self.generate_response(user_message)
```

#### **意图识别算法**

```python
def analyze_intent(self, message: str) -> Intent:
    """
    分析用户意图
    """
    # 1. 关键词提取
    keywords = self.extract_keywords(message)
    # "帮我运行 mvn clean" → ["运行", "mvn", "clean"]
    
    # 2. 语义分析
    semantic_vector = self.embed(message)
    
    # 3. 模式匹配
    patterns = {
        r"读取.*文件": Intent.FILE_READ,
        r"创建.*文件": Intent.FILE_CREATE,
        r"运行.*命令": Intent.CMD_EXECUTE,
        r"查找.*类": Intent.CODE_SEARCH,
        r"重构.*代码": Intent.CODE_REFACTOR
    }
    
    for pattern, intent in patterns.items():
        if re.search(pattern, message):
            return intent
    
    # 4. 基于机器学习的分类
    return self.ml_classifier.predict(message)
```

---

### 2.2 工具抽象层

#### **设计目标**
- 统一的工具调用接口
- 屏蔽不同 IDE 的差异
- 提供错误处理和重试机制
- 支持异步执行

#### **工具接口定义**

```typescript
interface ITool {
    /**
     * 工具名称
     */
    name: string;
    
    /**
     * 工具描述
     */
    description: string;
    
    /**
     * 输入参数 schema
     */
    inputSchema: JSONSchema;
    
    /**
     * 执行工具
     * @param action 具体动作
     * @param params 参数
     */
    execute(action: string, params: any): Promise<ToolResult>;
}

interface ToolResult {
    success: boolean;
    data?: any;
    error?: string;
    message?: string;
}
```

---

### 2.3 文件系统工具

#### **VS Code 实现**
```typescript
// VS Code 扩展中的文件操作实现
import * as vscode from 'vscode';

// 读取文件
async function readFile(filePath: string): Promise<string> {
    const uri = vscode.Uri.file(filePath);
    const bytes = await vscode.workspace.fs.readFile(uri);
    return new TextDecoder().decode(bytes);
}

// 写入文件
async function writeFile(filePath: string, content: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    const bytes = new TextEncoder().encode(content);
    await vscode.workspace.fs.writeFile(uri, bytes);
}

// 创建文件
async function createFile(filePath: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    await vscode.workspace.fs.writeFile(uri, new Uint8Array());
}

// 删除文件
async function deleteFile(filePath: string): Promise<void> {
    const uri = vscode.Uri.file(filePath);
    await vscode.workspace.fs.delete(uri);
}

// 列出目录
async function listDirectory(dirPath: string): Promise<string[]> {
    const uri = vscode.Uri.file(dirPath);
    const entries = await vscode.workspace.fs.readDirectory(uri);
    return entries.map(([name]) => name);
}
````

```typescript
import * as vscode from 'vscode';

export class FileSystemTool implements ITool {
    name = 'file_system';
    description = '文件和目录操作工具';
    
    inputSchema = {
        type: 'object',
        properties: {
            action: {
                type: 'string',
                enum: ['read', 'write', 'create', 'delete', 'list', 'move', 'copy']
            },
            path: { type: 'string' },
            content: { type: 'string' },
            recursive: { type: 'boolean' }
        },
        required: ['action', 'path']
    };
    
    async execute(action: string, params: any): Promise<ToolResult> {
        switch (action) {
            case 'read':
                return await this.readFile(params.path);
            case 'write':
                return await this.writeFile(params.path, params.content);
            case 'create':
                return await this.createFile(params.path);
            case 'delete':
                return await this.deleteFile(params.path);
            case 'list':
                return await this.listDirectory(params.path);
            case 'move':
                return await this.moveFile(params.path, params.newPath);
            case 'copy':
                return await this.copyFile(params.path, params.newPath);
            default:
                return { success: false, error: `Unknown action: ${action}` };
        }
    }
    
    private async readFile(path: string): Promise<ToolResult> {
        try {
            // 安全检查
            if (!this.validatePath(path)) {
                return { success: false, error: 'Invalid path' };
            }
            
            const uri = vscode.Uri.file(path);
            const bytes = await vscode.workspace.fs.readFile(uri);
            const content = new TextDecoder().decode(bytes);
            
            return {
                success: true,
                data: {
                    content,
                    size: bytes.length,
                    lastModified: await this.getLastModified(uri)
                }
            };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Read failed'
            };
        }
    }
    
    private async writeFile(path: string, content: string): Promise<ToolResult> {
        try {
            if (!this.validatePath(path)) {
                return { success: false, error: 'Invalid path' };
            }
            
            const uri = vscode.Uri.file(path);
            const bytes = new TextEncoder().encode(content);
            await vscode.workspace.fs.writeFile(uri, bytes);
            
            return {
                success: true,
                message: `File written: ${path}`,
                data: { size: bytes.length }
            };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Write failed'
            };
        }
    }
    
    private async createFile(path: string): Promise<ToolResult> {
        try {
            if (!this.validatePath(path)) {
                return { success: false, error: 'Invalid path' };
            }
            
            const uri = vscode.Uri.file(path);
            await vscode.workspace.fs.writeFile(uri, new Uint8Array());
            
            return {
                success: true,
                message: `File created: ${path}`
            };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'Create failed'
            };
        }
    }
    
    private async listDirectory(path: string): Promise<ToolResult> {
        try {
            if (!this.validatePath(path)) {
                return { success: false, error: 'Invalid path' };
            }
            
            const uri = vscode.Uri.file(path);
            const entries = await vscode.workspace.fs.readDirectory(uri);
            
            return {
                success: true,
                data: {
                    files: entries.map(([name, type]) => ({
                        name,
                        type: this.getFileType(type)
                    }))
                }
            };
        } catch (error) {
            return {
                success: false,
                error: error instanceof Error ? error.message : 'List failed'
            };
        }
    }
    
    /**
     * 路径安全验证
     */
    private validatePath(path: string): boolean {
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
}
```

---

### 2.4 终端执行工具

#### **VS Code 实现**

```typescript
import * as vscode from 'vscode';

export class TerminalTool implements ITool {
    name = 'terminal';
    description = '终端命令执行工具';
    
    inputSchema = {
        type: 'object',
        properties: {
            command: { 
                type: 'string',
                description: '要执行的命令'
            },
            cwd: { 
                type: 'string',
                description: '工作目录'
            },
            shell: {
                type: 'string',
                description: '使用的 shell'
            },
            timeout: {
                type: 'number',
                description: '超时时间（毫秒）'
            }
        },
        required: ['command']
    };
    
    async execute(action: string, params: any): Promise<ToolResult> {
        if (action === 'run') {
            return await this.runCommand(params);
        }
        return { success: false, error: `Unknown action: ${action}` };
    }
    
    private async runCommand(params: any): Promise<ToolResult> {
        const { command, cwd, shell, timeout = 30000 } = params;
        
        // 1. 安全检查
        if (!this.validateCommand(command)) {
            return {
                success: false,
                error: 'Command blocked by security check'
            };
        }
        
        return new Promise((resolve) => {
            // 2. 创建临时终端
            const terminal = vscode.window.createTerminal({
                name: 'Lingma Task',
                cwd: cwd,
                shellPath: shell
            });
            
            let output = '';
            let errorOutput = '';
            
            // 3. 监听终端输出
            const disposable = vscode.window.onDidWriteTerminalData(e => {
                if (e.terminal === terminal) {
                    output += e.data;
                }
            });
            
            // 4. 发送命令
            terminal.show();
            terminal.sendText(command, true);
            
            // 5. 超时处理
            const timer = setTimeout(() => {
                terminal.dispose();
                disposable.dispose();
                resolve({
                    success: false,
                    error: 'Command execution timeout',
                    data: { partialOutput: output }
                });
            }, timeout);
            
            // 6. 监听退出（简化处理）
            setTimeout(() => {
                terminal.dispose();
                disposable.dispose();
                clearTimeout(timer);
                
                resolve({
                    success: true,
                    data: {
                        stdout: output,
                        stderr: errorOutput,
                        exitCode: 0
                    },
                    message: 'Command executed successfully'
                });
            }, Math.min(timeout, 5000)); // 简单等待
        });
    }
    
    /**
     * 命令安全验证
     */
    private validateCommand(command: string): boolean {
        // 黑名单检查
        const blockedCommands = [
            'rm -rf /',
            'del /f /s /q C:\\*.*',
            'format c:',
            'mkfs',
            'dd if=/dev/zero',
            ':(){ :|:& };:'  // Fork bomb
        ];
        
        // 检查是否包含危险命令
        if (blockedCommands.some(cmd => command.includes(cmd))) {
            return false;
        }
        
        // 检查是否在允许的命令列表中
        const allowedPatterns = [
            /^mvn\s+.*/,           // Maven
            /^npm\s+.*/,           // npm
            /^node\s+.*/,          // Node.js
            /^java\s+.*/,          // Java
            /^javac\s+.*/,         // Java compiler
            /^git\s+.*/,           // Git
            /^docker\s+.*/,        // Docker
            /^python[3]?\s+.*/,    // Python
            /^echo\s+.*/,          // Echo
            /^cat\s+.*/,           // Cat
            /^ls\s+.*/,            // LS
            /^dir\s.*/,            // Dir
            /^cd\s.*/,             // CD
            /^mkdir\s+.*/,         // Mkdir
            /^touch\s+.*/,         // Touch
            /^grep\s+.*/,          // Grep
            /^find\s+.*/,          // Find
        ];
        
        return allowedPatterns.some(pattern => pattern.test(command));
    }
}
```

---

### 2.5 项目分析工具

#### **实现方式**

```typescript
interface ProjectStructure {
    root: string;
    projectType: 'maven' | 'gradle' | 'npm' | 'python' | 'other';
    srcDirs: string[];
    testDirs: string[];
    dependencies: Dependency[];
    mainClasses: string[];
    buildCommands: string[];
    testCommands: string[];
}

export class ProjectAnalyzer {
    /**
     * 分析项目结构
     */
    async analyzeProject(rootPath: string): Promise<ProjectStructure> {
        const structure: ProjectStructure = {
            root: rootPath,
            projectType: 'other',
            srcDirs: [],
            testDirs: [],
            dependencies: [],
            mainClasses: [],
            buildCommands: [],
            testCommands: []
        };
        
        // 1. 识别项目类型
        structure.projectType = await this.identifyProjectType(rootPath);
        
        // 2. 根据项目类型配置
        switch (structure.projectType) {
            case 'maven':
                await this.analyzeMavenProject(rootPath, structure);
                break;
            case 'gradle':
                await this.analyzeGradleProject(rootPath, structure);
                break;
            case 'npm':
                await this.analyzeNpmProject(rootPath, structure);
                break;
            case 'python':
                await this.analyzePythonProject(rootPath, structure);
                break;
        }
        
        // 3. 查找主类/入口点
        structure.mainClasses = await this.findEntryPoints(structure.srcDirs);
        
        return structure;
    }
    
    /**
     * 识别项目类型
     */
    private async identifyProjectType(rootPath: string): Promise<ProjectStructure['projectType']> {
        const checks = [
            { file: 'pom.xml', type: 'maven' as const },
            { file: 'build.gradle', type: 'gradle' as const },
            { file: 'package.json', type: 'npm' as const },
            { file: 'requirements.txt', type: 'python' as const },
            { file: 'setup.py', type: 'python' as const },
        ];
        
        for (const { file, type } of checks) {
            const exists = await this.fileExists(path.join(rootPath, file));
            if (exists) {
                return type;
            }
        }
        
        return 'other';
    }
    
    /**
     * 分析 Maven 项目
     */
    private async analyzeMavenProject(rootPath: string, structure: ProjectStructure): Promise<void> {
        structure.srcDirs = ['src/main/java', 'src/main/resources'];
        structure.testDirs = ['src/test/java', 'src/test/resources'];
        structure.buildCommands = ['mvn clean install', 'mvn package'];
        structure.testCommands = ['mvn test'];
        
        // 解析 pom.xml
        const pomPath = path.join(rootPath, 'pom.xml');
        const pomContent = await readFile(pomPath);
        structure.dependencies = this.parsePomXml(pomContent);
    }
    
    /**
     * 查找入口点（main 方法）
     */
    private async findEntryPoints(srcDirs: string[]): Promise<string[]> {
        const entryPoints: string[] = [];
        
        for (const dir of srcDirs) {
            const javaFiles = await this.findFilesByExtension(dir, '.java');
            
            for (const file of javaFiles) {
                const content = await readFile(file);
                
                // 查找包含 main 方法的类
                if (content.includes('public static void main')) {
                    const className = this.extractClassName(file, content);
                    entryPoints.push(className);
                }
            }
        }
        
        return entryPoints;
    }
    
    /**
     * 解析 Java 类名
     */
    private extractClassName(filePath: string, content: string): string {
        // 简单的正则提取（实际实现需要更复杂的 AST 解析）
        const match = content.match(/public\s+class\s+(\w+)/);
        if (match) {
            const packageName = this.extractPackageName(content);
            return packageName ? `${packageName}.${match[1]}` : match[1];
        }
        return path.basename(filePath, '.java');
    }
}
```

---

## 三、AI 与 IDE 通信机制

### 3.1 消息格式

#### **请求消息**

```json
{
  "type": "tool_call",
  "requestId": "req_123456",
  "timestamp": "2026-03-07T10:00:00Z",
  "tool": "file_system",
  "action": "read_file",
  "params": {
    "path": "/absolute/path/to/file.java"
  },
  "metadata": {
    "userId": "user_001",
    "sessionId": "session_abc"
  }
}
```

#### **响应消息**

```json
{
  "type": "tool_response",
  "requestId": "req_123456",
  "timestamp": "2026-03-07T10:00:01Z",
  "success": true,
  "data": {
    "content": "public class App {...}",
    "size": 1024,
    "lastModified": "2026-03-07T09:00:00Z",
    "encoding": "UTF-8"
  },
  "message": "File read successfully",
  "executionTime": 125
}
```

#### **错误响应**

```json
{
  "type": "tool_response",
  "requestId": "req_123456",
  "success": false,
  "error": {
    "code": "FILE_NOT_FOUND",
    "message": "File does not exist: /path/to/file.java",
    "details": {
      "attemptedPath": "/path/to/file.java",
      "workspaceRoot": "/workspace"
    }
  }
}
```

---

### 3.2 通信流程

```
┌─────────┐              ┌─────────┐              ┌─────────┐
│   AI    │              │  IDE    │              │    OS   │
│  Core   │              │ Extension│              │         │
└────┬────┘              └────┬────┘              └────┬────┘
     │                        │                        │
     │ 1. 接收用户请求         │                        │
     │    "读取 README.md"     │                        │
     │                        │                        │
     │ 2. 意图识别             │                        │
     │    intent = FILE_READ   │                        │
     │                        │                        │
     │ 3. 参数提取             │                        │
     │    path="README.md"     │                        │
     │                        │                        │
     │ 4. 发送工具调用请求     │                        │
     ├───────────────────────>│                        │
     │    JSON-RPC            │                        │
     │                        │                        │
     │                        │ 5. 安全检查            │
     │                        │    validatePath()      │
     │                        │                        │
     │                        │ 6. 调用 VS Code API    │
     │                        ├───────────────────────>│
     │                        │    fs.readFile()       │
     │                        │                        │
     │                        │ 7. 返回文件内容        │
     │                        <────────────────────────┤
     │                        │                        │
     │ 8. 返回结果给 AI        │                        │
     │    {content: "..."}    │                        │
     │<───────────────────────┤                        │
     │                        │                        │
     │ 9. 格式化输出          │                        │
     │    "已读取 README.md"   │                        │
     │                        │                        │
     ▼                        ▼                        ▼
```

---

## 四、完整工作流程示例

### 4.1 创建 Maven 项目

```
用户：帮我初始化一个 Java Maven 项目

【阶段 1】需求理解
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
AI Core:
  输入："帮我初始化一个 Java Maven 项目"
  ↓
  意图识别：PROJECT_INIT
  参数：{ type: "maven", language: "java" }
  ↓
  任务规划：需要创建多个文件和目录

【阶段 2】任务分解
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Task Planner:
  tasks = [
    {
      tool: "file_system",
      action: "create_dirs",
      params: {
        paths: [
          "src/main/java",
          "src/main/resources",
          "src/test/java",
          "src/test/resources"
        ]
      }
    },
    {
      tool: "file_system",
      action: "create_file",
      params: {
        path: "pom.xml",
        template: "maven-pom-template",
        variables: {
          groupId: "com.example",
          artifactId: "my-app",
          version: "1.0-SNAPSHOT"
        }
      }
    },
    {
      tool: "file_system",
      action: "create_file",
      params: {
        path: "src/main/java/App.java",
        template: "java-main-class"
      }
    },
    {
      tool: "file_system",
      action: "create_file",
      params: {
        path: "src/test/java/AppTest.java",
        template: "junit-test-class"
      }
    },
    {
      tool: "file_system",
      action: "create_file",
      params: {
        path: "README.md",
        template: "project-readme"
      }
    }
  ]

【阶段 3】并行执行
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Tool Executor:
  // 并发执行所有任务
  const results = await Promise.all(tasks.map(task => 
    executeTool(task.tool, task.action, task.params)
  ));
  
  // 收集结果
  results.forEach((result, index) => {
    if (result.success) {
      log(`✓ Created: ${tasks[index].params.path}`);
    } else {
      logError(`✗ Failed: ${tasks[index].params.path}`);
    }
  });

【阶段 4】结果汇总
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Response Generator:
  输出：
  "✅ 项目已创建完成！
  
   ✓ src/main/java
   ✓ src/main/resources  
   ✓ src/test/java
   ✓ src/test/resources
   ✓ pom.xml
   ✓ App.java
   ✓ AppTest.java
   ✓ README.md
   
   项目位置：/Users/username/my-project
   
   下一步建议：
   1. 打开终端运行：mvn clean install
   2. 修改 App.java 添加业务逻辑
   3. 编写单元测试"
```

---

### 4.2 执行构建命令

```
用户：帮我运行 mvn clean install

【阶段 1】命令识别
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
AI Core:
  输入："帮我运行 mvn clean install"
  ↓
  意图识别：CMD_EXECUTE
  参数：{
    command: "mvn clean install",
    toolType: "terminal"
  }
  ↓
  安全检查：命令在白名单中 ✓

【阶段 2】环境准备
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Terminal Tool:
  // 1. 检查工作目录
  cwd = getWorkspaceRoot();
  
  // 2. 验证 Maven 是否安装
  mavenInstalled = await commandExists('mvn');
  if (!mavenInstalled) {
    return { 
      success: false, 
      error: "Maven not found. Please install Maven first." 
    };
  }
  
  // 3. 创建临时终端
  terminal = vscode.window.createTerminal({
    name: 'Lingma Build',
    cwd: cwd
  });

【阶段 3】执行命令
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Terminal Tool:
  // 显示终端
  terminal.show();
  
  // 发送命令
  terminal.sendText('mvn clean install', true);
  
  // 监听输出
  let output = '';
  const listener = vscode.window.onDidWriteTerminalData(e => {
    if (e.terminal === terminal) {
      output += e.data;
      
      // 实时反馈给用户
      if (output.includes('BUILD SUCCESS')) {
        streamToUser('✅ BUILD SUCCESS');
      } else if (output.includes('BUILD FAILURE')) {
        streamToUser('❌ BUILD FAILURE');
      }
    }
  });
  
  // 等待完成
  await waitForCompletion(output, timeout=300000);

【阶段 4】结果分析
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Result Analyzer:
  // 解析 Maven 输出
  const analysis = {
    success: output.includes('BUILD SUCCESS'),
    duration: extractDuration(output),
    testsRun: extractTestsRun(output),
    testsFailed: extractTestsFailed(output),
    artifacts: extractArtifacts(output)
  };
  
  // 生成总结
  summary = `
构建${analysis.success ? '✅成功' : '❌失败'}
耗时：${analysis.duration}ms
测试：${analysis.testsRun} run, ${analysis.testsFailed} failed
产物：${analysis.artifacts.join(', ')}
  `;

【阶段 5】返回结果
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Response:
  "✅ Maven 构建完成
  
   执行时间：5 分 32 秒
   测试结果：15 tests run, 0 failed
   构建产物：
   - target/my-app-1.0-SNAPSHOT.jar
   - target/my-app-1.0-SNAPSHOT-sources.jar
   
   输出摘要：
   [INFO] Compiling 5 source files
   [INFO] Running com.example.AppTest
   [INFO] Tests run: 15, Failures: 0
   [INFO] BUILD SUCCESS"
```

---

## 五、安全机制

### 5.1 多层安全防护

```typescript
class SecurityManager {
    /**
     * 第一层：路径验证
     */
    static validateFilePath(path: string): boolean {
        // 1. 防止路径遍历攻击
        if (path.includes('../') || path.includes('..\\')) {
            console.warn('[Security] Path traversal detected');
            return false;
        }
        
        // 2. 检查工作区边界
        const workspaceRoot = vscode.workspace.rootPath;
        if (workspaceRoot && !path.startsWith(workspaceRoot)) {
            console.warn('[Security] Path outside workspace');
            return false;
        }
        
        // 3. 检查敏感文件
        const sensitiveFiles = [
            '.env', '.git/config', 'id_rsa', 'authorized_keys',
            'shadow', 'passwd', '.bash_history'
        ];
        if (sensitiveFiles.some(f => path.endsWith(f))) {
            console.warn('[Security] Sensitive file access blocked');
            return false;
        }
        
        return true;
    }
    
    /**
     * 第二层：命令验证
     */
    static validateCommand(command: string): boolean {
        // 1. 黑名单检查
        const blockedPatterns = [
            /rm\s+-rf\s+\//,           // 删除根目录
            /del\s+\/f\s+\/s\s+\/q\s+C:\\/,  // Windows 删除
            /format\s+[cCdD]:/,        // 格式化磁盘
            /mkfs/,                     // 创建文件系统
            /dd\s+if=\/dev\/zero/,     // 写零设备
            /:()\{\s*:|:&\s*\};:/,     // Fork bomb
            /chmod\s+-R\s+777\s+\//,   // 危险权限
            /wget\s+.*\|.*sh/,         // 下载并执行
            /curl\s+.*\|.*sh/          // 下载并执行
        ];
        
        if (blockedPatterns.some(pattern => pattern.test(command))) {
            console.warn('[Security] Dangerous command blocked');
            return false;
        }
        
        // 2. 白名单检查（可选）
        const allowedPatterns = [
            /^mvn\s+.*/,
            /^npm\s+(install|run|test|build|.+)/,
            /^node\s+.*/,
            /^java\s+.*/,
            /^javac\s+.*/,
            /^git\s+.*/,
            /^docker\s+.*/,
            /^python[3]?\s+.*/,
            /^echo\s+.*/,
            /^cat\s+.*/,
            /^ls\s+.*/,
            /^grep\s+.*/,
            /^find\s+.*/
        ];
        
        const isAllowed = allowedPatterns.some(pattern => 
            pattern.test(command)
        );
        
        if (!isAllowed) {
            console.warn('[Security] Command not in whitelist');
            // 可以选择阻止或仅记录日志
        }
        
        return isAllowed;
    }
    
    /**
     * 第三层：权限检查
     */
    static checkPermission(
        userId: string, 
        action: string, 
        resource?: string
    ): boolean {
        // 基于角色的访问控制
        const userRole = getUserRole(userId);
        const requiredRole = getRequiredRole(action);
        
        if (userRole.level < requiredRole.level) {
            console.warn(`[Security] Insufficient permissions: ${userRole.name} < ${requiredRole.name}`);
            return false;
        }
        
        return true;
    }
}
```

---

### 5.2 审计日志

```typescript
class AuditLogger {
    /**
     * 记录所有工具调用
     */
    static logToolCall(
        userId: string,
        tool: string,
        action: string,
        params: any,
        result: ToolResult
    ): void {
        const logEntry = {
            timestamp: new Date().toISOString(),
            userId,
            tool,
            action,
            params: this.sanitizeParams(params), // 脱敏
            success: result.success,
            error: result.error,
            executionTime: result.executionTime
        };
        
        // 写入审计日志
        fs.appendFileSync(
            'audit.log',
            JSON.stringify(logEntry) + '\n'
        );
        
        // 异常行为告警
        if (!result.success && this.isSuspicious(action)) {
            this.sendAlert(userId, action, result.error);
        }
    }
    
    /**
     * 参数脱敏
     */
    private static sanitizeParams(params: any): any {
        const sensitive = ['password', 'secret', 'token', 'key'];
        const sanitized = { ...params };
        
        for (const key of Object.keys(sanitized)) {
            if (sensitive.some(s => key.toLowerCase().includes(s))) {
                sanitized[key] = '[REDACTED]';
            }
        }
        
        return sanitized;
    }
}
```

---

## 六、性能优化

### 6.1 缓存策略

```typescript
class CacheManager {
    private cache = new Map<string, CachedResult>();
    
    /**
     * 带缓存的文件读取
     */
    async readFileWithCache(path: string): Promise<string> {
        const cacheKey = `file:${path}`;
        const cached = this.cache.get(cacheKey);
        
        // 检查缓存是否有效
        if (cached && !this.isExpired(cached) && !this.isModified(path, cached)) {
            return cached.content;
        }
        
        // 缓存未命中，读取文件
        const content = await this.actualReadFile(path);
        
        // 更新缓存
        this.cache.set(cacheKey, {
            content,
            timestamp: Date.now(),
            mtime: await this.getMtime(path)
        });
        
        return content;
    }
    
    /**
     * LRU 缓存淘汰
     */
    private evictIfNeeded(): void {
        const MAX_CACHE_SIZE = 1000;
        
        if (this.cache.size > MAX_CACHE_SIZE) {
            // 找到最旧的条目
            const oldest = Array.from(this.cache.entries())
                .sort((a, b) => a[1].timestamp - b[1].timestamp)[0];
            
            this.cache.delete(oldest[0]);
        }
    }
}
```

---

### 6.2 并发控制

```typescript
class ConcurrencyController {
    private activeTasks = 0;
    private readonly MAX_CONCURRENT = 5;
    private queue: Array<() => Promise<any>> = [];
    
    /**
     * 限制并发任务数
     */
    async executeWithLimit<T>(task: () => Promise<T>): Promise<T> {
        if (this.activeTasks >= this.MAX_CONCURRENT) {
            // 加入队列
            return new Promise((resolve, reject) => {
                this.queue.push(async () => {
                    try {
                        const result = await task();
                        resolve(result);
                    } catch (error) {
                        reject(error);
                    }
                });
            });
        }
        
        this.activeTasks++;
        
        try {
            return await task();
        } finally {
            this.activeTasks--;
            this.processQueue();
        }
    }
    
    /**
     * 处理队列中的下一个任务
     */
    private processQueue(): void {
        if (this.queue.length > 0 && this.activeTasks < this.MAX_CONCURRENT) {
            const nextTask = this.queue.shift();
            if (nextTask) {
                nextTask();
            }
        }
    }
}
```

---

## 七、错误处理

### 7.1 统一错误处理

```typescript
class ErrorHandler {
    /**
     * 处理工具调用错误
     */
    static handleToolError(
        tool: string,
        action: string,
        error: Error
    ): ToolResult {
        // 记录错误
        logger.error(`Tool error: ${tool}.${action}`, error);
        
        // 分类错误
        const errorType = this.classifyError(error);
        
        switch (errorType) {
            case 'FILE_NOT_FOUND':
                return {
                    success: false,
                    error: 'File not found',
                    suggestion: 'Please check the file path and try again'
                };
            
            case 'PERMISSION_DENIED':
                return {
                    success: false,
                    error: 'Permission denied',
                    suggestion: 'You do not have permission to perform this action'
                };
            
            case 'TIMEOUT':
                return {
                    success: false,
                    error: 'Operation timed out',
                    suggestion: 'The operation took too long. Try again or contact support'
                };
            
            default:
                return {
                    success: false,
                    error: 'An unexpected error occurred',
                    suggestion: 'Please try again or report this issue'
                };
        }
    }
    
    /**
     * 错误分类
     */
    private static classifyError(error: Error): string {
        if (error.message.includes('ENOENT')) {
            return 'FILE_NOT_FOUND';
        }
        if (error.message.includes('EACCES')) {
            return 'PERMISSION_DENIED';
        }
        if (error.message.includes('ETIMEDOUT')) {
            return 'TIMEOUT';
        }
        return 'UNKNOWN';
    }
}
```

---

## 八、监控与诊断

### 8.1 性能指标

```typescript
class MetricsCollector {
    private metrics = {
        toolCalls: 0,
        successfulCalls: 0,
        failedCalls: 0,
        avgExecutionTime: 0,
        p95ExecutionTime: 0,
        cacheHitRate: 0
    };
    
    /**
     * 记录工具调用
     */
    recordToolCall(executionTime: number, success: boolean): void {
        this.metrics.toolCalls++;
        
        if (success) {
            this.metrics.successfulCalls++;
        } else {
            this.metrics.failedCalls++;
        }
        
        // 更新平均执行时间
        this.metrics.avgExecutionTime = 
            (this.metrics.avgExecutionTime * (this.metrics.toolCalls - 1) + executionTime) 
            / this.metrics.toolCalls;
        
        // 更新 P95 执行时间（使用直方图）
        this.updateP95(executionTime);
    }
    
    /**
     * 获取健康状态
     */
    getHealthStatus(): HealthStatus {
        const successRate = this.metrics.successfulCalls / this.metrics.toolCalls;
        
        return {
            status: successRate > 0.95 ? 'healthy' : 'degraded',
            metrics: this.metrics,
            recommendations: this.generateRecommendations()
        };
    }
}
```

---

## 九、扩展机制

### 9.1 自定义工具注册

```typescript
interface CustomTool {
    name: string;
    version: string;
    author: string;
    execute(action: string, params: any): Promise<ToolResult>;
}

class ToolRegistry {
    private tools = new Map<string, ITool>();
    
    /**
     * 注册自定义工具
     */
    registerTool(tool: CustomTool): void {
        if (this.tools.has(tool.name)) {
            throw new Error(`Tool ${tool.name} already exists`);
        }
        
        // 验证工具接口
        this.validateTool(tool);
        
        // 注册
        this.tools.set(tool.name, tool);
        
        console.log(`Tool registered: ${tool.name}@${tool.version}`);
    }
    
    /**
     * 获取工具
     */
    getTool(name: string): ITool | undefined {
        return this.tools.get(name);
    }
    
    /**
     * 列出所有工具
     */
    listTools(): ToolInfo[] {
        return Array.from(this.tools.values()).map(tool => ({
            name: tool.name,
            description: tool.description,
            inputSchema: tool.inputSchema
        }));
    }
}
```

---

## 十、最佳实践

### 10.1 开发建议

1. **始终进行输入验证**
   - 验证所有用户输入
   - 使用白名单而非黑名单
   - 实施适当的转义

2. **最小权限原则**
   - 只授予必要的权限
   - 定期审查权限设置
   - 实施基于角色的访问控制

3. **错误处理**
   - 捕获所有异常
   - 提供有意义的错误信息
   - 记录详细的错误日志

4. **性能优化**
   - 实施缓存策略
   - 限制并发数量
   - 监控性能指标

5. **可维护性**
   - 保持代码简洁
   - 添加充分的注释
   - 编写单元测试

---

**文档版本:** 1.0  
**最后更新:** 2026-03-07  
**维护团队:** One AI Team  
**联系方式:** support@lingma.ai
