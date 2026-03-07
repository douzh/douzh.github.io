# Lingma Skill 配置指南

本文档详细介绍如何在 Lingma 中创建、配置和使用 Skill（技能），让你能够封装复杂工作流，实现一键执行。

---

## 一、什么是 Skill

### **1.1 Skill 的概念**

**Skill（技能）** 是预定义的复杂操作流程，它将多个步骤封装为一个可重复使用的能力单元。

```
Skill = 多步骤流程 + 自动化执行 + 最佳实践封装
```

### **1.2 Skill vs 记忆 vs 规则**

| 特性 | **记忆** | **规则** | **Skill** |
|------|---------|---------|----------|
| **本质** | 知识和信息 | 行为和约束 | **流程和能力** |
| **用途** | 记住偏好 | 规范行为 | **执行复杂任务** |
| **复杂度** | 简单信息 | 单条指令 | **多步骤流程** |
| **示例** | "项目用 Maven" | "必须写注释" | **"一键部署"** |

### **1.3 适用场景**

✅ **适合用 Skill 的场景：**
- 需要多步骤才能完成的任务
- 需要重复执行的标准流程
- 包含专业知识的操作
- 团队协作的最佳实践
- 容易出错但必须执行的操作

❌ **不适合用 Skill 的场景：**
- 单一简单操作
- 需要创造性思维的任务
- 纯咨询类问题

---

## 二、Skill 的实现方式

Lingma 提供三种 Skill 实现方式，从简单到复杂：

```
难度递增 →
┌─────────────┬─────────────┬──────────────┐
│   记忆封装   │   规则定义   │   MCP 工具    │
│   ⭐        │   ⭐⭐       │   ⭐⭐⭐        │
│   5 分钟     │   15 分钟    │   1 小时 +      │
└─────────────┴─────────────┴──────────────┘
          ↓           ↓            ↓
      简单流程    中等流程     复杂工作流
```

---

### **2.4 Skill 中的工具自动安装**

在 Skill 执行过程中，经常需要依赖外部工具（如 npm、Maven、Docker 等）。Lingma 支持**智能检测和自动安装**所需工具。

#### **工具安装能力说明**

**✅ 我能自动安装的：**
- ✅ npm/yarn/pnpm 包（Node.js 生态）
- ✅ pip/poetry 包（Python 生态）
- ✅ Maven/Gradle 依赖（Java 生态）
- ✅ Chocolatey 包（Windows）
- ✅ Homebrew 包（macOS）
- ✅ APT/YUM包（Linux，需要 sudo 权限）

**❌ 我不能自动安装的：**
- ❌ 需要图形界面交互的软件（VSCode、IDEA）
- ❌ 需要管理员密码的系统软件
- ❌ .exe/.dmg 安装包下载和执行
- ❌ 浏览器下载文件

#### **工具安装策略**

```typescript
// Skill 中的智能安装逻辑
async function smartInstallTool(toolName: string): Promise<InstallResult> {
  // 步骤 1: 检查是否已安装
  const installed = await checkToolInstalled(toolName);
  if (installed) {
    return { status: 'already_installed', tool: toolName };
  }
  
  // 步骤 2: 尝试自动安装（使用包管理器）
  try {
    const installCmd = getInstallCommand(toolName, process.platform);
    await executeCommand(installCmd);
    
    // 步骤 3: 验证安装
    const verifyResult = await executeCommand(`${toolName} --version`);
    if (verifyResult.code === 0) {
      return { status: 'installed', tool: toolName, version: extractVersion(verifyResult.stdout) };
    }
  } catch (error) {
    // 安装失败，但不阻塞
  }
  
  // 步骤 4: 提供手动安装指南
  return {
    status: 'failed',
    tool: toolName,
    manualGuide: getManualInstallGuide(toolName)
  };
}
```

#### **在 Skill 中添加预检和安装**

```json
{
  "skill": "setup-node-project",
  "preflightCheck": {
    "enabled": true,
    "tools": [
      {"name": "node", "version": ">=18"},
      {"name": "npm", "version": ">=9"},
      {"name": "git"}
    ],
    "autoInstall": true
  },
  "steps": [
    {
      "name": "环境检查",
      "action": "preflight_check",
      "onMissing": "auto_install_or_prompt"
    },
    {
      "name": "安装项目依赖",
      "command": "npm install"
    }
  ]
}
```

## 三、方式一：用记忆封装 Skill

这是最简单的方式，适合个人使用。

### **3.1 创建记忆 Skill**

#### **示例 1：项目初始化 Skill（带工具检测）**

```
您：我想创建一个 Skill，当我输入“初始化 Java 项目”时，执行以下步骤：
      1. 检查 Java 和 Maven 是否安装，未安装则提示
      2. 创建 Maven 目录结构
      3. 生成 pom.xml（包含 JUnit 和 Log4j）
      4. 创建主类 App.java
      5. 创建测试类 AppTest.java
      6. 添加 README.md
      7. 配置.gitignore
      8. 运行 mvn clean compile 验证

Lingma: ✅ 已创建 Skill: java-project-init

【记忆内容】
类别：project_introduction
标题：Java 项目初始化流程（带工具检测）
内容：
{
  "skill": "java-project-init",
  "trigger": "初始化 Java 项目",
  "preflightCheck": {
    "enabled": true,
    "tools": [
      {"name": "java", "version": ">=17"},
      {"name": "mvn", "version": ">=3.8"}
    ],
    "autoInstall": true,
    "onMissing": "prompt_or_install"
  },
  "steps": [
    {
      "name": "环境预检",
      "action": "preflight_check",
      "description": "检查 Java 和 Maven 是否安装",
      "onError": {
        "action": "try_install",
        "fallback": "提供手动安装指南并等待用户确认"
      }
    },
    {
      "name": "创建目录",
      "action": "create_dirs",
      "paths": [
        "src/main/java",
        "src/main/resources",
        "src/test/java",
        "src/test/resources"
      ]
    },
    {
      "name": "生成 pom.xml",
      "action": "create_file",
      "template": "maven-pom",
      "dependencies": ["junit", "log4j"]
    },
    {
      "name": "创建主类",
      "action": "create_file",
      "path": "src/main/java/App.java",
      "template": "java-main-class"
    },
    {
      "name": "创建测试类",
      "action": "create_file",
      "path": "src/test/java/AppTest.java",
      "template": "junit-test"
    },
    {
      "name": "添加说明文档",
      "action": "create_file",
      "path": "README.md",
      "template": "project-readme"
    },
    {
      "name": "配置 Git 忽略",
      "action": "create_file",
      "path": ".gitignore",
      "template": "maven-gitignore"
    },
    {
      "name": "验证项目",
      "action": "run_command",
      "command": "mvn clean compile",
      "timeout": 120000
    }
  ],
  "validation": {
    "checkFiles": ["pom.xml", "src/main/java/App.java"],
    "runCommand": "mvn test"
  }
}

下次你说“初始化 Java 项目”我就会：
1. 先检查 Java 和 Maven
2. 如果缺失会尝试自动安装
3. 然后执行项目初始化流程！
```

**实际执行效果：**
```
您：初始化 Java 项目

Lingma: 🚀 开始执行 Skill: java-project-init

【步骤 0/8】环境预检...
  ✓ 检查 Java... ✗ 未安装
  ✓ 尝试安装 Java (choco install openjdk17)
  ✓ Java 安装完成 (17.0.9)
  
  ✓ 检查 Maven... ✗ 未安装
  ✓ 尝试安装 Maven (choco install maven)
  ✓ Maven 安装完成 (3.9.6)
  
  ✅ 环境准备完成！

【步骤 1/8】创建目录... ✓
【步骤 2/8】生成 pom.xml... ✓
【步骤 3/8】创建主类... ✓
【步骤 4/8】创建测试类... ✓
【步骤 5/8】添加说明文档... ✓
【步骤 6/8】配置 Git 忽略... ✓
【步骤 7/8】验证项目... ✓

✅ Skill 执行完成！
项目已就绪，运行以下命令开始：
  cd my-project && mvn clean install
```

```
您：我想创建一个 Skill，当我输入"初始化 Java 项目"时，执行以下步骤：
      1. 创建 Maven 目录结构
      2. 生成 pom.xml（包含 JUnit 和 Log4j）
      3. 创建主类 App.java
      4. 创建测试类 AppTest.java
      5. 添加 README.md
      6. 配置.gitignore

Lingma: ✅ 已创建 Skill: java-project-init

【记忆内容】
类别：project_introduction
标题：Java 项目初始化流程
内容：
{
  "skill": "java-project-init",
  "trigger": "初始化 Java 项目",
  "steps": [
    {
      "name": "创建目录",
      "action": "create_dirs",
      "paths": [
        "src/main/java",
        "src/main/resources",
        "src/test/java",
        "src/test/resources"
      ]
    },
    {
      "name": "生成 pom.xml",
      "action": "create_file",
      "template": "maven-pom",
      "dependencies": ["junit", "log4j"]
    },
    {
      "name": "创建主类",
      "action": "create_file",
      "path": "src/main/java/App.java",
      "template": "java-main-class"
    },
    {
      "name": "创建测试类",
      "action": "create_file",
      "path": "src/test/java/AppTest.java",
      "template": "junit-test"
    },
    {
      "name": "添加说明文档",
      "action": "create_file",
      "path": "README.md",
      "template": "project-readme"
    },
    {
      "name": "配置 Git 忽略",
      "action": "create_file",
      "path": ".gitignore",
      "template": "maven-gitignore"
    }
  ],
  "validation": {
    "checkFiles": ["pom.xml", "src/main/java/App.java"],
    "runCommand": "mvn clean compile"
  }
}

下次你说"初始化 Java 项目"我就会自动执行这个流程！
```

---

#### **示例 2：代码清理 Skill（带工具检测）**

```
您：创建一个 Skill，用于清理 Python 项目

Lingma: ✅ 已创建 Skill: python-cleanup

【记忆内容】
{
  "skill": "python-cleanup",
  "trigger": "清理 Python 项目",
  "preflightCheck": {
    "enabled": true,
    "tools": [
      {"name": "python", "version": ">=3.8"},
      {"name": "black", "optional": true},
      {"name": "flake8", "optional": true}
    ],
    "autoInstall": true
  },
  "steps": [
    {
      "name": "检查并安装工具",
      "action": "preflight_check",
      "onMissing": {
        "black": "pip install black",
        "flake8": "pip install flake8"
      }
    },
    {
      "name": "删除缓存",
      "command": "find . -type d -name __pycache__ -exec rm -rf {} +"
    },
    {
      "name": "删除编译文件",
      "command": "find . -type f -name "*.pyc" -delete"
    },
    {
      "name": "清理虚拟环境",
      "command": "rm -rf venv .venv"
    },
    {
      "name": "清除构建产物",
      "command": "rm -rf build dist *.egg-info"
    },
    {
      "name": "清理测试覆盖",
      "command": "rm -rf .coverage htmlcov"
    },
    {
      "name": "运行格式化工具",
      "command": "black .",
      "continueOnError": true
    },
    {
      "name": "运行代码检查",
      "command": "flake8 .",
      "continueOnError": true
    }
  ]
}
```

**实际执行效果：**
```
您：清理 Python 项目

Lingma: 🚀 开始执行 Skill: python-cleanup

【步骤 0/7】检查工具...
  ✓ Python 已安装 (3.11.6)
  ✗ Black 未安装 → 正在安装 (pip install black)
  ✓ Black 安装完成
  ✗ Flake8 未安装 → 正在安装 (pip install flake8)
  ✓ Flake8 安装完成

【步骤 1/7】删除缓存... ✓
【步骤 2/7】删除编译文件... ✓
【步骤 3/7】清理虚拟环境... ✓
【步骤 4/7】清除构建产物... ✓
【步骤 5/7】清理测试覆盖... ✓
【步骤 6/7】运行格式化... ✓
【步骤 7/7】运行代码检查... ✓

✅ 项目清理完成！
```

---

### **3.2 使用记忆 Skill**

#### **触发方式**

**方式 1：直接说触发词**
```
您：初始化 Java 项目

Lingma: 🚀 开始执行 Skill: java-project-init

正在执行步骤 1/6: 创建目录... ✓
正在执行步骤 2/6: 生成 pom.xml... ✓
正在执行步骤 3/6: 创建主类... ✓
正在执行步骤 4/6: 创建测试类... ✓
正在执行步骤 5/6: 添加说明文档... ✓
正在执行步骤 6/6: 配置 Git 忽略... ✓

✅ Skill 执行完成！
创建了 6 个文件，项目已就绪。
运行以下命令开始：
  cd my-project && mvn clean install
```

**方式 2：使用斜杠命令（推荐）**
```
您：/skill java-project-init

Lingma: 🎯 执行 Skill: java-project-init
...（同上）
```

**方式 3：带参数触发**
```
您：初始化 Java 项目，名称为 user-service

Lingma: 🚀 开始执行 Skill: java-project-init
        参数：项目名称 = user-service

正在创建：user-service
  ✓ src/main/java
  ✓ src/main/resources
  ...
```

---

### **3.3 管理记忆 Skill**

#### **查看所有 Skill**

```
您：我有哪些 Skill？

Lingma: 📋 你创建的 Skill 列表：

【项目类】
1. java-project-init
   触发词："初始化 Java 项目"
   步骤数：6
   
2. python-webapp-init
   触发词："创建 Python Web 应用"
   步骤数：8

【维护类】
3. python-cleanup
   触发词："清理 Python 项目"
   步骤数：7
   
4. npm-cleanup
   触发词："清理 Node 项目"
   步骤数：5

【部署类】
5. docker-deploy
   触发词："部署到 Docker"
   步骤数：10
```

#### **更新 Skill**

```
您：更新 java-project-init，添加 Lombok 支持

Lingma: ✅ 已更新 Skill: java-project-init

变更内容：
- 在 pom.xml 模板中添加 Lombok 依赖
- 在主类模板中使用 @Data 注解
- 新增可选参数：addLombok (默认 false)

现在你可以说：
"初始化 Java 项目，添加 Lombok"
```

#### **删除 Skill**

```
您：删除旧的 cleanup-old-skill

Lingma: ✅ 已删除 Skill: cleanup-old-skill

当前剩余 Skill 数量：4
```

---

## 四、方式二：用规则定义 Skill

适合团队使用和标准化流程。

### **4.1 创建规则 Skill**

#### **示例：代码审查 Skill**

```
您：设置一个 Skill 规则，当我说"审查代码"时：
      1. 检查代码风格（ESLint/Checkstyle）
      2. 分析复杂度（圈复杂度 < 10）
      3. 检查测试覆盖率（> 80%）
      4. 查找潜在 Bug
      5. 生成审查报告
      6. 提出改进建议

Lingma: ✅ 已创建规则 Skill: code-review

【规则配置】
名称：code-review-skill
触发词："审查代码"、"code review"
类型：条件规则
执行流程：
  IF 检测到 Java 文件:
    - 使用 Checkstyle
    - 使用 SpotBugs
    - 运行单元测试
  
  IF 检测到 JavaScript 文件:
    - 使用 ESLint
    - 使用 Prettier 检查格式
    - 运行 Jest 测试
  
  输出:
    - 评分（A/B/C/D）
    - 问题列表
    - 修复建议
```

---

#### **示例：CI/CD Skill**

```
您：创建一个持续集成的 Skill

Lingma: ✅ 已创建 CI/CD Skill

【规则配置】
名称：continuous-integration
触发词："运行 CI"、"持续集成"
前置条件：
  - Git 仓库已初始化
  - 有测试用例
  - 有构建脚本

执行流程：
  1. 【准备阶段】
     - 拉取最新代码
     - 安装依赖
     - 检查环境变量
  
  2. 【构建阶段】
     - 编译代码
     - 运行静态分析
     - 打包应用
  
  3. 【测试阶段】
     - 运行单元测试
     - 运行集成测试
     - 生成覆盖率报告
  
  4. 【质量门禁】
     - 测试覆盖率 > 80% ✓
     - 无严重 Bug ✓
     - 代码风格合规 ✓
  
  5. 【输出报告】
     - 构建状态
     - 测试结果
     - 质量评分
     - 改进建议
```

---

### **4.2 规则 Skill 的高级用法**

#### **条件分支**

```json
{
  "skill": "smart-build",
  "trigger": "构建项目",
  "conditions": [
    {
      "if": "file_exists('pom.xml')",
      "then": "execute('mvn clean package')"
    },
    {
      "if": "file_exists('package.json')",
      "then": "execute('npm run build')"
    },
    {
      "if": "file_exists('build.gradle')",
      "then": "execute('./gradlew build')"
    }
  ],
  "fallback": "error('未识别项目类型')"
}
```

#### **并行执行**

```json
{
  "skill": "full-test-suite",
  "parallel": [
    "run_unit_tests",
    "run_integration_tests",
    "check_code_coverage",
    "validate_api_contract"
  ],
  "sequential": [
    "generate_report",
    "notify_team"
  ]
}
```

#### **错误恢复**

```json
{
  "skill": "safe-deploy",
  "steps": [
    {"action": "backup_database"},
    {"action": "deploy_app"},
    {"action": "health_check"}
  ],
  "on_error": {
    "retry": 3,
    "rollback": true,
    "notify": ["team-lead", "devops"]
  }
}
```

---

## 五、方式三：用 MCP 开发 Skill 工具

最强大的方式，适合复杂工作流和团队共享。

### **5.1 创建 MCP Skill 服务器**

#### **项目结构**

```
skills-server/
├── package.json
├── tsconfig.json
├── src/
│   ├── index.ts              # 入口
│   ├── skills/               # Skill 定义
│   │   ├── project-init.ts   # 项目初始化
│   │   ├── code-review.ts    # 代码审查
│   │   └── deploy.ts         # 部署
│   ├── utils/                # 工具函数
│   └── types.ts              # 类型定义
└── README.md
```

---

#### **完整代码示例**

```typescript
// src/index.ts
import { FastMCP } from 'mcp';
import { z } from 'zod';
import { projectInitSkill } from './skills/project-init';
import { codeReviewSkill } from './skills/code-review';
import { deploySkill } from './skills/deploy';
import { toolInstaller } from './utils/tool-installer';

const mcp = new FastMCP('Lingma Skills');

// 注册 Skill 1：智能项目初始化（带工具检测）
mcp.tool({
  name: 'smart-project-init',
  description: '根据模板智能初始化项目（自动检测并安装所需工具）',
  inputSchema: z.object({
    projectName: z.string().describe('项目名称'),
    template: z.enum(['java-maven', 'python-flask', 'node-express']).describe('项目模板'),
    includeTests: z.boolean().default(true).describe('是否包含测试'),
    addDocker: z.boolean().default(false).describe('是否添加 Docker 配置'),
    features: z.array(z.string()).default([]).describe('额外功能'),
    autoInstallTools: z.boolean().default(true).describe('是否自动安装缺失工具')
  })
}, async (params) => {
  console.log(`🎯 初始化项目：${params.projectName}`);
  console.log(`   模板：${params.template}`);
  console.log(`   自动安装工具：${params.autoInstallTools ? '是' : '否'}`);
  
  // 步骤 1: 预检工具
  const requiredTools = getRequiredTools(params.template);
  const toolReport = await toolInstaller.checkAndInstall(requiredTools, {
    autoInstall: params.autoInstallTools,
    platform: process.platform
  });
  
  if (!toolReport.allInstalled && !params.autoInstallTools) {
    throw new Error(`缺少必需的工具：${toolReport.missing.join(', ')}`);
  }
  
  // 步骤 2: 根据模板选择不同策略
  switch (params.template) {
    case 'java-maven':
      return await projectInitSkill.initJavaProject(params);
    case 'python-flask':
      return await projectInitSkill.initFlaskProject(params);
    case 'node-express':
      return await projectInitSkill.initExpressProject(params);
  }
});

// 注册 Skill 2：深度代码审查
mcp.tool({
  name: 'deep-code-review',
  description: '深度代码审查，包含风格、复杂度、安全检查',
  inputSchema: z.object({
    filePath: z.string().describe('文件路径'),
    depth: z.enum(['quick', 'standard', 'deep']).default('standard'),
    focusAreas: z.array(z.enum(['style', 'security', 'performance', 'tests'])).default(['style'])
  })
}, async (params) => {
  const code = await readFile(params.filePath);
  
  // 多维度分析
  const analysis = {
    style: analyzeStyle(code),
    complexity: analyzeComplexity(code),
    security: analyzeSecurity(code),
    performance: analyzePerformance(code)
  };
  
  // 生成报告
  const report = generateReviewReport(analysis, params.depth);
  
  return {
    success: true,
    issues: report.issues,
    suggestions: report.suggestions,
    score: report.score
  };
});

// 注册 Skill 3：一键部署
mcp.tool({
  name: 'one-click-deploy',
  description: '安全部署到生产环境',
  inputSchema: z.object({
    environment: z.enum(['staging', 'production']).describe('部署环境'),
    skipTests: z.boolean().default(false).describe('是否跳过测试'),
    autoRollback: z.boolean().default(true).describe('失败自动回滚')
  })
}, async (params) => {
  try {
    // 前置检查
    if (!params.skipTests) {
      await runAllTests();
    }
    
    // 备份当前版本
    const backupId = await backupCurrentVersion();
    
    // 部署新版本
    await deploy(params.environment);
    
    // 健康检查
    const healthy = await healthCheck();
    
    if (!healthy && params.autoRollback) {
      // 自动回滚
      await rollback(backupId);
      throw new Error('部署失败，已自动回滚');
    }
    
    return {
      success: healthy,
      deployedAt: new Date(),
      version: getCurrentVersion()
    };
  } catch (error) {
    return {
      success: false,
      error: error.message
    };
  }
});

// 启动服务器
if (process.argv[1].endsWith('index.ts')) {
  mcp.run();
}

// 辅助函数
function getRequiredTools(template: string): string[] {
  const tools = {
    'java-maven': ['java', 'mvn'],
    'python-flask': ['python', 'pip'],
    'node-express': ['node', 'npm']
  };
  return tools[template] || [];
}

export default mcp;

```

### **工具安装器模块**

创建独立的工具安装器模块，供所有 Skill 使用：

```typescript
// src/utils/tool-installer.ts
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

export interface ToolCheckResult {
  name: string;
  installed: boolean;
  version?: string;
  installAttempted: boolean;
  installSuccess: boolean;
  error?: string;
  manualGuide?: string;
}

export interface InstallOptions {
  autoInstall: boolean;
  platform: string;
}

/**
 * 智能工具安装器
 */
class ToolInstaller {
  /**
   * 检查并安装工具
   */
  async checkAndInstall(
    tools: string[], 
    options: InstallOptions
  ): Promise<{allInstalled: boolean, missing: string[], results: ToolCheckResult[]}> {
    const results: ToolCheckResult[] = [];
    const missing: string[] = [];
    
    console.log('🔍 开始检查工具...\n');
    
    for (const tool of tools) {
      const result = await this.checkSingleTool(tool, options);
      results.push(result);
      
      if (!result.installed) {
        missing.push(tool);
      }
    }
    
    const allInstalled = missing.length === 0;
    
    if (allInstalled) {
      console.log('✅ 所有工具已就绪！\n');
    } else {
      console.log(`⚠️ 缺少 ${missing.length} 个工具：${missing.join(', ')}\n`);
    }
    
    return { allInstalled, missing, results };
  }
  
  /**
   * 检查单个工具
   */
  private async checkSingleTool(
    tool: string, 
    options: InstallOptions
  ): Promise<ToolCheckResult> {
    const result: ToolCheckResult = {
      name: tool,
      installed: false,
      installAttempted: false,
      installSuccess: false
    };
    
    // 步骤 1: 检查是否已安装
    const checkCmd = `${tool} --version`;
    try {
      const { stdout } = await execAsync(checkCmd);
      result.installed = true;
      result.version = this.extractVersion(stdout);
      console.log(`✓ ${tool} 已安装 (${result.version})`);
      return result;
    } catch (error) {
      console.log(`✗ ${tool} 未安装`);
    }
    
    // 步骤 2: 尝试自动安装
    if (options.autoInstall) {
      result.installAttempted = true;
      
      try {
        console.log(`  → 正在安装 ${tool}...`);
        const installCmd = this.getInstallCommand(tool, options.platform);
        await execAsync(installCmd, { timeout: 300000 });
        
        // 步骤 3: 验证安装
        const verifyResult = await execAsync(`${tool} --version`);
        result.installed = true;
        result.installSuccess = true;
        result.version = this.extractVersion(verifyResult.stdout);
        console.log(`  ✓ ${tool} 安装成功 (${result.version})`);
        
      } catch (error) {
        result.installSuccess = false;
        result.error = (error as Error).message;
        result.manualGuide = this.getManualGuide(tool, options.platform);
        console.log(`  ✗ ${tool} 安装失败`);
        console.log(`    手动安装指南：${result.manualGuide}`);
      }
    }
    
    return result;
  }
  
  /**
   * 获取安装命令
   */
  private getInstallCommand(tool: string, platform: string): string {
    const installers: Record<string, Record<string, string>> = {
      windows: {
        node: 'choco install nodejs-lts',
        npm: 'choco install nodejs-lts',
        python: 'choco install python',
        git: 'choco install git',
        java: 'choco install openjdk17',
        mvn: 'choco install maven',
        docker: 'choco install docker-desktop'
      },
      macos: {
        node: 'brew install node',
        npm: 'brew install node',
        python: 'brew install python',
        git: 'brew install git',
        java: 'brew install openjdk@17',
        mvn: 'brew install maven',
        docker: 'brew install --cask docker'
      },
      linux: {
        node: 'curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash - && sudo apt-get install -y nodejs',
        npm: 'curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash - && sudo apt-get install -y nodejs',
        python: 'sudo apt install python3 python3-pip',
        git: 'sudo apt install git',
        java: 'sudo apt install openjdk-17-jdk',
        mvn: 'sudo apt install maven',
        docker: 'sudo apt install docker.io'
      }
    };
    
    const platformInstallers = installers[platform] || installers.linux;
    return platformInstallers[tool] || `echo "Unknown tool: ${tool}"`;
  }
  
  /**
   * 提取版本号
   */
  private extractVersion(versionOutput: string): string {
    const match = versionOutput.match(/v?(\d+\.\d+\.\d+)/);
    return match ? match[1] : 'unknown';
  }
  
  /**
   * 获取手动安装指南
   */
  private getManualGuide(tool: string, platform: string): string {
    const guides: Record<string, string> = {
      node: 'https://nodejs.org/',
      python: 'https://www.python.org/downloads/',
      java: 'https://adoptium.net/',
      docker: 'https://www.docker.com/get-started'
    };
    
    return guides[tool] || `请搜索 "${tool} install ${platform}" 获取安装指南`;
  }
}

// 导出单例
export const toolInstaller = new ToolInstaller();
```

### **5.2 配置和使用 MCP Skill**

#### **安装依赖**

```bash
cd skills-server
npm install
npm run build
```

#### **注册到 Lingma**

```json
{
  "mcpServers": {
    "skills": {
      "command": "node",
      "args": ["/path/to/skills-server/dist/index.js"]
    }
  }
}
```

#### **使用 Skill**

```
您：/skill smart-project-init --projectName=my-app --template=java-maven --features=lombok,docker

Lingma: 🎯 执行 Skill: smart-project-init

【项目配置】
名称：my-app
模板：java-maven
功能：lombok, docker

【执行流程】
✓ 创建目录结构
✓ 生成 pom.xml（包含 Lombok）
✓ 创建主类和测试类
✓ 配置 Dockerfile
✓ 添加 Docker Compose
✓ 初始化 Git 仓库

【项目统计】
创建文件：15 个
总行数：450 行
预计时间：3 秒

项目已就绪！运行：
  cd my-app && mvn clean install
```

## 六、Skill 实战案例

### **6.1 微服务项目生成器**

```json
{
  "skill": "microservice-generator",
  "description": "生成完整的微服务项目",
  "parameters": {
    "serviceName": "用户服务",
    "database": "postgresql",
    "auth": "jwt",
    "monitoring": true
  },
  "steps": [
    {
      "name": "生成项目骨架",
      "action": "scaffold",
      "template": "spring-boot-microservice"
    },
    {
      "name": "配置数据库",
      "action": "setup-database",
      "config": {
        "type": "postgresql",
        "migrationTool": "flyway"
      }
    },
    {
      "name": "添加认证模块",
      "action": "add-auth",
      "type": "jwt",
      "provider": "spring-security"
    },
    {
      "name": "集成监控",
      "action": "add-monitoring",
      "tools": ["prometheus", "grafana", "zipkin"]
    },
    {
      "name": "生成 API 文档",
      "action": "add-swagger"
    },
    {
      "name": "创建 Docker 配置",
      "action": "add-docker"
    },
    {
      "name": "生成单元测试",
      "action": "generate-tests"
    },
    {
      "name": "编写 README",
      "action": "create-readme"
    }
  ],
  "output": {
    "structure": "标准 Maven 项目",
    "documentation": "完整的 API 文档",
    "tests": "单元测试覆盖率 > 80%"
  }
}
```

### **6.2 自动化测试 Skill**

```typescript
// automated-testing-skill.ts
mcp.tool({
  name: 'automated-testing',
  description: '自动化测试全流程',
  inputSchema: z.object({
    testTypes: z.array(z.enum(['unit', 'integration', 'e2e', 'performance'])),
    coverageThreshold: z.number().default(80),
    parallel: z.boolean().default(true)
  })
}, async (params) => {
  const results = [];
  
  // 并行执行不同类型的测试
  if (params.parallel) {
    const promises = params.testTypes.map(type => runTest(type));
    results.push(...await Promise.all(promises));
  } else {
    for (const type of params.testTypes) {
      results.push(await runTest(type));
    }
  }
  
  // 检查覆盖率
  const coverage = await checkCoverage();
  if (coverage < params.coverageThreshold) {
    throw new Error(`覆盖率不足：${coverage}% < ${params.coverageThreshold}%`);
  }
  
  // 生成报告
  return generateTestReport(results, coverage);
});
```


### **6.3 数据库迁移 Skill**

```typescript
// database-migration-skill.ts
mcp.tool({
  name: 'safe-db-migration',
  description: '安全的数据库迁移（带回滚）',
  inputSchema: z.object({
    migrationFile: z.string(),
    backupBefore: z.boolean().default(true),
    dryRun: z.boolean().default(true),
    rollbackPlan: z.boolean().default(true)
  })
}, async (params) => {
  // 1. 干跑（不实际执行）
  if (params.dryRun) {
    const dryRunResult = await migrate(params.migrationFile, { dryRun: true });
    console.log('干跑结果:', dryRunResult.sql);
  }
  
  // 2. 备份数据库
  if (params.backupBefore) {
    await backupDatabase();
  }
  
  // 3. 执行迁移
  const result = await migrate(params.migrationFile);
  
  // 4. 验证数据完整性
  const integrity = await verifyDataIntegrity();
  
  // 5. 如果失败且启用了回滚计划
  if (!result.success && params.rollbackPlan) {
    await rollback();
    throw new Error('迁移失败，已回滚');
  }
  
  return { success: integrity.valid };
});
```

## 七、Skill 最佳实践

### **7.1 设计原则**

#### **KISS 原则（Keep It Simple and Stupid）**
```
❌ 过于复杂
一个 Skill 做太多事情

✅ 恰到好处
一个 Skill 专注一类任务
```

#### **单一职责**
```
❌ 大而全
"项目初始化"包含：Java、Python、Node、前端...

✅ 小而美
- "java-project-init"
- "python-project-init"
- "node-project-init"
```

#### **可组合性**
```typescript
// 可以组合多个 Skill
const fullWorkflow = [
  'init-project',
  'setup-ci-cd',
  'configure-monitoring',
  'deploy-to-staging'
];
```

### **7.2 命名规范**

```
✅ 推荐：
- 动词开头：init-, create-, run-, deploy-
- 清晰描述：java-project-init
- 统一风格：kebab-case（短横线命名）

❌ 避免：
- 名词开头：project-, code-
- 模糊不清：do-it, process
- 混合风格：javaProjectInit, JAVA_PROJECT_INIT
```

---

### **7.3 错误处理**

```typescript
// 完善的错误处理
try {
  await executeSkill(params);
} catch (error) {
  // 1. 记录详细错误
  logger.error('Skill 执行失败', {
    skill: params.name,
    error: error.message,
    stack: error.stack
  });
  
  // 2. 尝试恢复
  if (error.recoverable) {
    await retry(params);
  }
  
  // 3. 通知用户
  notifyUser({
    type: 'error',
    message: `Skill "${params.name}" 执行失败`,
    suggestion: '请检查日志获取详细信息'
  });
  
  // 4. 回滚（如果有必要）
  if (params.autoRollback) {
    await rollback();
  }
}
```

### **7.4 性能优化**

```typescript
// 1. 缓存重复操作
const cache = new Map();
async function getCachedResult(key: string, fn: Function) {
  if (cache.has(key)) {
    return cache.get(key);
  }
  const result = await fn();
  cache.set(key, result);
  return result;
}

// 2. 并行执行独立步骤
const results = await Promise.all([
  step1(), // 互不依赖
  step2(),
  step3()
]);

// 3. 流式处理大数据
async function* processLargeDataset(data: any[]) {
  for (const item of data) {
    yield await processItem(item);
  }
}
```

## 八、Skill 模板库

### **8.1 常用 Skill 模板**

#### **模板 1：项目初始化**

```json
{
  "name": "PROJECT_INIT",
  "description": "标准化项目初始化",
  "version": "1.0.0",
  "parameters": {
    "projectName": {"type": "string", "required": true},
    "template": {"type": "string", "enum": ["basic", "web", "api"]},
    "includeTests": {"type": "boolean", "default": true}
  },
  "steps": [
    "create_directory_structure",
    "generate_config_files",
    "create_main_files",
    "setup_git",
    "run_validation"
  ],
  "validation": {
    "checkFiles": ["README.md", "package.json"],
    "runCommand": "npm install"
  }
}
```

#### **模板 2：代码质量检查**

```json
{
  "name": "CODE_QUALITY_CHECK",
  "description": "全面代码质量检查",
  "parameters": {
    "filePath": {"type": "string", "required": true},
    "checks": {
      "type": "array",
      "items": {"enum": ["lint", "format", "security", "complexity"]}
    }
  },
  "steps": [
    "analyze_syntax",
    "check_style",
    "scan_security",
    "measure_complexity",
    "generate_report"
  ],
  "output": {
    "format": "json",
    "includeSuggestions": true
  }
}
```

---

## 九、Skill 商店和分享

### **9.1 创建 Skill 包**

```json
{
  "name": "@lingma/skill-project-init",
  "version": "1.0.0",
  "description": "项目初始化 Skill 包",
  "skills": [
    "java-maven-init",
    "python-flask-init",
    "node-express-init"
  ],
  "author": "Your Name",
  "license": "MIT"
}
```

### **9.2 分享 Skill**

```
您可以将自定义 Skill 分享到：
- GitHub Gist
- 团队内部 Wiki
- Lingma 社区（规划中）

格式：
{
  "skill": "your-skill-name",
  "definition": {...},
  "examples": [...]
}
```

## 十、故障排查

### **常见问题**

#### **Q1: Skill 不执行怎么办？**

```
检查清单：
□ 触发词是否正确
□ 参数是否完整
□ 前置条件是否满足
□ 权限是否足够
□ 资源是否可用
```

#### **Q2: Skill 执行一半失败了？**

```
处理步骤：
1. 查看错误日志
2. 检查回滚机制
3. 手动清理中间状态
4. 修复问题后重试
```

#### **Q3: 如何调试 Skill？**

```typescript
// 启用调试模式
LINGMA_DEBUG=true lingma

// 查看详细执行过程
Lingma: [DEBUG] Skill: java-project-init
        [DEBUG] Step 1/6: 开始...
        [DEBUG] Step 1/6: 完成 ✓
```

## 相关资源

- [Lingma 用户指南](./lingma-user-guide.md) - 基础使用
- [Lingma 记忆与规则](./lingma-user-guide.md#42-使用记忆功能) - 记忆和规则配置
- [MCP 开发指南](../mcp/mcp-development-guide.md) - MCP 工具开发
- [VS Code API 指南](./vscode-api-guide.md) - VS Code 接口参考

---

**文档版本:** 1.0  
**最后更新:** 2026-03-07  
**维护团队:** One AI Team  
**反馈建议:** support@lingma.ai
