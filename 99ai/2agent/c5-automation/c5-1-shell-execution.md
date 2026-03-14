# c5-1 Shell 命令执行与安全沙箱

## 1. 概述

Shell 命令执行是 AI智能体与操作系统交互的核心能力，但也带来严重的安全风险。本章详解如何在安全性和功能性之间取得平衡。

## 2. 基础实现

### 2.1 命令执行器

```javascript
const { exec, spawn } = require('child_process');
const util = require('util');
const execAsync = util.promisify(exec);

class ShellExecutor {
  constructor(options = {}) {
    this.defaultTimeout = options.timeout || 30000; // 30 秒
    this.workingDir = options.workingDir || process.cwd();
    this.allowedCommands = options.allowedCommands || [];
    this.blockedCommands = options.blockedCommands || [];
  }
  
  async execute(command, options = {}) {
    // 安全检查
    this.validateCommand(command);
    
    const timeout = options.timeout || this.defaultTimeout;
    const cwd = options.cwd || this.workingDir;
    
    try {
      const { stdout, stderr } = await execAsync(command, {
        cwd,
        timeout,
        maxBuffer: 10 * 1024 * 1024, // 10MB
        env: { ...process.env, ...options.env }
      });
      
      return {
        success: true,
        stdout,
        stderr,
        command,
        executionTime: Date.now()
      };
      
    } catch (error) {
      return {
        success: false,
        error: error.message,
        stdout: error.stdout || '',
        stderr: error.stderr || '',
        code: error.code,
        signal: error.signal
      };
    }
  }
  
  validateCommand(command) {
    // 检查黑名单
    for (const blocked of this.blockedCommands) {
      if (command.includes(blocked)) {
        throw new Error(`Command blocked: ${blocked} is not allowed`);
      }
    }
    
    // 如果有白名单，检查是否在白名单内
    if (this.allowedCommands.length > 0) {
      const isAllowed = this.allowedCommands.some(allowed => 
        command.startsWith(allowed)
      );
      
      if (!isAllowed) {
        throw new Error(`Command not in whitelist: ${command}`);
      }
    }
    
    // 检查危险模式
    const dangerousPatterns = [
      /\brm\s+-rf\s+\//,      // rm -rf /
      /\bchmod\s+-R\s+777/,   // chmod -R 777
      />\s*\/dev\/sd\w/,      // > /dev/sdX
      /\bmkfs\./,             // mkfs.*
      /:\(\)\{\s*:\|:&\s*\};:/ // Fork bomb
    ];
    
    for (const pattern of dangerousPatterns) {
      if (pattern.test(command)) {
        throw new Error(`Dangerous command pattern detected: ${command}`);
      }
    }
  }
}

// 使用示例
const executor = new ShellExecutor({
  blockedCommands: ['rm -rf', 'sudo', 'su ', 'passwd'],
  allowedCommands: ['ls', 'cat', 'grep', 'find', 'git']
});

const result = await executor.execute('ls -la');
console.log(result.stdout);
```

### 2.2 流式输出

```javascript
class StreamingShellExecutor {
  async executeStream(command, callbacks) {
    return new Promise((resolve, reject) => {
      const child = spawn(command, {
        shell: true,
        stdio: ['ignore', 'pipe', 'pipe']
      });
      
      let stdout = '';
      let stderr = '';
      
      child.stdout.on('data', (data) => {
        const text = data.toString();
        stdout += text;
        callbacks.onStdout?.(text);
      });
      
      child.stderr.on('data', (data) => {
        const text = data.toString();
        stderr += text;
        callbacks.onStderr?.(text);
      });
      
      child.on('close', (code) => {
        resolve({
          success: code === 0,
          exitCode: code,
          stdout,
          stderr
        });
      });
      
      child.on('error', reject);
      
      // 超时处理
      if (callbacks.timeout) {
        setTimeout(() => {
          child.kill('SIGKILL');
          reject(new Error('Execution timeout'));
        }, callbacks.timeout);
      }
    });
  }
}

// 使用示例
const streamingExecutor = new StreamingShellExecutor();

await streamingExecutor.executeStream('ping -c 10 google.com', {
  onStdout: (data) => {
    process.stdout.write(data); // 实时输出
  },
  onStderr: (data) => {
    console.error(data);
  },
  timeout: 30000
});
```

## 3. 安全沙箱

### 3.1 参数转义

```javascript
class CommandSanitizer {
  // 转义 shell 特殊字符
  static escapeArg(arg) {
    if (typeof arg !== 'string') {
      arg = String(arg);
    }
    
    // Windows
    if (process.platform === 'win32') {
      return `"${arg.replace(/"/g, '\\"')}"`;
    }
    
    // Unix/Linux/Mac
    return `'${arg.replace(/'/g, "'\\''")}'`;
  }
  
  // 构建安全的命令
  static buildCommand(baseCommand, args = []) {
    const escapedArgs = args.map(arg => this.escapeArg(arg));
    return `${baseCommand} ${escapedArgs.join(' ')}`;
  }
  
  // 验证文件路径
  static validateFilePath(path) {
    const normalized = path.normalize(path);
    
    // 防止路径遍历攻击
    if (normalized.includes('..') && !normalized.startsWith('/')) {
      throw new Error('Invalid path: directory traversal detected');
    }
    
    // 防止访问敏感目录
    const sensitivePaths = [
      '/etc', '/root', '/boot', '/dev',
      'C:\\Windows', 'C:\\Program Files'
    ];
    
    for (const sensitive of sensitivePaths) {
      if (normalized.startsWith(sensitive)) {
        throw new Error(`Access to ${sensitive} is forbidden`);
      }
    }
    
    return normalized;
  }
}

// 使用示例
const safeCommand = CommandSanitizer.buildCommand('ls', [
  '-la',
  '/home/user/my documents',  // 包含空格
  "file'name.txt"             // 包含单引号
]);

console.log(safeCommand);
// 输出：ls -la '/home/user/my documents' 'file'\''name.txt'
```

### 3.2 资源限制

```javascript
const os = require('os');

class ResourceLimiter {
  constructor(limits = {}) {
    this.limits = {
      cpuPercent: limits.cpu || 50,      // CPU 使用率上限 50%
      memoryMB: limits.memory || 512,    // 内存上限 512MB
      diskIO: limits.diskIO || 100,      // 磁盘 IO 上限 (MB/s)
      network: limits.network || false,  // 禁用网络
      ...limits
    };
  }
  
  async executeWithLimits(command, limitsOverride = {}) {
    const limits = { ...this.limits, ...limitsOverride };
    
    // 创建受限的子进程
    const child = spawn(command, {
      shell: true,
      stdio: ['ignore', 'pipe', 'pipe'],
      env: {
        ...process.env,
        RESOURCE_LIMITS: JSON.stringify(limits)
      }
    });
    
    // 监控资源使用
    const monitorInterval = setInterval(() => {
      this.monitorProcess(child.pid, limits);
    }, 1000);
    
    return new Promise((resolve, reject) => {
      let stdout = '';
      let stderr = '';
      
      child.stdout.on('data', (data) => stdout += data);
      child.stderr.on('data', (data) => stderr += data);
      
      child.on('close', (code) => {
        clearInterval(monitorInterval);
        resolve({
          success: code === 0,
          stdout,
          stderr,
          pid: child.pid
        });
      });
      
      child.on('error', (error) => {
        clearInterval(monitorInterval);
        reject(error);
      });
    });
  }
  
  monitorProcess(pid, limits) {
    // 这里应该使用系统工具（如 ps、top）来监控资源
    // 简化示例
    
    if (limits.network) {
      // 阻止网络连接（需要 root 权限或使用防火墙）
      // 在实际应用中，应该使用 iptables 或类似工具
    }
  }
}

// 使用示例
const limiter = new ResourceLimiter({
  cpu: 30,
  memory: 256,
  network: true  // 禁用网络
});

await limiter.executeWithLimits('python3 heavy_computation.py');
```

### 3.3 Docker 容器隔离

```javascript
const Docker = require('dockerode');

class DockerSandbox {
  constructor(dockerOptions = {}) {
    this.docker = new Docker(dockerOptions);
    this.defaultConfig = {
      Image: 'node:18-alpine',
      Cmd: ['sh', '-c'],
      HostConfig: {
        AutoRemove: true,
        Memory: 512 * 1024 * 1024, // 512MB
        NanoCpus: 500000000,       // 0.5 CPU
        NetworkMode: 'none',       // 禁用网络
        PidsLimit: 50              // 进程数限制
      }
    };
  }
  
  async executeInContainer(command, options = {}) {
    const config = {
      ...this.defaultConfig,
      ...options,
      Cmd: [...this.defaultConfig.Cmd, command]
    };
    
    // 创建容器
    const container = await this.docker.createContainer(config);
    
    try {
      // 启动容器
      await container.start();
      
      // 等待执行完成
      const result = await container.wait();
      
      // 获取日志
      const logs = await container.logs({
        stdout: true,
        stderr: true
      });
      
      return {
        success: result.StatusCode === 0,
        exitCode: result.StatusCode,
        output: logs.toString()
      };
      
    } finally {
      // 确保容器被删除
      try {
        await container.remove({ force: true });
      } catch (e) {
        console.error('Failed to remove container:', e);
      }
    }
  }
}

// 使用示例
const sandbox = new DockerSandbox();

// 在隔离环境中执行不受信任的代码
const result = await sandbox.executeInContainer(`
  npm install express && 
  node server.js
`, {
  HostConfig: {
    ...this.defaultConfig.HostConfig,
    Binds: ['/tmp/code:/app']  // 挂载代码目录
  },
  WorkingDir: '/app'
});

console.log(result.output);
```

## 4. 审计与日志

### 4.1 命令审计

```javascript
const fs = require('fs').promises;

class CommandAuditor {
  constructor(logFile = './logs/command-audit.log') {
    this.logFile = logFile;
    this.enabled = true;
  }
  
  async log(execution) {
    if (!this.enabled) return;
    
    const auditEntry = {
      timestamp: new Date().toISOString(),
      command: execution.command,
      user: execution.user || 'system',
      sessionId: execution.sessionId,
      success: execution.success,
      duration: execution.duration,
      pid: execution.pid,
      cwd: execution.cwd,
      riskLevel: this.assessRisk(execution.command)
    };
    
    // 异步写入日志
    await fs.appendFile(
      this.logFile,
      JSON.stringify(auditEntry) + '\n',
      'utf-8'
    );
    
    // 高风险操作实时告警
    if (auditEntry.riskLevel === 'high') {
      this.sendAlert(auditEntry);
    }
  }
  
  assessRisk(command) {
    const highRiskPatterns = [
      /\bsudo\b/,
      /\brm\s+-rf\b/,
      /\bchmod\b/,
      /\bchown\b/,
      />/,  // 重定向
      /\|/   // 管道
    ];
    
    for (const pattern of highRiskPatterns) {
      if (pattern.test(command)) {
        return 'high';
      }
    }
    
    return 'low';
  }
  
  async sendAlert(entry) {
    console.warn('⚠️ HIGH RISK COMMAND EXECUTED:', entry);
    // 可以集成到告警系统（邮件、Slack 等）
  }
  
  // 生成审计报告
  async generateReport(startTime, endTime) {
    const logContent = await fs.readFile(this.logFile, 'utf-8');
    const entries = logContent.split('\n')
      .filter(line => line.trim())
      .map(line => JSON.parse(line));
    
    const filtered = entries.filter(e => {
      const time = new Date(e.timestamp);
      return time >= startTime && time <= endTime;
    });
    
    return {
      totalExecutions: filtered.length,
      successfulExecutions: filtered.filter(e => e.success).length,
      highRiskCommands: filtered.filter(e => e.riskLevel === 'high').length,
      uniqueCommands: new Set(filtered.map(e => e.command)).size,
      topUsers: this.getTopUsers(filtered),
      timeline: this.getTimeline(filtered)
    };
  }
  
  getTopUsers(entries) {
    const userCount = {};
    entries.forEach(e => {
      userCount[e.user] = (userCount[e.user] || 0) + 1;
    });
    
    return Object.entries(userCount)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10);
  }
}

// 集成到执行器
class AuditedShellExecutor extends ShellExecutor {
  constructor(options) {
    super(options);
    this.auditor = new CommandAuditor(options.logFile);
  }
  
  async execute(command, options = {}) {
    const startTime = Date.now();
    
    try {
      const result = await super.execute(command, options);
      
      // 记录审计日志
      await this.auditor.log({
        command,
        ...result,
        duration: Date.now() - startTime,
        user: options.user,
        sessionId: options.sessionId,
        cwd: options.cwd
      });
      
      return result;
      
    } catch (error) {
      await this.auditor.log({
        command,
        success: false,
        error: error.message,
        duration: Date.now() - startTime,
        user: options.user,
        sessionId: options.sessionId
      });
      
      throw error;
    }
  }
}
```

## 5. 最佳实践清单

✅ **DO - 推荐做法：**

1. **始终使用白名单机制**
   ```javascript
   const executor = new ShellExecutor({
     allowedCommands: ['ls', 'cat', 'grep', 'find']
   });
   ```

2. **转义所有用户输入**
   ```javascript
   const safePath = CommandSanitizer.escapeArg(userInput);
   const command = `cat ${safePath}`;
   ```

3. **设置合理的超时时间**
   ```javascript
   await executor.execute(command, { timeout: 5000 });
   ```

4. **记录所有执行的命令**
   ```javascript
   const auditor = new CommandAuditor();
   await auditor.log(execution);
   ```

5. **对高风险操作二次确认**
   ```javascript
   if (riskLevel === 'high') {
     await requestUserConfirmation();
   }
   ```

❌ **DON'T - 避免做法：**

1. ❌ 不要直接拼接用户输入到命令中
2. ❌ 不要以 root 身份运行 AI助手
3. ❌ 不要忽略错误和异常
4. ❌ 不要允许无限制的命令执行
5. ❌ 不要忘记定期清理审计日志

---

**下一节：** [c5-2 文件系统操作](./c5-2-file-system-ops.md)  
**上一节：** [c5 自动化执行模块](./README.md)
