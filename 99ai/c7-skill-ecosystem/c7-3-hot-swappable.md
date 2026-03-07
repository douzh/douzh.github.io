# c7-3 热插拔机制

## 1. 概述

热插拔（Hot-swappable）允许在不重启系统的情况下动态加载和卸载技能。本章详解动态模块加载、沙箱隔离和优雅降级策略。

## 2. 动态模块加载

### 2.1 ES Modules 动态导入

```typescript
class DynamicModuleLoader {
  private loadedModules: Map<string, any>;
  private modulePaths: Map<string, string>;
  
  constructor() {
    this.loadedModules = new Map();
    this.modulePaths = new Map();
  }
  
  // 动态加载模块
  async load(skillName: string, modulePath: string): Promise<any> {
    // 检查是否已加载
    if (this.loadedModules.has(skillName)) {
      console.log(`Module ${skillName} already loaded, skipping`);
      return this.loadedModules.get(skillName);
    }
    
    try {
      // 使用动态 import()
      const module = await import(modulePath);
      
      // 验证模块接口
      this.validateModule(module, skillName);
      
      // 缓存
      this.loadedModules.set(skillName, module);
      this.modulePaths.set(skillName, modulePath);
      
      console.log(`✅ Loaded module: ${skillName}`);
      
      // 调用生命周期钩子
      if (module.onLoad) {
        await module.onLoad();
      }
      
      return module;
      
    } catch (error) {
      console.error(`❌ Failed to load module ${skillName}:`, error);
      throw error;
    }
  }
  
  // 卸载模块
  async unload(skillName: string): Promise<void> {
    const module = this.loadedModules.get(skillName);
    
    if (!module) {
      throw new Error(`Module ${skillName} not loaded`);
    }
    
    try {
      // 调用卸载钩子
      if (module.onUnload) {
        await module.onUnload();
      }
      
      // 清理缓存
      this.loadedModules.delete(skillName);
      this.modulePaths.delete(skillName);
      
      // Node.js 中清除 require 缓存
      delete require.cache[require.resolve(skillName)];
      
      console.log(`✅ Unloaded module: ${skillName}`);
      
    } catch (error) {
      console.error(`❌ Failed to unload module ${skillName}:`, error);
      throw error;
    }
  }
  
  // 热更新模块
  async reload(skillName: string): Promise<any> {
    await this.unload(skillName);
    
    // 短暂延迟确保缓存清理
    await new Promise(resolve => setTimeout(resolve, 100));
    
    const modulePath = this.modulePaths.get(skillName);
    if (!modulePath) {
      throw new Error(`Module path not found for ${skillName}`);
    }
    
    // 添加时间戳避免缓存
    const timestampedPath = `${modulePath}?t=${Date.now()}`;
    return await this.load(skillName, timestampedPath);
  }
  
  // 获取已加载的模块
  get(skillName: string): any {
    return this.loadedModules.get(skillName);
  }
  
  // 列出所有已加载的模块
  list(): string[] {
    return Array.from(this.loadedModules.keys());
  }
  
  private validateModule(module: any, skillName: string) {
    // 检查必需的方法
    if (!module.execute || typeof module.execute !== 'function') {
      throw new Error(`Module ${skillName} must export an 'execute' function`);
    }
  }
}

// 使用示例
const loader = new DynamicModuleLoader();

// 加载技能
await loader.load('weather-skill', './skills/weather.js');

// 使用技能
const weatherModule = loader.get('weather-skill');
const result = await weatherModule.execute({ location: '北京' });

// 热更新
await loader.reload('weather-skill');

// 卸载
await loader.unload('weather-skill');
```

### 2.2 CommonJS 支持

```typescript
import { createRequire } from 'module';
import path from 'path';

class CommonJSModuleLoader {
  private requireCache: Map<string, NodeRequire>;
  
  constructor() {
    this.requireCache = new Map();
  }
  
  async load(skillName: string, modulePath: string): Promise<any> {
    // 为每个模块创建独立的 require
    const resolvedPath = path.resolve(modulePath);
    const moduleDir = path.dirname(resolvedPath);
    
    const moduleRequire = createRequire(moduleDir);
    
    // 清除旧缓存（如果是重新加载）
    this.clearCache(resolvedPath);
    
    // 同步加载（CommonJS 特性）
    const module = moduleRequire(resolvedPath);
    
    this.requireCache.set(skillName, moduleRequire);
    
    console.log(`✅ Loaded CommonJS module: ${skillName}`);
    
    return module;
  }
  
  private clearCache(modulePath: string) {
    // 清除主模块缓存
    delete require.cache[require.resolve(modulePath)];
    
    // 清除子依赖缓存
    for (const key in require.cache) {
      if (key.includes(modulePath)) {
        delete require.cache[key];
      }
    }
  }
  
  unload(skillName: string) {
    this.requireCache.delete(skillName);
    console.log(`✅ Unloaded CommonJS module: ${skillName}`);
  }
}
```

## 3. 沙箱隔离

### 3.1 VM 沙箱

```typescript
import vm from 'vm';
import fs from 'fs';

class VMSandbox {
  private contexts: Map<string, vm.Context>;
  
  constructor() {
    this.contexts = new Map();
  }
  
  // 创建隔离上下文
  createContext(skillName: string, globals: any = {}) {
    const context = vm.createContext({
      ...globals,
      console: this.createSafeConsole(skillName),
      setTimeout,
      setInterval,
      clearTimeout,
      clearInterval,
      Buffer,
      __skillName: skillName
    });
    
    this.contexts.set(skillName, context);
    return context;
  }
  
  // 执行沙箱代码
  executeInSandbox(skillName: string, code: string, args?: any): any {
    const context = this.contexts.get(skillName);
    
    if (!context) {
      throw new Error(`Sandbox for ${skillName} not found`);
    }
    
    const wrappedCode = `
      (async function() {
        try {
          ${code}
        } catch (error) {
          throw new Error(\`Skill execution error: \${error.message}\`);
        }
      })();
    `;
    
    return vm.runInContext(wrappedCode, context, {
      timeout: 5000, // 5 秒超时
      displayErrors: true
    });
  }
  
  // 从文件加载并执行
  async loadAndExecute(skillName: string, filePath: string, args?: any) {
    const code = await fs.promises.readFile(filePath, 'utf-8');
    
    // 创建上下文
    this.createContext(skillName, {
      input: args,
      output: null
    });
    
    return await this.executeInSandbox(skillName, code, args);
  }
  
  // 销毁沙箱
  destroySandbox(skillName: string) {
    this.contexts.delete(skillName);
    console.log(`🗑️ Destroyed sandbox: ${skillName}`);
  }
  
  private createSafeConsole(skillName: string) {
    return {
      log: (...args: any[]) => {
        console.log(`[${skillName}]`, ...args);
      },
      error: (...args: any[]) => {
        console.error(`[${skillName}]`, ...args);
      },
      warn: (...args: any[]) => {
        console.warn(`[${skillName}]`, ...args);
      }
    };
  }
}

// 使用示例
const sandbox = new VMSandbox();

const skillCode = `
  const result = input.location.toUpperCase();
  output = { result };
`;

await sandbox.loadAndExecute(
  'test-skill',
  './skills/test.js',
  { location: '北京' }
);
```

### 3.2 子进程隔离

```typescript
import { fork, ChildProcess } from 'child_process';

class ProcessSandbox {
  private processes: Map<string, ChildProcess>;
  
  constructor() {
    this.processes = new Map();
  }
  
  // 启动工作进程
  spawnWorker(skillName: string, workerScript: string): Promise<void> {
    return new Promise((resolve, reject) => {
      const child = fork(workerScript, [], {
        stdio: ['pipe', 'pipe', 'pipe', 'ipc'],
        env: {
          ...process.env,
          SKILL_NAME: skillName
        },
        execArgv: ['--max-old-space-size=128'] // 内存限制 128MB
      });
      
      child.on('spawn', () => {
        this.processes.set(skillName, child);
        console.log(`✅ Spawned worker: ${skillName} (PID: ${child.pid})`);
        resolve();
      });
      
      child.on('error', (error) => {
        console.error(`❌ Worker error: ${skillName}`, error);
        reject(error);
      });
      
      child.on('exit', (code, signal) => {
        console.log(`🛑 Worker exited: ${skillName} (code: ${code}, signal: ${signal})`);
        this.processes.delete(skillName);
      });
    });
  }
  
  // 发送消息到工作进程
  sendToWorker(skillName: string, message: any): Promise<any> {
    return new Promise((resolve, reject) => {
      const child = this.processes.get(skillName);
      
      if (!child) {
        return reject(new Error(`Worker ${skillName} not found`));
      }
      
      const messageId = `${skillName}_${Date.now()}`;
      
      const handler = (response: any) => {
        if (response.messageId === messageId) {
          child.off('message', handler);
          resolve(response.data);
        }
      };
      
      child.on('message', handler);
      
      child.send({ messageId, data: message });
      
      // 超时处理
      setTimeout(() => {
        child.off('message', handler);
        reject(new Error('Request timeout'));
      }, 5000);
    });
  }
  
  // 停止工作进程
  async stopWorker(skillName: string): Promise<void> {
    const child = this.processes.get(skillName);
    
    if (!child) return;
    
    return new Promise((resolve) => {
      child.once('exit', () => {
        console.log(`🛑 Stopped worker: ${skillName}`);
        resolve();
      });
      
      // 优雅关闭
      child.send({ type: 'SHUTDOWN' });
      
      // 强制终止（5 秒后）
      setTimeout(() => {
        if (child.connected) {
          child.kill('SIGKILL');
        }
      }, 5000);
    });
  }
  
  // 停止所有
  async stopAll() {
    const promises = Array.from(this.processes.keys()).map(name => 
      this.stopWorker(name)
    );
    
    await Promise.all(promises);
    console.log('🛑 All workers stopped');
  }
}

// worker-script.js 示例
process.on('message', (message) => {
  if (message.type === 'SHUTDOWN') {
    process.exit(0);
  }
  
  // 处理请求
  const result = executeSkill(message.data);
  
  process.send({
    messageId: message.messageId,
    data: result
  });
});

function executeSkill(data: any) {
  // 实际技能逻辑
  return { success: true, result: data };
}
```

## 4. 优雅降级

### 4.1 失败恢复策略

```typescript
class ResilientSkillExecutor {
  private fallbacks: Map<string, FallbackHandler>;
  private retryConfig: Map<string, RetryConfig>;
  
  constructor() {
    this.fallbacks = new Map();
    this.retryConfig = new Map();
  }
  
  // 注册降级策略
  registerFallback(skillName: string, handler: FallbackHandler) {
    this.fallbacks.set(skillName, handler);
  }
  
  // 执行技能（带重试和降级）
  async execute(skillName: string, input: any): Promise<any> {
    const config = this.retryConfig.get(skillName) || {
      maxRetries: 3,
      delay: 1000,
      backoff: 2
    };
    
    let lastError: Error;
    
    for (let attempt = 0; attempt <= config.maxRetries; attempt++) {
      try {
        const skill = await this.loadSkill(skillName);
        return await skill.execute(input);
        
      } catch (error) {
        lastError = error as Error;
        console.error(`Attempt ${attempt + 1} failed for ${skillName}:`, error);
        
        if (attempt < config.maxRetries) {
          const delay = config.delay * Math.pow(config.backoff, attempt);
          await this.sleep(delay);
        }
      }
    }
    
    // 所有重试失败，执行降级策略
    console.warn(`⚠️ Skill ${skillName} failed after ${config.maxRetries} retries, using fallback`);
    return await this.executeFallback(skillName, input, lastError!);
  }
  
  private async executeFallback(skillName: string, input: any, error: Error) {
    const fallback = this.fallbacks.get(skillName);
    
    if (!fallback) {
      throw new Error(`No fallback registered for ${skillName}: ${error.message}`);
    }
    
    try {
      return await fallback(input, error);
    } catch (fallbackError) {
      throw new Error(`Fallback also failed: ${fallbackError.message}`);
    }
  }
  
  private async loadSkill(skillName: string) {
    // 实际加载逻辑
    return {
      execute: async (input: any) => {
        // 模拟实现
        return { result: input };
      }
    };
  }
  
  private sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// 使用示例
const executor = new ResilientSkillExecutor();

// 注册天气技能的降级策略
executor.registerFallback('get_weather', async (input, error) => {
  // 返回缓存的天气数据
  return {
    cached: true,
    data: {
      temp: 20,
      condition: '数据暂时不可用'
    }
  };
});

// 执行（自动重试和降级）
const result = await executor.execute('get_weather', {
  location: '北京'
});
```

---

**下一节：** [c7 其他主题](./c7-other-topics.md)  
**上一节：** [c7-2 技能注册与发现](./c7-2-skill-registry.md)
