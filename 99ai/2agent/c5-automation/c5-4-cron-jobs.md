# c5-4 Cron Jobs 定时任务实现

## 1. 概述

定时任务让 AI智能体能够在特定时间或周期性地执行任务，如数据备份、报告生成、健康检查等。

## 2. Cron 表达式基础

### 2.1 Cron 格式说明

```
* * * * * *
│ │ │ │ │ │
│ │ │ │ │ └─ 秒 (可选，0-59)
│ │ │ │ └─── 分 (0-59)
│ │ │ └───── 时 (0-23)
│ │ └─────── 日 (1-31)
│ └───────── 月 (1-12)
└─────────── 星期 (0-7, 0 和 7 都代表周日)
```

**特殊字符：**
- `*` - 每个值（每分钟）
- `,` - 分隔多个值（1,3,5）
- `-` - 范围（1-5）
- `/` - 步长（*/5 表示每 5 个单位）

### 2.2 常用示例

```javascript
const cronExamples = {
  '每小时': '0 * * * *',
  '每天午夜': '0 0 * * *',
  '每周一上午 9 点': '0 9 * * 1',
  '每月 1 号': '0 0 1 * *',
  '每 5 分钟': '*/5 * * * *',
  '工作日每半小时': '*/30 9-18 * * 1-5',
  '每年元旦': '0 0 1 1 *'
};
```

## 3. Node-cron 实战

### 3.1 基础调度器

```javascript
const cron = require('node-cron');

class TaskScheduler {
  constructor() {
    this.scheduledTasks = new Map();
    this.timezone = 'Asia/Shanghai';
  }
  
  // 添加定时任务
  schedule(name, cronExpression, task, options = {}) {
    if (this.scheduledTasks.has(name)) {
      throw new Error(`Task "${name}" already exists`);
    }
    
    const scheduledTask = cron.schedule(cronExpression, async () => {
      try {
        console.log(`[${new Date().toISOString()}] Executing task: ${name}`);
        await task();
        console.log(`✅ Task completed: ${name}`);
      } catch (error) {
        console.error(`❌ Task failed: ${name}`, error);
        
        if (options.onError) {
          await options.onError(error);
        }
      }
    }, {
      timezone: options.timezone || this.timezone,
      scheduled: options.scheduled !== false
    });
    
    const taskInfo = {
      name,
      cronExpression,
      task: scheduledTask,
      createdAt: Date.now(),
      lastExecution: null,
      nextExecution: this.getNextExecution(cronExpression),
      executionCount: 0,
      options
    };
    
    this.scheduledTasks.set(name, taskInfo);
    console.log(`✅ Scheduled task: ${name} (${cronExpression})`);
    
    return taskInfo;
  }
  
  // 获取下次执行时间
  getNextExecution(cronExpression) {
    // 简化的计算，实际可用 cron-parser 库
    const date = new Date();
    date.setMinutes(date.getMinutes() + 5);
    return date;
  }
  
  // 暂停任务
  pause(name) {
    const taskInfo = this.scheduledTasks.get(name);
    
    if (!taskInfo) {
      throw new Error(`Task "${name}" not found`);
    }
    
    taskInfo.task.stop();
    console.log(`⏸️ Paused task: ${name}`);
  }
  
  // 恢复任务
  resume(name) {
    const taskInfo = this.scheduledTasks.get(name);
    
    if (!taskInfo) {
      throw new Error(`Task "${name}" not found`);
    }
    
    taskInfo.task.start();
    console.log(`▶️ Resumed task: ${name}`);
  }
  
  // 取消任务
  cancel(name) {
    const taskInfo = this.scheduledTasks.get(name);
    
    if (!taskInfo) {
      throw new Error(`Task "${name}" not found`);
    }
    
    taskInfo.task.destroy();
    this.scheduledTasks.delete(name);
    console.log(`🗑️ Cancelled task: ${name}`);
  }
  
  // 列出所有任务
  listTasks() {
    return Array.from(this.scheduledTasks.values()).map(info => ({
      name: info.name,
      cronExpression: info.cronExpression,
      lastExecution: info.lastExecution,
      nextExecution: info.nextExecution,
      executionCount: info.executionCount,
      status: info.task.getStatus ? (info.task.getStatus() ? 'running' : 'stopped') : 'unknown'
    }));
  }
  
  // 立即执行任务
  async executeNow(name) {
    const taskInfo = this.scheduledTasks.get(name);
    
    if (!taskInfo) {
      throw new Error(`Task "${name}" not found`);
    }
    
    console.log(`⚡ Manually executing task: ${name}`);
    await taskInfo.options.manualExecution?.() || taskInfo.task.callback?.();
  }
}

// 使用示例
const scheduler = new TaskScheduler();

// 每天凌晨 2 点备份数据库
scheduler.schedule('daily-backup', '0 2 * * *', async () => {
  console.log('Backing up database...');
  await backupDatabase();
  console.log('Backup completed');
});

// 每 5 分钟检查健康状态
scheduler.schedule('health-check', '*/5 * * * *', async () => {
  const health = await checkHealth();
  if (!health.healthy) {
    await sendAlert('Service unhealthy!');
  }
});

// 每周一上午 9 点生成周报
scheduler.schedule('weekly-report', '0 9 * * 1', async () => {
  await generateWeeklyReport();
});

// 查看任务列表
console.log(scheduler.listTasks());
```

### 3.2 带重试机制的任务

```javascript
class ResilientScheduler extends TaskScheduler {
  scheduleWithRetry(name, cronExpression, task, options = {}) {
    const retryOptions = {
      maxRetries: options.maxRetries || 3,
      retryDelay: options.retryDelay || 60000, // 1 分钟
      backoffMultiplier: options.backoffMultiplier || 2,
      ...options
    };
    
    return this.schedule(name, cronExpression, async () => {
      let lastError;
      
      for (let attempt = 1; attempt <= retryOptions.maxRetries; attempt++) {
        try {
          await task();
          return; // 成功则返回
          
        } catch (error) {
          lastError = error;
          console.error(
            `[${name}] Attempt ${attempt}/${retryOptions.maxRetries} failed:`,
            error.message
          );
          
          if (attempt < retryOptions.maxRetries) {
            const delay = retryOptions.retryDelay * 
                         Math.pow(retryOptions.backoffMultiplier, attempt - 1);
            
            console.log(`[${name}] Retrying in ${delay/1000}s...`);
            await this.sleep(delay);
          }
        }
      }
      
      // 所有重试失败
      console.error(`[${name}] All attempts failed. Last error:`, lastError.message);
      
      if (options.onFinalFailure) {
        await options.onFinalFailure(lastError);
      }
    }, options);
  }
  
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// 使用示例
const resilientScheduler = new ResilientScheduler();

resilientScheduler.scheduleWithRetry(
  'api-sync',
  '0 */6 * * *', // 每 6 小时
  async () => {
    await syncWithExternalAPI();
  },
  {
    maxRetries: 5,
    retryDelay: 30000, // 30 秒
    onFinalFailure: async (error) => {
      await sendCriticalAlert(`API sync failed: ${error.message}`);
    }
  }
);
```

## 4. 高级功能

### 4.1 任务依赖管理

```javascript
class DependentScheduler extends TaskScheduler {
  constructor() {
    super();
    this.dependencies = new Map(); // taskName -> [dependencyNames]
    this.completedTasks = new Set();
  }
  
  // 添加带依赖的任务
  scheduleWithDependencies(name, cronExpression, task, dependencies = []) {
    this.dependencies.set(name, dependencies);
    
    return this.schedule(name, cronExpression, async () => {
      // 等待依赖完成
      await this.waitForDependencies(name);
      
      // 执行任务
      await task();
      
      // 标记为完成
      this.completedTasks.add(name);
    });
  }
  
  async waitForDependencies(taskName) {
    const deps = this.dependencies.get(taskName) || [];
    
    while (true) {
      const allCompleted = deps.every(dep => this.completedTasks.has(dep));
      
      if (allCompleted) {
        break;
      }
      
      console.log(`[${taskName}] Waiting for dependencies:`, 
                  deps.filter(d => !this.completedTasks.has(d)));
      
      await this.sleep(5000); // 每 5 秒检查一次
    }
  }
  
  resetDaily() {
    // 每天重置完成状态
    this.completedTasks.clear();
  }
}

// 使用示例
const depScheduler = new DependentScheduler();

// 定义任务链
depScheduler.scheduleWithDependencies(
  'extract-data',
  '0 1 * * *', // 凌晨 1 点
  extractData
);

depScheduler.scheduleWithDependencies(
  'transform-data',
  '0 2 * * *', // 凌晨 2 点
  transformData,
  ['extract-data'] // 依赖数据抽取完成
);

depScheduler.scheduleWithDependencies(
  'load-data',
  '0 3 * * *', // 凌晨 3 点
  loadData,
  ['transform-data'] // 依赖数据转换完成
);
```

### 4.2 分布式任务锁

```javascript
const Redis = require('ioredis');

class DistributedScheduler extends TaskScheduler {
  constructor(redisConfig) {
    super();
    this.redis = new Redis(redisConfig);
    this.instanceId = `${require('os').hostname()}-${process.pid}`;
  }
  
  async scheduleDistributed(name, cronExpression, task, options = {}) {
    const lockKey = `lock:${name}`;
    const lockTimeout = options.lockTimeout || 300000; // 5 分钟
    
    return this.schedule(name, cronExpression, async () => {
      // 尝试获取分布式锁
      const acquired = await this.acquireLock(lockKey, lockTimeout);
      
      if (!acquired) {
        console.log(`[${name}] Skipped execution - lock held by another instance`);
        return;
      }
      
      try {
        await task();
      } finally {
        // 释放锁
        await this.releaseLock(lockKey);
      }
    });
  }
  
  async acquireLock(key, timeout) {
    const result = await this.redis.set(key, this.instanceId, 'PX', timeout, 'NX');
    return result === 'OK';
  }
  
  async releaseLock(key) {
    const owner = await this.redis.get(key);
    
    if (owner === this.instanceId) {
      await this.redis.del(key);
    }
  }
}

// 使用示例（多实例部署）
const distributedScheduler = new DistributedScheduler({
  host: 'localhost',
  port: 6379
});

// 即使有多个实例，任务也只会执行一次
distributedScheduler.scheduleDistributed(
  'global-cleanup',
  '0 4 * * *',
  async () => {
    await cleanupOldData();
  }
);
```

---

**下一节：** [c5-8 邮件服务集成](./c5-8-email-integration.md)  
**上一节：** [c5-3 进程管理](./c5-3-process-management.md)
