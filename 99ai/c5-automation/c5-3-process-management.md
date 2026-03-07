# c5-3 进程管理与调度

## 1. 概述

进程管理让 AI智能体能够启动、监控和控制后台进程，实现长时间运行的任务和并发操作。

## 2. 基础进程管理

### 2.1 进程启动与停止

```javascript
const { spawn, fork, exec } = require('child_process');

class ProcessManager {
  constructor() {
    this.processes = new Map();
    this.maxProcesses = 50;
  }
  
  // 启动子进程
  spawnProcess(name, command, args = [], options = {}) {
    if (this.processes.size >= this.maxProcesses) {
      throw new Error(`Maximum process limit (${this.maxProcesses}) reached`);
    }
    
    const child = spawn(command, args, {
      stdio: ['ignore', 'pipe', 'pipe'],
      detached: options.detached || false,
      env: { ...process.env, ...options.env },
      cwd: options.cwd || process.cwd()
    });
    
    const processInfo = {
      name,
      pid: child.pid,
      child,
      startTime: Date.now(),
      status: 'running',
      restartCount: 0
    };
    
    // 监听输出
    child.stdout.on('data', (data) => {
      console.log(`[${name}] ${data.toString().trim()}`);
      processInfo.lastOutput = data.toString();
    });
    
    child.stderr.on('data', (data) => {
      console.error(`[${name}] ERROR: ${data.toString().trim()}`);
      processInfo.lastError = data.toString();
    });
    
    // 监听退出
    child.on('exit', (code, signal) => {
      console.log(`[${name}] exited with code ${code}, signal ${signal}`);
      processInfo.status = 'exited';
      processInfo.exitCode = code;
      processInfo.exitSignal = signal;
      
      // 自动重启（如果配置了）
      if (options.autoRestart && code !== 0 && processInfo.restartCount < 3) {
        setTimeout(() => {
          processInfo.restartCount++;
          this.spawnProcess(name, command, args, options);
        }, 1000 * processInfo.restartCount);
      }
    });
    
    this.processes.set(name, processInfo);
    console.log(`✅ Started process: ${name} (PID: ${child.pid})`);
    
    return child;
  }
  
  // 获取进程信息
  getProcess(name) {
    return this.processes.get(name);
  }
  
  // 列出所有进程
  listProcesses() {
    return Array.from(this.processes.entries()).map(([name, info]) => ({
      name,
      pid: info.pid,
      status: info.status,
      uptime: Date.now() - info.startTime,
      restartCount: info.restartCount
    }));
  }
  
  // 停止进程
  async stopProcess(name, signal = 'SIGTERM', timeout = 5000) {
    const processInfo = this.processes.get(name);
    
    if (!processInfo) {
      throw new Error(`Process ${name} not found`);
    }
    
    return new Promise((resolve, reject) => {
      const child = processInfo.child;
      
      // 设置超时强制杀死
      const forceKillTimeout = setTimeout(() => {
        if (processInfo.status === 'running') {
          child.kill('SIGKILL');
          console.warn(`[${name}] Force killed after ${timeout}ms`);
        }
      }, timeout);
      
      child.once('exit', () => {
        clearTimeout(forceKillTimeout);
        this.processes.delete(name);
        console.log(`🛑 Stopped process: ${name}`);
        resolve();
      });
      
      child.kill(signal);
    });
  }
  
  // 停止所有进程
  async stopAll(signal = 'SIGTERM') {
    const promises = [];
    
    for (const [name] of this.processes) {
      promises.push(this.stopProcess(name, signal));
    }
    
    await Promise.all(promises);
    console.log('🛑 All processes stopped');
  }
}

// 使用示例
const pm = new ProcessManager();

// 启动一个长期运行的服务
pm.spawnProcess('web-server', 'node', ['server.js'], {
  autoRestart: true,
  env: { NODE_ENV: 'production' }
});

// 启动数据处理任务
pm.spawnProcess('data-processor', 'python', ['-u', 'process.py']);

// 查看状态
console.log(pm.listProcesses());

// 优雅关闭
process.on('SIGINT', async () => {
  await pm.stopAll();
  process.exit(0);
});
```

### 2.2 进程资源监控

```javascript
const os = require('os');

class ProcessMonitor {
  constructor() {
    this.metrics = new Map();
  }
  
  // 监控进程资源
  monitorProcess(pid) {
    try {
      // Linux/Mac: 使用 ps 命令
      if (process.platform !== 'win32') {
        const { execSync } = require('child_process');
        
        const output = execSync(
          `ps -p ${pid} -o pid,ppid,%cpu,%mem,rss,vsz,etime,cmd`,
          { encoding: 'utf8' }
        );
        
        const lines = output.trim().split('\n');
        if (lines.length > 1) {
          const [header, ...values] = lines;
          const parts = values.join(' ').split(/\s+/);
          
          return {
            pid: parseInt(parts[0]),
            ppid: parseInt(parts[1]),
            cpu: parseFloat(parts[2]),
            memory: parseFloat(parts[3]),
            rss: parseInt(parts[4]) * 1024, // KB to bytes
            virtualSize: parseInt(parts[5]) * 1024,
            elapsedTime: parts[6],
            command: parts.slice(7).join(' ')
          };
        }
      }
      
      // Windows: 使用 tasklist
      return this.getWindowsProcessInfo(pid);
      
    } catch (error) {
      console.error(`Failed to monitor process ${pid}:`, error.message);
      return null;
    }
  }
  
  // 系统资源使用情况
  getSystemStats() {
    return {
      cpu: {
        usage: os.loadavg(),
        cores: os.cpus().length,
        model: os.cpus()[0].model
      },
      memory: {
        total: os.totalmem(),
        free: os.freemem(),
        used: os.totalmem() - os.freemem(),
        usagePercent: ((os.totalmem() - os.freemem()) / os.totalmem() * 100).toFixed(2) + '%'
      },
      platform: os.platform(),
      uptime: os.uptime(),
      hostname: os.hostname()
    };
  }
  
  // 定期监控
  startPeriodicMonitoring(pids, interval = 5000) {
    const metrics = [];
    
    const timer = setInterval(() => {
      const snapshot = {
        timestamp: Date.now(),
        system: this.getSystemStats(),
        processes: []
      };
      
      for (const pid of pids) {
        const info = this.monitorProcess(pid);
        if (info) {
          snapshot.processes.push(info);
        }
      }
      
      metrics.push(snapshot);
      
      // 保持最近 100 个样本
      if (metrics.length > 100) {
        metrics.shift();
      }
      
      console.log(`[Monitor] CPU: ${snapshot.system.cpu.usage[0].toFixed(2)}, ` +
                  `Mem: ${snapshot.system.memory.usagePercent}`);
      
    }, interval);
    
    return {
      stop: () => clearInterval(timer),
      getMetrics: () => metrics
    };
  }
}

// 使用示例
const monitor = new ProcessMonitor();

// 监控系统资源
const stats = monitor.getSystemStats();
console.log(stats);

// 监控特定进程
const processInfo = monitor.monitorProcess(12345);
console.log(processInfo);

// 定期监控
const monitoring = monitor.startPeriodicMonitoring([12345, 67890]);

// 5 分钟后停止
setTimeout(() => {
  const allMetrics = monitoring.getMetrics();
  console.log('Collected', allMetrics.length, 'samples');
  monitoring.stop();
}, 5 * 60 * 1000);
```

## 3. 进程池管理

### 3.1 Worker 进程池

```javascript
const { fork } = require('child_process');
const path = require('path');

class WorkerPool {
  constructor(workerScript, options = {}) {
    this.workerScript = workerScript;
    this.poolSize = options.poolSize || os.cpus().length;
    this.workers = [];
    this.taskQueue = [];
    this.availableWorkers = [];
  }
  
  // 初始化进程池
  initialize() {
    console.log(`Initializing worker pool with ${this.poolSize} workers...`);
    
    for (let i = 0; i < this.poolSize; i++) {
      this.createWorker(i);
    }
  }
  
  createWorker(index) {
    const worker = fork(this.workerScript, [], {
      env: { WORKER_ID: index }
    });
    
    const workerInfo = {
      id: index,
      worker,
      busy: false,
      currentTask: null,
      tasksCompleted: 0
    };
    
    worker.on('message', (message) => {
      this.handleWorkerMessage(workerInfo, message);
    });
    
    worker.on('exit', () => {
      console.log(`Worker ${index} exited, restarting...`);
      this.workers = this.workers.filter(w => w.id !== index);
      this.createWorker(index);
    });
    
    this.workers.push(workerInfo);
    this.availableWorkers.push(workerInfo);
    
    console.log(`✅ Worker ${index} ready`);
  }
  
  handleWorkerMessage(workerInfo, message) {
    if (message.type === 'task_complete') {
      workerInfo.busy = false;
      workerInfo.tasksCompleted++;
      
      // 通知任务完成
      if (workerInfo.currentTask?.callback) {
        workerInfo.currentTask.callback(null, message.result);
      }
      
      // 回到可用列表
      this.availableWorkers.push(workerInfo);
      
      // 处理下一个排队任务
      this.processNextTask();
    }
  }
  
  // 提交任务
  execute(taskData, callback) {
    const task = {
      data: taskData,
      callback
    };
    
    if (this.availableWorkers.length > 0) {
      // 有可用 worker，立即执行
      const worker = this.availableWorkers.shift();
      this.assignTask(worker, task);
    } else {
      // 无可用 worker，加入队列
      this.taskQueue.push(task);
      console.log(`Task queued (queue size: ${this.taskQueue.length})`);
    }
  }
  
  assignTask(worker, task) {
    worker.busy = true;
    worker.currentTask = task;
    
    worker.worker.send({
      type: 'task',
      data: task.data
    });
  }
  
  processNextTask() {
    if (this.taskQueue.length > 0 && this.availableWorkers.length > 0) {
      const task = this.taskQueue.shift();
      const worker = this.availableWorkers.shift();
      this.assignTask(worker, task);
    }
  }
  
  // 获取池状态
  getStatus() {
    return {
      totalWorkers: this.workers.length,
      availableWorkers: this.availableWorkers.length,
      busyWorkers: this.workers.filter(w => w.busy).length,
      queueLength: this.taskQueue.length,
      totalTasksCompleted: this.workers.reduce((sum, w) => sum + w.tasksCompleted, 0)
    };
  }
  
  // 销毁池
  async destroy() {
    console.log('Destroying worker pool...');
    
    const exitPromises = this.workers.map(w => {
      return new Promise(resolve => {
        w.worker.once('exit', resolve);
        w.worker.send({ type: 'shutdown' });
      });
    });
    
    await Promise.all(exitPromises);
    this.workers = [];
    this.availableWorkers = [];
    
    console.log('🛑 Worker pool destroyed');
  }
}

// worker.js 示例
// if (process.send) {
//   process.on('message', (msg) => {
//     if (msg.type === 'task') {
//       const result = doHeavyWork(msg.data);
//       process.send({
//         type: 'task_complete',
//         result
//       });
//     } else if (msg.type === 'shutdown') {
//       process.exit(0);
//     }
//   });
// }

// 使用示例
const pool = new WorkerPool(path.join(__dirname, 'worker.js'), {
  poolSize: 4
});

pool.initialize();

// 提交多个任务
for (let i = 0; i < 10; i++) {
  pool.execute({ taskId: i, data: `Task ${i}` }, (err, result) => {
    if (err) {
      console.error(`Task ${i} failed:`, err);
    } else {
      console.log(`Task ${i} completed:`, result);
    }
  });
}

// 监控状态
setInterval(() => {
  console.log('Pool status:', pool.getStatus());
}, 2000);
```

---

**下一节：** [c5-4 Cron Jobs 定时任务](./c5-4-cron-jobs.md)  
**上一节：** [c5-2 文件系统操作](./c5-2-file-system-ops.md)
