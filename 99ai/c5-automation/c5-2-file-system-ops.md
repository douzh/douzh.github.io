# c5-2 文件系统操作

## 1. 概述

文件系统操作是 AI智能体的基础能力，包括文件读写、目录管理、文件监控等。本章详解安全、高效的文件操作实践。

## 2. 基础文件操作

### 2.1 安全的文件读写

```javascript
const fs = require('fs').promises;
const path = require('path');

class SafeFileOperations {
  constructor(options = {}) {
    this.baseDir = path.resolve(options.baseDir || process.cwd());
    this.allowedExtensions = options.allowedExtensions || [];
    this.maxFileSize = options.maxFileSize || 10 * 1024 * 1024; // 10MB
    this.forbiddenPaths = options.forbiddenPaths || [
      '/etc', '/root', '/boot', '/dev',
      'C:\\Windows', 'C:\\Program Files'
    ];
  }
  
  // 验证路径安全性
  validatePath(filePath) {
    const resolved = path.resolve(this.baseDir, filePath);
    const normalized = path.normalize(resolved);
    
    // 防止路径遍历攻击
    if (!normalized.startsWith(this.baseDir)) {
      throw new Error(`Access denied: ${filePath} is outside base directory`);
    }
    
    // 检查是否在禁止列表中
    for (const forbidden of this.forbiddenPaths) {
      if (normalized.startsWith(forbidden)) {
        throw new Error(`Access to ${forbidden} is forbidden`);
      }
    }
    
    return normalized;
  }
  
  // 检查文件扩展名
  validateExtension(filePath) {
    if (this.allowedExtensions.length === 0) return true;
    
    const ext = path.extname(filePath).toLowerCase();
    return this.allowedExtensions.includes(ext);
  }
  
  async readFile(filePath, options = {}) {
    const safePath = this.validatePath(filePath);
    
    // 检查文件大小
    const stats = await fs.stat(safePath);
    if (stats.size > this.maxFileSize) {
      throw new Error(`File too large: ${stats.size} bytes (max: ${this.maxFileSize})`);
    }
    
    return await fs.readFile(safePath, options.encoding || 'utf-8');
  }
  
  async writeFile(filePath, content, options = {}) {
    const safePath = this.validatePath(filePath);
    
    if (!this.validateExtension(safePath)) {
      throw new Error(`File extension not allowed: ${path.extname(safePath)}`);
    }
    
    // 确保父目录存在
    await fs.mkdir(path.dirname(safePath), { recursive: true });
    
    await fs.writeFile(safePath, content, options.encoding || 'utf-8');
    return safePath;
  }
  
  async appendFile(filePath, content) {
    const safePath = this.validatePath(filePath);
    await fs.appendFile(safePath, content);
    return safePath;
  }
  
  async deleteFile(filePath) {
    const safePath = this.validatePath(filePath);
    
    // 重要文件保护
    const protectedPatterns = ['*.conf', '*.cfg', '*.ini', '.env'];
    const fileName = path.basename(safePath);
    
    for (const pattern of protectedPatterns) {
      if (fileName.endsWith(pattern.replace('*', ''))) {
        throw new Error(`Cannot delete protected file: ${fileName}`);
      }
    }
    
    await fs.unlink(safePath);
  }
}

// 使用示例
const fileOps = new SafeFileOperations({
  baseDir: '/home/user/workspace',
  allowedExtensions: ['.txt', '.md', '.json', '.js'],
  maxFileSize: 5 * 1024 * 1024
});

// 安全读取
const content = await fileOps.readFile('docs/readme.md');

// 安全写入
await fileOps.writeFile('notes/todo.txt', '- Buy milk\n- Walk dog');
```

### 2.2 批量文件操作

```javascript
class BatchFileOperations {
  constructor(baseOps) {
    this.ops = baseOps;
  }
  
  async batchRead(filePatterns) {
    const results = [];
    
    for (const pattern of filePatterns) {
      try {
        const files = await this.globFiles(pattern);
        
        for (const file of files) {
          const content = await this.ops.readFile(file);
          results.push({
            file,
            content,
            success: true
          });
        }
      } catch (error) {
        results.push({
          pattern,
          error: error.message,
          success: false
        });
      }
    }
    
    return results;
  }
  
  async batchRename(renames) {
    const results = [];
    
    for (const { oldPath, newPath } of renames) {
      try {
        const safeOld = this.ops.validatePath(oldPath);
        const safeNew = this.ops.validatePath(newPath);
        
        await fs.rename(safeOld, safeNew);
        results.push({ oldPath, newPath, success: true });
      } catch (error) {
        results.push({ oldPath, newPath, success: false, error: error.message });
      }
    }
    
    return results;
  }
  
  async globFiles(pattern) {
    const glob = require('glob-promise');
    return await glob(pattern, { cwd: this.ops.baseDir });
  }
}

// 使用示例
const batchOps = new BatchFileOperations(fileOps);

// 批量读取所有 Markdown 文件
const mdFiles = await batchOps.batchRead(['**/*.md']);

// 批量重命名
await batchOps.batchRename([
  { oldPath: 'old-name.txt', newPath: 'new-name.txt' },
  { oldPath: 'temp.log', newPath: 'archived.log' }
]);
```

## 3. 文件监控

### 3.1 实时监控变化

```javascript
const chokidar = require('chokidar');

class FileWatcher {
  constructor(watchDir, options = {}) {
    this.watchDir = watchDir;
    this.listeners = new Map();
    
    this.watcher = chokidar.watch(watchDir, {
      ignored: /(^|[\/\\])\../, // 忽略隐藏文件
      persistent: true,
      ignoreInitial: options.ignoreInitial || false,
      awaitWriteFinish: {
        stabilityThreshold: 100,
        pollInterval: 10
      }
    });
    
    this.setupEventHandlers();
  }
  
  setupEventHandlers() {
    this.watcher
      .on('add', (path) => this.notifyListeners('create', path))
      .on('change', (path) => this.notifyListeners('modify', path))
      .on('unlink', (path) => this.notifyListeners('delete', path))
      .on('addDir', (path) => this.notifyListeners('createDir', path))
      .on('unlinkDir', (path) => this.notifyListeners('deleteDir', path))
      .on('error', (error) => console.error('Watcher error:', error));
  }
  
  on(eventType, callback) {
    if (!this.listeners.has(eventType)) {
      this.listeners.set(eventType, []);
    }
    this.listeners.get(eventType).push(callback);
  }
  
  notifyListeners(eventType, path) {
    const callbacks = this.listeners.get(eventType) || [];
    callbacks.forEach(cb => cb(path));
    
    // 也触发通用事件
    const allCallbacks = this.listeners.get('all') || [];
    allCallbacks.forEach(cb => cb(eventType, path));
  }
  
  close() {
    return this.watcher.close();
  }
}

// 使用示例
const watcher = new FileWatcher('./workspace');

watcher.on('create', (path) => {
  console.log(`📄 File created: ${path}`);
});

watcher.on('modify', async (path) => {
  console.log(`✏️ File modified: ${path}`);
  
  // 可以触发自定义逻辑，如自动保存、重新加载等
  if (path.endsWith('.md')) {
    await this.processMarkdownFile(path);
  }
});

watcher.on('delete', (path) => {
  console.log(`🗑️ File deleted: ${path}`);
});
```

### 3.2 自动备份机制

```javascript
class AutoBackupService {
  constructor(sourceDir, backupDir, options = {}) {
    this.sourceDir = sourceDir;
    this.backupDir = backupDir;
    this.maxBackups = options.maxBackups || 10;
    this.includePatterns = options.includePatterns || ['**/*'];
    this.excludePatterns = options.excludePatterns || ['node_modules/**', '.git/**'];
    
    this.watcher = new FileWatcher(sourceDir);
    this.setupAutoBackup();
  }
  
  setupAutoBackup() {
    // 文件变化时延迟备份（避免频繁写入）
    const debounceBackup = this.debounce(async (path) => {
      await this.backupFile(path);
    }, 2000);
    
    this.watcher.on('modify', debounceBackup);
    this.watcher.on('create', debounceBackup);
  }
  
  async backupFile(filePath) {
    const relativePath = path.relative(this.sourceDir, filePath);
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const backupPath = path.join(
      this.backupDir,
      `${relativePath}.${timestamp}.bak`
    );
    
    try {
      await fs.mkdir(path.dirname(backupPath), { recursive: true });
      await fs.copyFile(filePath, backupPath);
      
      console.log(`✅ Backed up: ${relativePath}`);
      
      // 清理旧备份
      await this.cleanupOldBackups(relativePath);
      
    } catch (error) {
      console.error(`❌ Backup failed for ${filePath}:`, error);
    }
  }
  
  async cleanupOldBackups(relativePath) {
    const glob = require('glob-promise');
    const pattern = `${relativePath}.*.bak`;
    const backups = await glob(pattern, { cwd: this.backupDir });
    
    if (backups.length > this.maxBackups) {
      // 按时间排序，删除旧的
      const sorted = backups.sort((a, b) => {
        const timeA = this.extractTimestamp(a);
        const timeB = this.extractTimestamp(b);
        return timeB - timeA; // 新的在前
      });
      
      for (let i = this.maxBackups; i < sorted.length; i++) {
        await fs.unlink(path.join(this.backupDir, sorted[i]));
      }
    }
  }
  
  extractTimestamp(filename) {
    const match = filename.match(/\.(\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2})/);
    return match ? new Date(match[1]).getTime() : 0;
  }
  
  debounce(func, wait) {
    let timeout;
    return (...args) => {
      clearTimeout(timeout);
      timeout = setTimeout(() => func.apply(this, args), wait);
    };
  }
}

// 使用示例
const backupService = new AutoBackupService(
  './workspace/projects',
  './backups/projects',
  { maxBackups: 5 }
);

console.log('Auto-backup service started');
```

## 4. 高级功能

### 4.1 智能文件组织

```javascript
class SmartFileOrganizer {
  constructor(baseDir) {
    this.baseDir = baseDir;
  }
  
  async organizeByType(sourceDir) {
    const files = await this.getAllFiles(sourceDir);
    
    for (const file of files) {
      const ext = path.extname(file).toLowerCase();
      const typeName = this.getFileType(ext);
      
      const targetDir = path.join(sourceDir, '__organized', typeName);
      const fileName = path.basename(file);
      const targetPath = path.join(targetDir, fileName);
      
      await fs.mkdir(targetDir, { recursive: true });
      await fs.rename(file, targetPath);
      
      console.log(`Moved ${fileName} → ${typeName}/`);
    }
  }
  
  getFileType(ext) {
    const typeMap = {
      // 图片
      '.jpg': 'images', '.jpeg': 'images', '.png': 'images',
      '.gif': 'images', '.svg': 'images', '.webp': 'images',
      
      // 文档
      '.pdf': 'documents', '.doc': 'documents', '.docx': 'documents',
      '.xls': 'documents', '.xlsx': 'documents', '.ppt': 'documents',
      
      // 代码
      '.js': 'code', '.ts': 'code', '.py': 'code',
      '.java': 'code', '.cpp': 'code', '.go': 'code',
      
      // 配置文件
      '.json': 'config', '.yaml': 'config', '.yml': 'config',
      '.toml': 'config', '.ini': 'config',
      
      // 日志
      '.log': 'logs',
      
      // 其他
      '.txt': 'text', '.md': 'text', '.csv': 'data'
    };
    
    return typeMap[ext] || 'misc';
  }
  
  async getAllFiles(dir) {
    const glob = require('glob-promise');
    return await glob('**/*', { 
      cwd: dir, 
      absolute: true,
      nodir: true 
    });
  }
}

// 使用示例
const organizer = new SmartFileOrganizer('./downloads');
await organizer.organizeByType('./downloads');

// 结果：
// downloads/
//   __organized/
//     images/
//       photo.jpg
//       screenshot.png
//     documents/
//       report.pdf
//     code/
//       script.js
```

### 4.2 文件内容分析

```javascript
class FileAnalyzer {
  async analyzeDirectory(dir) {
    const files = await this.getAllFiles(dir);
    const analysis = {
      totalFiles: files.length,
      byExtension: {},
      bySize: {
        small: 0,    // < 1KB
        medium: 0,   // 1KB - 1MB
        large: 0,    // > 1MB
        total: 0
      },
      oldestFile: null,
      newestFile: null,
      largestFile: null
    };
    
    for (const file of files) {
      const stats = await fs.stat(file);
      const ext = path.extname(file).toLowerCase();
      
      // 按扩展名统计
      analysis.byExtension[ext] = (analysis.byExtension[ext] || 0) + 1;
      
      // 按大小统计
      analysis.bySize.total += stats.size;
      if (stats.size < 1024) {
        analysis.bySize.small++;
      } else if (stats.size < 1024 * 1024) {
        analysis.bySize.medium++;
      } else {
        analysis.bySize.large++;
      }
      
      // 最大文件
      if (!analysis.largestFile || stats.size > analysis.largestFile.size) {
        analysis.largestFile = { path: file, size: stats.size };
      }
      
      // 最早和最新的文件
      if (!analysis.oldestFile || stats.mtime < analysis.oldestFile.mtime) {
        analysis.oldestFile = { path: file, mtime: stats.mtime };
      }
      if (!analysis.newestFile || stats.mtime > analysis.newestFile.mtime) {
        analysis.newestFile = { path: file, mtime: stats.mtime };
      }
    }
    
    return analysis;
  }
  
  async findDuplicates(dir) {
    const files = await this.getAllFiles(dir);
    const hashToFiles = new Map();
    
    for (const file of files) {
      const hash = await this.hashFile(file);
      
      if (!hashToFiles.has(hash)) {
        hashToFiles.set(hash, []);
      }
      hashToFiles.get(hash).push(file);
    }
    
    // 返回重复的文件组
    const duplicates = [];
    for (const [hash, fileList] of hashToFiles.entries()) {
      if (fileList.length > 1) {
        duplicates.push({
          hash,
          files: fileList,
          count: fileList.length
        });
      }
    }
    
    return duplicates;
  }
  
  async hashFile(filePath) {
    const crypto = require('crypto');
    const content = await fs.readFile(filePath);
    return crypto.createHash('md5').update(content).digest('hex');
  }
}

// 使用示例
const analyzer = new FileAnalyzer();
const report = await analyzer.analyzeDirectory('./workspace');

console.log('Directory Analysis:');
console.log(`Total files: ${report.totalFiles}`);
console.log(`Total size: ${(report.bySize.total / 1024 / 1024).toFixed(2)} MB`);
console.log('By extension:', report.byExtension);

// 查找重复文件
const duplicates = await analyzer.findDuplicates('./workspace');
if (duplicates.length > 0) {
  console.log(`Found ${duplicates.length} groups of duplicate files`);
}
```

---

**下一节：** [c5-5 浏览器自动化](./c5-5-browser-automation.md)  
**上一节：** [c5-1 Shell 命令执行](./c5-1-shell-execution.md)
