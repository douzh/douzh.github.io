# c5-5 浏览器自动化

## 1. 概述

浏览器自动化让 AI智能体能够操作网页、抓取数据、自动填表等。本章详解 Puppeteer 和 Playwright 的使用技巧和最佳实践。

## 2. Puppeteer 基础

### 2.1 启动与控制

```javascript
const puppeteer = require('puppeteer');

class BrowserController {
  constructor(options = {}) {
    this.defaultOptions = {
      headless: options.headless ?? true,
      args: [
        '--no-sandbox',
        '--disable-setuid-sandbox',
        '--disable-dev-shm-usage',
        '--disable-accelerated-2d-canvas',
        '--disable-gpu'
      ],
      defaultViewport: { width: 1920, height: 1080 },
      ...options
    };
    
    this.browser = null;
  }
  
  async launch() {
    this.browser = await puppeteer.launch(this.defaultOptions);
    console.log('Browser launched');
  }
  
  async newPage() {
    if (!this.browser) await this.launch();
    return await this.browser.newPage();
  }
  
  async close() {
    if (this.browser) {
      await this.browser.close();
      this.browser = null;
    }
  }
}

// 使用示例
const browser = new BrowserController({ headless: true });

const page = await browser.newPage();
await page.goto('https://example.com');
await page.screenshot({ path: 'screenshot.png' });

await browser.close();
```

### 2.2 页面操作

```javascript
class PageActions {
  constructor(page) {
    this.page = page;
  }
  
  // 导航和等待
  async navigate(url, waitUntil = 'networkidle2') {
    await this.page.goto(url, { 
      waitUntil,
      timeout: 30000 
    });
  }
  
  // 点击元素
  async click(selector, options = {}) {
    await this.page.waitForSelector(selector, { timeout: 5000 });
    
    if (options.delay) {
      await this.page.click(selector, { delay: options.delay });
    } else {
      await this.page.click(selector);
    }
  }
  
  // 输入文本
  async type(selector, text, options = {}) {
    await this.page.waitForSelector(selector);
    
    if (options.clear) {
      await this.page.click(selector, { clickCount: 3 }); // 全选
    }
    
    await this.page.type(selector, text, { 
      delay: options.delay || 50 // 模拟人类打字
    });
  }
  
  // 选择下拉选项
  async select(selector, value) {
    await this.page.select(selector, value);
  }
  
  // 上传文件
  async uploadFile(selector, filePath) {
    const input = await this.page.$(selector);
    await input.uploadFile(filePath);
  }
  
  // 滚动页面
  async scrollTo(selector) {
    await this.page.evaluate((sel) => {
      document.querySelector(sel)?.scrollIntoView({ behavior: 'smooth' });
    }, selector);
  }
  
  // 截图
  async screenshot(options = {}) {
    const defaultOptions = {
      fullPage: false,
      type: 'png',
      quality: 80
    };
    
    return await this.page.screenshot({ 
      ...defaultOptions, 
      ...options 
    });
  }
  
  // PDF 导出
  async toPDF(options = {}) {
    const defaultOptions = {
      format: 'A4',
      printBackground: true
    };
    
    return await this.page.pdf({ 
      ...defaultOptions, 
      ...options 
    });
  }
}

// 实战示例
async function automateLogin() {
  const browser = new BrowserController();
  const page = await browser.newPage();
  const actions = new PageActions(page);
  
  await actions.navigate('https://example.com/login');
  
  // 填写表单
  await actions.type('#username', 'myuser', { clear: true });
  await actions.type('#password', 'mypassword', { clear: true });
  
  // 提交
  await actions.click('#login-button');
  
  // 等待跳转
  await page.waitForNavigation();
  
  // 截图保存
  await actions.screenshot({ path: 'dashboard.png' });
  
  await browser.close();
}
```

## 3. 数据抓取

### 3.1 智能数据提取

```javascript
class DataScraper {
  constructor(page) {
    this.page = page;
  }
  
  // 提取列表数据
  async scrapeList(listSelector, itemSelectors) {
    return await this.page.evaluate((listSel, itemSels) => {
      const items = [];
      const listElements = document.querySelectorAll(listSel);
      
      for (const itemEl of listElements) {
        const item = {};
        
        for (const [key, selector] of Object.entries(itemSels)) {
          const el = itemEl.querySelector(selector);
          item[key] = el ? (el.textContent?.trim() || el.getAttribute('href')) : null;
        }
        
        items.push(item);
      }
      
      return items;
    }, listSelector, itemSelectors);
  }
  
  // 提取结构化数据
  async scrapeStructuredData(schema) {
    return await this.page.evaluate((schemaObj) => {
      const data = {};
      
      for (const [key, selector] of Object.entries(schemaObj)) {
        const el = document.querySelector(selector);
        
        if (!el) {
          data[key] = null;
          continue;
        }
        
        // 根据标签类型提取数据
        if (el.tagName === 'IMG') {
          data[key] = el.src;
        } else if (el.tagName === 'A') {
          data[key] = {
            text: el.textContent.trim(),
            href: el.href
          };
        } else if (el.tagName === 'INPUT') {
          data[key] = el.value;
        } else {
          data[key] = el.textContent.trim();
        }
      }
      
      return data;
    }, schema);
  }
  
  // 处理分页
  async scrapeWithPagination(config) {
    const allResults = [];
    let currentPage = 1;
    
    while (true) {
      console.log(`Scraping page ${currentPage}...`);
      
      // 抓取当前页
      const results = await config.extractor();
      allResults.push(...results);
      
      // 检查是否有下一页
      if (config.hasNext) {
        const hasNext = await this.page.evaluate(config.hasNext);
        if (!hasNext) break;
      }
      
      // 点击下一页
      if (config.nextButton) {
        await this.page.click(config.nextButton);
        await this.page.waitForNavigation({ waitUntil: 'networkidle2' });
      } else {
        break;
      }
      
      currentPage++;
      
      // 防止被封
      if (config.delayBetweenPages) {
        await this.sleep(config.delayBetweenPages);
      }
    }
    
    return allResults;
  }
  
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// 使用示例
async function scrapeProducts() {
  const browser = new BrowserController();
  const page = await browser.newPage();
  await page.goto('https://example.com/products');
  
  const scraper = new DataScraper(page);
  
  // 定义数据结构
  const productSchema = {
    name: '.product-name',
    price: '.product-price',
    image: '.product-image img',
    link: '.product-link a',
    rating: '.product-rating'
  };
  
  // 抓取所有产品
  const products = await scraper.scrapeWithPagination({
    extractor: () => scraper.scrapeStructuredData(productSchema),
    nextButton: '.next-page',
    hasNext: () => !!document.querySelector('.next-page'),
    delayBetweenPages: 2000
  });
  
  console.log(`Scraped ${products.length} products`);
  
  await browser.close();
  return products;
}
```

### 3.2 反反爬虫策略

```javascript
class StealthScraper {
  constructor(browser) {
    this.browser = browser;
  }
  
  async initPage(page) {
    // 隐藏 WebDriver 特征
    await page.evaluateOnNewDocument(() => {
      // Override navigator.webdriver
      Object.defineProperty(navigator, 'webdriver', {
        get: () => undefined
      });
      
      // Mock plugins
      Object.defineProperty(navigator, 'plugins', {
        get: () => [1, 2, 3, 4, 5]
      });
      
      // Mock languages
      Object.defineProperty(navigator, 'languages', {
        get: () => ['en-US', 'en']
      });
    });
    
    // 设置随机 User-Agent
    const userAgents = [
      'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36...',
      'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36...',
      'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36...'
    ];
    
    const randomUA = userAgents[Math.floor(Math.random() * userAgents.length)];
    await page.setUserAgent(randomUA);
    
    // 设置随机 viewport
    await page.setViewport({
      width: 1920 + Math.floor(Math.random() * 100),
      height: 1080 + Math.floor(Math.random() * 100)
    });
  }
  
  // 模拟人类行为
  async humanLikeAction(page, action) {
    // 随机延迟
    const delay = 100 + Math.random() * 200;
    await page.waitForTimeout(delay);
    
    // 执行动作
    await action();
    
    // 动作后延迟
    await page.waitForTimeout(200 + Math.random() * 300);
  }
}

// 使用示例
async function stealthScrape() {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  
  const stealth = new StealthScraper(browser);
  await stealth.initPage(page);
  
  await page.goto('https://example.com');
  
  // 人类般的点击
  await stealth.humanLikeAction(page, async () => {
    await page.click('#button');
  });
  
  await browser.close();
}
```

## 4. Playwright 跨浏览器

### 4.1 多浏览器支持

```javascript
const { chromium, firefox, webkit } = require('playwright');

class CrossBrowserAutomation {
  constructor(browserType = 'chromium') {
    this.browserType = browserType;
    this.browser = null;
  }
  
  async launch(browserOptions = {}) {
    const browsers = {
      chromium: chromium,
      firefox: firefox,
      webkit: webkit
    };
    
    const browserEngine = browsers[this.browserType];
    
    this.browser = await browserEngine.launch({
      headless: true,
      ...browserOptions
    });
    
    console.log(`Launched ${this.browserType}`);
  }
  
  async newContext(options = {}) {
    if (!this.browser) await this.launch();
    
    return await this.browser.newContext({
      viewport: { width: 1920, height: 1080 },
      userAgent: 'Mozilla/5.0 ...',
      ...options
    });
  }
  
  async recordVideo(context, videoDir) {
    await context.tracing.start({ screenshots: true, snapshots: true });
    
    return async () => {
      await context.tracing.stop({ path: `${videoDir}/trace.zip` });
    };
  }
}

// 使用示例：跨浏览器测试
async function testAcrossBrowsers() {
  const urls = ['https://example.com'];
  const browsers = ['chromium', 'firefox', 'webkit'];
  
  for (const browserType of browsers) {
    console.log(`Testing on ${browserType}...`);
    
    const automation = new CrossBrowserAutomation(browserType);
    const context = await automation.newContext();
    const page = await context.newPage();
    
    await page.goto(urls[0]);
    const title = await page.title();
    
    console.log(`${browserType}: ${title}`);
    
    await context.close();
  }
}
```

## 5. 语义快照生成

### 5.1 Accessibility Tree 提取

```javascript
class SemanticSnapshotGenerator {
  constructor(page) {
    this.page = page;
  }
  
  async generateSnapshot() {
    const accessibility = await this.page.accessibility.snapshot();
    return this.processAccessibilityTree(accessibility);
  }
  
  processAccessibilityTree(node, depth = 0) {
    if (!node) return '';
    
    let result = '';
    const indent = '  '.repeat(depth);
    
    // 提取关键信息
    const info = {
      role: node.role,
      name: node.name,
      value: node.value,
      description: node.description
    };
    
    // 过滤无用信息
    if (info.name || info.value) {
      result += `${indent}[${info.role}]`;
      
      if (info.name) {
        result += ` "${info.name}"`;
      }
      
      if (info.value) {
        result += ` = "${info.value}"`;
      }
      
      result += '\n';
    }
    
    // 递归处理子节点
    if (node.children) {
      for (const child of node.children) {
        result += this.processAccessibilityTree(child, depth + 1);
      }
    }
    
    return result;
  }
  
  // 转换为 Token 友好的格式
  async generateTokenFriendlySnapshot() {
    const snapshot = await this.generateSnapshot();
    
    // 进一步压缩
    const compressed = snapshot
      .replace(/\[button\]/g, '🔘')
      .replace(/\[link\]/g, '🔗')
      .replace(/\[heading\]/g, '📌')
      .replace(/\[text\]/g, '')
      .replace(/\[image\]/g, '🖼️');
    
    return {
      raw: snapshot,
      compressed,
      tokenCount: this.estimateTokens(compressed)
    };
  }
  
  estimateTokens(text) {
    // 粗略估算：英文 4 字符/token，中文 1.5 字符/token
    const englishChars = text.replace(/[^\x00-\x7F]/g, '').length;
    const chineseChars = text.match(/[\u4e00-\u9fa5]/g)?.length || 0;
    
    return Math.ceil(englishChars / 4 + chineseChars / 1.5);
  }
}

// 使用示例
async function analyzePage() {
  const browser = new BrowserController();
  const page = await browser.newPage();
  await page.goto('https://example.com');
  
  const snapshotGen = new SemanticSnapshotGenerator(page);
  const snapshot = await snapshotGen.generateTokenFriendlySnapshot();
  
  console.log(`Raw snapshot (${snapshot.tokenCount} tokens):`);
  console.log(snapshot.compressed);
  
  await browser.close();
}
```

---

**上一节：** [c5-2 文件系统操作](./c5-2-file-system-ops.md)  
**c5 模块核心文档完成！**
