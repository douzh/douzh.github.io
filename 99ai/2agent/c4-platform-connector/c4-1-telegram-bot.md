# c4-1 Telegram Bot API 实战

## 1. 概述

Telegram Bot API 是最简单、最友好的机器人开发平台之一。本章详解从创建到高级功能的完整开发流程。

## 2. 快速开始

### 2.1 创建机器人

```javascript
// Step 1: 通过 @BotFather 创建机器人
// 在 Telegram 中搜索 @BotFather，发送 /newbot
// 按提示输入机器人名称和用户名
// 获取 Token: 1234567890:ABCdefGHIjklMNOpqrsTUVwxyz

const TELEGRAM_BOT_TOKEN = '1234567890:ABCdefGHIjklMNOpqrsTUVwxyz';
```

### 2.2 基础消息处理

```javascript
const axios = require('axios');

class TelegramBot {
  constructor(token) {
    this.token = token;
    this.baseUrl = `https://api.telegram.org/bot${token}`;
    this.webhookUrl = null;
  }
  
  // 获取更新（轮询方式）
  async getUpdates(offset = 0, limit = 100) {
    const response = await axios.get(`${this.baseUrl}/getUpdates`, {
      params: { offset, limit, timeout: 30 }
    });
    
    return response.data.result;
  }
  
  // 发送消息
  async sendMessage(chatId, text, options = {}) {
    const response = await axios.post(`${this.baseUrl}/sendMessage`, {
      chat_id: chatId,
      text,
      parse_mode: options.parseMode || 'Markdown',
      reply_markup: options.replyMarkup ? JSON.stringify(options.replyMarkup) : undefined
    });
    
    return response.data;
  }
  
  // 启动轮询
  async startPolling() {
    let offset = 0;
    
    console.log('🤖 Bot started polling...');
    
    while (true) {
      try {
        const updates = await this.getUpdates(offset);
        
        for (const update of updates) {
          offset = update.update_id + 1;
          await this.handleUpdate(update);
        }
      } catch (error) {
        console.error('Polling error:', error);
        await this.sleep(1000);
      }
    }
  }
  
  // 处理更新
  async handleUpdate(update) {
    if (update.message) {
      await this.handleMessage(update.message);
    } else if (update.callback_query) {
      await this.handleCallbackQuery(update.callback_query);
    }
  }
  
  // 处理消息
  async handleMessage(message) {
    const chatId = message.chat.id;
    const text = message.text;
    
    console.log(`Received from ${chatId}: ${text}`);
    
    // 简单的回声机器人
    if (text === '/start') {
      await this.sendMessage(chatId, '👋 欢迎使用本机器人！\n发送 /help 查看帮助');
    } else if (text === '/help') {
      await this.sendMessage(chatId, '📚 可用命令:\n/start - 开始\n/help - 帮助\n/echo - 回声');
    } else if (text.startsWith('/echo ')) {
      const echoText = text.replace('/echo ', '');
      await this.sendMessage(chatId, `🔊 ${echoText}`);
    } else {
      await this.sendMessage(chatId, `收到：${text}`);
    }
  }
  
  // 处理回调查询（按钮点击）
  async handleCallbackQuery(callbackQuery) {
    const chatId = callbackQuery.message.chat.id;
    const data = callbackQuery.data;
    
    await this.sendMessage(chatId, `您点击了：${data}`);
    
    // 确认回调已处理
    await axios.post(`${this.baseUrl}/answerCallbackQuery`, {
      callback_query_id: callbackQuery.id
    });
  }
  
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// 使用示例
const bot = new TelegramBot(TELEGRAM_BOT_TOKEN);
bot.startPolling();
```

## 3. 高级功能

### 3.1 Inline Keyboard

```javascript
class AdvancedTelegramBot extends TelegramBot {
  // 发送带内联键盘的消息
  async sendMessageWithKeyboard(chatId, text, keyboard) {
    return await this.sendMessage(chatId, text, {
      replyMarkup: {
        inline_keyboard: keyboard
      }
    });
  }
  
  // 示例：主菜单
  async showMainMenu(chatId) {
    const keyboard = [
      [
        { text: '📊 统计', callback_data: 'stats' },
        { text: '⚙️ 设置', callback_data: 'settings' }
      ],
      [
        { text: '📞 联系', callback_data: 'contact' },
        { text: 'ℹ️ 关于', callback_data: 'about' }
      ]
    ];
    
    await this.sendMessageWithKeyboard(
      chatId,
      '🏠 *主菜单*\n请选择一个选项:',
      keyboard
    );
  }
  
  // 示例：分页键盘
  createPaginationKeyboard(currentPage, totalPages, dataPrefix) {
    const keyboard = [];
    
    // 数据行
    const dataRow = [];
    for (let i = currentPage - 2; i <= currentPage + 2; i++) {
      if (i >= 1 && i <= totalPages) {
        dataRow.push({
          text: i === currentPage ? `● ${i}` : `${i}`,
          callback_data: `${dataPrefix}_page_${i}`
        });
      }
    }
    keyboard.push(dataRow);
    
    // 导航按钮
    const navRow = [];
    
    if (currentPage > 1) {
      navRow.push({ text: '◀ Prev', callback_data: `${dataPrefix}_page_${currentPage - 1}` });
    }
    
    if (currentPage < totalPages) {
      navRow.push({ text: 'Next ▶', callback_data: `${dataPrefix}_page_${currentPage + 1}` });
    }
    
    if (navRow.length > 0) {
      keyboard.push(navRow);
    }
    
    return keyboard;
  }
  
  // 覆盖 handleCallbackQuery 处理更多场景
  async handleCallbackQuery(callbackQuery) {
    const chatId = callbackQuery.message.chat.id;
    const messageId = callbackQuery.message.message_id;
    const data = callbackQuery.data;
    
    if (data === 'stats') {
      await this.showStats(chatId, messageId);
    } else if (data === 'settings') {
      await this.showSettings(chatId, messageId);
    } else if (data.startsWith('list_page_')) {
      const page = parseInt(data.replace('list_page_', ''));
      await this.showPaginatedList(chatId, messageId, page);
    }
    
    // 确认回调
    await axios.post(`${this.baseUrl}/answerCallbackQuery`, {
      callback_query_id: callbackQuery.id
    });
  }
  
  async showStats(chatId, messageId) {
    const stats = {
      users: 1234,
      messages: 5678,
      uptime: '99.9%'
    };
    
    const keyboard = [[{ text: '↩️ 返回', callback_data: 'main_menu' }]];
    
    await this.editMessageText(
      chatId,
      messageId,
      `📊 *统计数据*\n\n` +
      `👥 用户：${stats.users}\n` +
      `💬 消息：${stats.messages}\n` +
      `⏱ 在线率：${stats.uptime}`,
      keyboard
    );
  }
  
  // 编辑消息文本
  async editMessageText(chatId, messageId, text, keyboard = null) {
    const params = {
      chat_id: chatId,
      message_id: messageId,
      text,
      parse_mode: 'Markdown'
    };
    
    if (keyboard) {
      params.reply_markup = JSON.stringify({ inline_keyboard: keyboard });
    }
    
    await axios.post(`${this.baseUrl}/editMessageText`, params);
  }
}

// 使用示例
const advancedBot = new AdvancedTelegramBot(TELEGRAM_BOT_TOKEN);
advancedBot.startPolling();
```

### 3.2 文件处理

```javascript
const fs = require('fs');
const FormData = require('form-data');

class FileHandlingBot extends TelegramBot {
  // 发送图片
  async sendPhoto(chatId, photoPath, caption = '') {
    const form = new FormData();
    form.append('chat_id', chatId);
    form.append('photo', fs.createReadStream(photoPath));
    form.append('caption', caption);
    form.append('parse_mode', 'Markdown');
    
    await axios.post(`${this.baseUrl}/sendPhoto`, form, {
      headers: form.getHeaders()
    });
  }
  
  // 发送文档
  async sendDocument(chatId, documentPath, caption = '') {
    const form = new FormData();
    form.append('chat_id', chatId);
    form.append('document', fs.createReadStream(documentPath));
    form.append('caption', caption);
    
    await axios.post(`${this.baseUrl}/sendDocument`, form, {
      headers: form.getHeaders()
    });
  }
  
  // 下载文件
  async downloadFile(fileId, downloadPath) {
    // 获取文件信息
    const fileResponse = await axios.get(`${this.baseUrl}/getFile?file_id=${fileId}`);
    const filePath = fileResponse.data.result.file_path;
    
    // 下载文件
    const fileUrl = `https://api.telegram.org/file/bot${this.token}/${filePath}`;
    const response = await axios.get(fileUrl, { responseType: 'stream' });
    
    const writer = fs.createWriteStream(downloadPath);
    response.data.pipe(writer);
    
    return new Promise((resolve, reject) => {
      writer.on('finish', resolve);
      writer.on('error', reject);
    });
  }
  
  // 处理用户上传的文件
  async handleUserUpload(message) {
    const fileId = this.getFileIdFromMessage(message);
    
    if (!fileId) {
      await this.sendMessage(message.chat.id, '❌ 未找到文件');
      return;
    }
    
    const downloadPath = `/tmp/uploads/${fileId}`;
    await this.downloadFile(fileId, downloadPath);
    
    await this.sendMessage(
      message.chat.id,
      `✅ 文件已下载并保存到：${downloadPath}`
    );
  }
  
  getFileIdFromMessage(message) {
    if (message.document) return message.document.file_id;
    if (message.photo) return message.photo[message.photo.length - 1].file_id;
    if (message.audio) return message.audio.file_id;
    if (message.video) return message.video.file_id;
    return null;
  }
}
```

## 4. Webhook 模式

### 4.1 Express 服务器集成

```javascript
const express = require('express');
const bodyParser = require('body-parser');

class WebhookTelegramBot extends TelegramBot {
  constructor(token, webhookUrl) {
    super(token);
    this.webhookUrl = webhookUrl;
    this.app = express();
    this.setupMiddleware();
  }
  
  setupMiddleware() {
    this.app.use(bodyParser.json());
    
    // Webhook 端点
    this.app.post(`/webhook/${this.token}`, async (req, res) => {
      try {
        const update = req.body;
        await this.handleUpdate(update);
        res.sendStatus(200);
      } catch (error) {
        console.error('Webhook error:', error);
        res.sendStatus(500);
      }
    });
    
    // 健康检查
    this.app.get('/health', (req, res) => {
      res.json({ status: 'ok', timestamp: Date.now() });
    });
  }
  
  // 设置 Webhook
  async setWebhook() {
    const response = await axios.post(`${this.baseUrl}/setWebhook`, {
      url: this.webhookUrl
    });
    
    if (response.data.ok) {
      console.log('✅ Webhook set successfully');
    } else {
      console.error('❌ Failed to set webhook:', response.data);
    }
  }
  
  // 删除 Webhook（切换回轮询）
  async deleteWebhook() {
    await axios.post(`${this.baseUrl}/deleteWebhook`);
    console.log('🗑️ Webhook deleted');
  }
  
  // 获取 Webhook 信息
  async getWebhookInfo() {
    const response = await axios.get(`${this.baseUrl}/getWebhookInfo`);
    console.log('Webhook Info:', response.data.result);
    return response.data.result;
  }
  
  // 启动服务器
  startServer(port = 3000) {
    this.app.listen(port, () => {
      console.log(`🚀 Server running on port ${port}`);
    });
  }
}

// 使用示例
const webhookBot = new WebhookTelegramBot(
  TELEGRAM_BOT_TOKEN,
  'https://your-domain.com/webhook/1234567890:ABCdefGHIjklMNOpqrsTUVwxyz'
);

// 部署时调用一次
await webhookBot.setWebhook();

// 启动服务器
webhookBot.startServer(3000);
```

---

**下一节：** [c4-5 钉钉机器人](./c4-5-dingtalk-bot.md)  
**上一节：** [c4 平台对接模块](./README.md)
