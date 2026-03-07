# c4 平台对接其他主题概要

## c4-2 Discord Bot

```javascript
const { Client, GatewayIntentBits } = require('discord.js');

class DiscordBot {
  constructor(token) {
    this.client = new Client({
      intents: [
        GatewayIntentBits.Guilds,
        GatewayIntentBits.GuildMessages,
        GatewayIntentBits.MessageContent
      ]
    });
    
    this.client.login(token);
    this.setupEventHandlers();
  }
  
  setupEventHandlers() {
    this.client.on('ready', () => {
      console.log(`✅ Logged in as ${this.client.user.tag}`);
    });
    
    this.client.on('messageCreate', async (message) => {
      if (message.author.bot) return;
      
      if (message.content === '!ping') {
        await message.reply('Pong! 🏓');
      }
    });
  }
}
```

## c4-3 WhatsApp Business API

```javascript
// 使用 Twilio SDK
const twilio = require('twilio');

class WhatsAppBot {
  constructor(accountSid, authToken) {
    this.client = twilio(accountSid, authToken);
  }
  
  async sendMessage(to, body) {
    return await this.client.messages.create({
      from: 'whatsapp:+14155238886',
      to: `whatsapp:+${to}`,
      body
    });
  }
  
  // Webhook 接收消息
  setupWebhook(app) {
    app.post('/whatsapp/webhook', (req, res) => {
      const message = req.body;
      console.log('Received:', message.From, message.Body);
      
      // 自动回复
      this.sendMessage(message.From.replace('whatsapp:', ''), '收到您的消息');
      
      res.status(200).send('OK');
    });
  }
}
```

## c4-4 Slack Bot

```javascript
const { App } = require('@slack/bolt');

class SlackBot {
  constructor(token, signingSecret) {
    this.app = new App({
      token,
      signingSecret
    });
    
    this.setupListeners();
  }
  
  setupListeners() {
    // 监听消息
    this.app.message('hello', async ({ message, say }) => {
      await say(`Hello <@${message.user}>! 👋`);
    });
    
    // Slash Command
    this.app.command('/hello', async ({ command, ack, say }) => {
      await ack();
      await say('Hello from Slack bot!');
    });
    
    // Action (按钮点击)
    this.app.action('button_action', async ({ action, ack, say }) => {
      await ack();
      await say(`You clicked: ${action.text.text}`);
    });
  }
  
  start(port = 3000) {
    (async () => {
      await this.app.start(port);
      console.log(`⚡️ Bolt app started on port ${port}`);
    })();
  }
}
```

## c4-6 飞书开放平台

```javascript
const axios = require('axios');

class FeishuBot {
  constructor(appId, appSecret) {
    this.appId = appId;
    this.appSecret = appSecret;
    this.accessToken = null;
  }
  
  // 获取 access_token
  async getAccessToken() {
    const response = await axios.post(
      'https://open.feishu.cn/open-apis/auth/v3/tenant_access_token/internal',
      {
        app_id: this.appId,
        app_secret: this.appSecret
      }
    );
    
    this.accessToken = response.data.tenant_access_token;
    return this.accessToken;
  }
  
  // 发送文本消息
  async sendTextMessage(chatId, text) {
    await this.getAccessToken();
    
    const response = await axios.post(
      'https://open.feishu.cn/open-apis/im/v1/messages',
      {
        receive_id: chatId,
        msg_type: 'text',
        content: JSON.stringify({ text })
      },
      {
        headers: {
          'Authorization': `Bearer ${this.accessToken}`,
          'Content-Type': 'application/json'
        }
      }
    );
    
    return response.data;
  }
  
  // 发送富文本消息
  async sendRichTextMessage(chatId, elements) {
    await this.getAccessToken();
    
    const response = await axios.post(
      'https://open.feishu.cn/open-apis/im/v1/messages',
      {
        receive_id: chatId,
        msg_type: 'interactive',
        content: JSON.stringify({
          config: { wide_screen_mode: true },
          elements
        })
      },
      {
        headers: {
          'Authorization': `Bearer ${this.accessToken}`,
          'Content-Type': 'application/json'
        }
      }
    );
    
    return response.data;
  }
}

// 使用示例
const feishuBot = new FeishuBot('cli_xxx', 'secret_xxx');

await feishuBot.sendTextMessage('oc_xxx', '这是一条测试消息');

await feishuBot.sendRichTextMessage('oc_xxx', [
  {
    tag: 'div',
    text: {
      tag: 'lark_md',
      content: '**标题**\n正文内容'
    }
  },
  {
    tag: 'action',
    actions: [
      {
        tag: 'button',
        text: { tag: 'plain_text', content: '确认' },
        type: 'primary',
        value: { action: 'confirm' }
      }
    ]
  }
]);
```

## c4-7 WebSocket 协议详解

```javascript
const WebSocket = require('ws');

class WebSocketClient {
  constructor(url) {
    this.url = url;
    this.ws = null;
    this.reconnectInterval = 5000;
  }
  
  connect() {
    this.ws = new WebSocket(this.url);
    
    this.ws.on('open', () => {
      console.log('✅ Connected');
    });
    
    this.ws.on('message', (data) => {
      const message = JSON.parse(data);
      this.handleMessage(message);
    });
    
    this.ws.on('close', () => {
      console.log('🔴 Disconnected, reconnecting...');
      setTimeout(() => this.connect(), this.reconnectInterval);
    });
    
    this.ws.on('error', (error) => {
      console.error('❌ Error:', error);
    });
  }
  
  send(message) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    }
  }
  
  handleMessage(message) {
    // 处理不同类型的消息
    switch (message.type) {
      case 'ping':
        this.send({ type: 'pong' });
        break;
      case 'event':
        this.processEvent(message.data);
        break;
    }
  }
  
  processEvent(data) {
    // 业务逻辑处理
  }
}
```

## c4-8 OAuth 2.0 认证流程

```javascript
const express = require('express');
const axios = require('axios');

class OAuth2Handler {
  constructor(config) {
    this.clientId = config.clientId;
    this.clientSecret = config.clientSecret;
    this.redirectUri = config.redirectUri;
    this.authUrl = config.authUrl;
    this.tokenUrl = config.tokenUrl;
  }
  
  // 生成授权 URL
  getAuthorizationUrl(state) {
    const params = new URLSearchParams({
      client_id: this.clientId,
      redirect_uri: this.redirectUri,
      response_type: 'code',
      scope: 'read write',
      state
    });
    
    return `${this.authUrl}?${params.toString()}`;
  }
  
  // 换取 access_token
  async exchangeCode(code) {
    const response = await axios.post(this.tokenUrl, {
      client_id: this.clientId,
      client_secret: this.clientSecret,
      code,
      grant_type: 'authorization_code',
      redirect_uri: this.redirectUri
    });
    
    return response.data;
  }
  
  // 刷新 token
  async refreshToken(refreshToken) {
    const response = await axios.post(this.tokenUrl, {
      client_id: this.clientId,
      client_secret: this.clientSecret,
      refresh_token: refreshToken,
      grant_type: 'refresh_token'
    });
    
    return response.data;
  }
}

// Express 集成
const app = express();
const oauth = new OAuth2Handler(config);

app.get('/auth', (req, res) => {
  const state = generateRandomState();
  res.redirect(oauth.getAuthorizationUrl(state));
});

app.get('/callback', async (req, res) => {
  const { code, state } = req.query;
  
  try {
    const tokens = await oauth.exchangeCode(code);
    res.json(tokens);
  } catch (error) {
    res.status(500).send(error.message);
  }
});
```

---

**c4 模块核心内容完成！**
