# c4-5 钉钉机器人开发实战

## 1. 概述

钉钉机器人支持群聊消息推送、交互式卡片、工作台应用等多种交互方式。本章详解两种主要开发模式。

## 2. 群聊机器人（简单模式）

### 2.1 创建机器人

```javascript
// Step 1: 在钉钉群设置中添加机器人
// 群设置 → 智能群助手 → 添加机器人 → 自定义
// 获取 Webhook URL

const DINGTALK_WEBHOOK = 'https://oapi.dingtalk.com/robot/send?access_token=YOUR_ACCESS_TOKEN';
const SECRET = 'SECxxxxxxxx'; // 加签密钥（可选但推荐）
```

### 2.2 基础消息发送

```javascript
const axios = require('axios');
const crypto = require('crypto');

class DingTalkBot {
  constructor(webhook, secret) {
    this.webhook = webhook;
    this.secret = secret;
  }
  
  // 生成签名（如果使用加签）
  generateSignature() {
    if (!this.secret) return '';
    
    const timestamp = Date.now().toString();
    const stringToSign = `${timestamp}\n${this.secret}`;
    
    const signature = crypto
      .createHmac('sha256', this.secret)
      .update(stringToSign, 'utf8')
      .digest()
      .toString('base64');
    
    return encodeURIComponent(signature);
  }
  
  // 发送消息
  async sendMessage(message) {
    const url = new URL(this.webhook);
    
    // 添加签名和时间戳
    if (this.secret) {
      const timestamp = Date.now().toString();
      const sign = this.generateSignature();
      url.searchParams.append('timestamp', timestamp);
      url.searchParams.append('sign', sign);
    }
    
    try {
      const response = await axios.post(url.toString(), message, {
        headers: { 'Content-Type': 'application/json; charset=utf-8' }
      });
      
      if (response.data.errcode !== 0) {
        throw new Error(`DingTalk API error: ${response.data.errmsg}`);
      }
      
      return response.data;
    } catch (error) {
      console.error('Failed to send message:', error);
      throw error;
    }
  }
  
  // 发送文本消息
  async sendText(content, mentions = null) {
    return await this.sendMessage({
      msgtype: 'text',
      text: {
        content,
        mentioned_mobile_list: mentions // ['13800000000', 'all']
      }
    });
  }
  
  // 发送 Markdown 消息
  async sendMarkdown(title, text, mentions = null) {
    return await this.sendMessage({
      msgtype: 'markdown',
      markdown: {
        title,
        text
      },
      at: {
        atMobiles: mentions,
        isAtAll: mentions && mentions.includes('all')
      }
    });
  }
  
  // 发送链接消息
  async sendLink(title, text, picUrl, messageUrl) {
    return await this.sendMessage({
      msgtype: 'link',
      link: {
        title,
        text,
        picUrl,
        messageUrl
      }
    });
  }
  
  // 发送卡片消息（ActionCard）
  async sendActionCard(title, text, btnOrientation, singleTitle, singleURL) {
    return await this.sendMessage({
      msgtype: 'action_card',
      actionCard: {
        title,
        text,
        btnOrientation, // 0: 竖直，1: 水平
        singleTitle,
        singleURL
      }
    });
  }
  
  // 发送 OA 消息
  async sendOA(head, body) {
    return await this.sendMessage({
      msgtype: 'oa',
      oa: {
        message_url: 'dingtalk://dingtalkclient/page/link',
        head,
        body
      }
    });
  }
}

// 使用示例
const bot = new DingTalkBot(DINGTALK_WEBHOOK, SECRET);

// 发送文本消息
await bot.sendText('大家好，这是一条测试消息');

// 发送 Markdown 并@所有人
await bot.sendMarkdown(
  '周报通知',
  `## 周报提醒

请各位及时提交本周工作周报。

截止时间：**本周五 18:00**`,
  ['all']
);

// 发送链接消息
await bot.sendLink(
  '新产品发布',
  '点击查看我们最新的产品介绍',
  'https://example.com/image.jpg',
  'https://example.com/product'
);
```

## 3. 交互式卡片（高级模式）

### 3.1 独立卡片消息

```javascript
class InteractiveDingTalkBot extends DingTalkBot {
  // 发送按钮卡片
  async sendButtonCard(title, text, buttons) {
    return await this.sendMessage({
      msgtype: 'interactive',
      interactive: {
        type: 'btn',
        version: '1.0',
        card: {
          header: {
            title: {
              text: title,
              template: 'blue' // blue, red, green, etc.
            }
          },
          contents: [
            {
              field: {
                text: {
                  content: text,
                  template: 'gray'
                }
              }
            },
            {
              actions: buttons.map(btn => ({
                type: 'button',
                text: {
                  content: btn.title,
                  template: btn.template || 'blue'
                },
                actionType: btn.actionType, // link, outChat, callback
                url: btn.url,
                data: btn.data // 回调数据
              }))
            }
          ]
        }
      }
    });
  }
  
  // 发送列表卡片
  async sendListCard(title, items) {
    return await this.sendMessage({
      msgtype: 'interactive',
      interactive: {
        type: 'list',
        version: '1.0',
        card: {
          header: {
            title: {
              text: title,
              template: 'blue'
            }
          },
          contents: [
            {
              list: items.map(item => ({
                type: 'image_text',
                image: {
                  url: item.image
                },
                text: {
                  content: item.text,
                  template: 'black'
                },
                desc: {
                  content: item.desc,
                  template: 'gray'
                }
              }))
            }
          ]
        }
      }
    });
  }
  
  // 发送表单卡片
  async sendFormCard(title, formData) {
    return await this.sendMessage({
      msgtype: 'interactive',
      interactive: {
        type: 'form',
        version: '1.0',
        card: {
          header: {
            title: {
              text: title,
              template: 'blue'
            }
          },
          contents: [
            {
              form: Object.entries(formData).map(([label, value]) => ({
                label: {
                  text: label,
                  template: 'gray'
                },
                value: {
                  text: value,
                  template: 'black'
                }
              }))
            }
          ]
        }
      }
    });
  }
}

// 使用示例
const interactiveBot = new InteractiveDingTalkBot(DINGTALK_WEBHOOK, SECRET);

// 发送按钮卡片
await interactiveBot.sendButtonCard(
  '会议邀请',
  '下午 3 点召开项目评审会议，是否参加？',
  [
    { title: '✅ 参加', template: 'green', actionType: 'callback', data: '{"action":"accept"}' },
    { title: '❌ 请假', template: 'red', actionType: 'callback', data: '{"action":"decline"}' }
  ]
);

// 发送列表卡片（产品列表）
await interactiveBot.sendListCard(
  '新品推荐',
  [
    { 
      image: 'https://example.com/product1.jpg',
      text: '智能手表 Pro',
      desc: '¥1999 | 已售 1000+'
    },
    {
      image: 'https://example.com/product2.jpg',
      text: '无线耳机 Air',
      desc: '¥899 | 已售 500+'
    }
  ]
);

// 发送表单卡片（审批结果）
await interactiveBot.sendFormCard(
  '请假审批结果',
  {
    '申请人': '张三',
    '请假类型': '年假',
    '开始时间': '2024-03-10 09:00',
    '结束时间': '2024-03-12 18:00',
    '审批状态': '✅ 已通过',
    '审批人': '李四经理'
  }
);
```

## 4. 回调处理（Express 服务器）

```javascript
const express = require('express');
const bodyParser = require('body-parser');

class CallbackDingTalkBot extends InteractiveDingTalkBot {
  constructor(webhook, secret, appKey, appSecret) {
    super(webhook, secret);
    this.appKey = appKey;
    this.appSecret = appSecret;
    this.app = express();
    this.setupMiddleware();
    
    // 存储回调处理器
    this.callbackHandlers = new Map();
  }
  
  setupMiddleware() {
    this.app.use(bodyParser.json());
    
    // 接收回调
    this.app.post('/dingtalk/callback', async (req, res) => {
      try {
        const { encrypt, timestamp, nonce, token } = req.query;
        const body = req.body;
        
        // 验证签名（简化版，实际需完整实现）
        const isValid = this.verifySignature(encrypt, timestamp, nonce, token, body);
        
        if (!isValid) {
          return res.status(401).json({ code: 401, message: 'Invalid signature' });
        }
        
        // 解密数据
        const decryptedData = this.decrypt(encrypt);
        
        // 处理回调
        const result = await this.handleCallback(decryptedData);
        
        res.json({
          code: 200,
          success: true,
          data: result
        });
      } catch (error) {
        console.error('Callback error:', error);
        res.status(500).json({ code: 500, message: error.message });
      }
    });
  }
  
  // 注册回调处理器
  on(actionType, handler) {
    this.callbackHandlers.set(actionType, handler);
  }
  
  async handleCallback(data) {
    const { actionType, data: actionData } = JSON.parse(data);
    
    const handler = this.callbackHandlers.get(actionType);
    
    if (handler) {
      return await handler(JSON.parse(actionData));
    } else {
      console.log('No handler for actionType:', actionType);
      return { handled: false };
    }
  }
  
  verifySignature(encrypt, timestamp, nonce, token, body) {
    // 实际项目中需要完整实现钉钉的签名验证逻辑
    return true;
  }
  
  decrypt(encrypt) {
    // 使用 appSecret 解密
    return 'decrypted_data';
  }
  
  startServer(port = 3000) {
    this.app.listen(port, () => {
      console.log(`🚀 DingTalk callback server running on port ${port}`);
    });
  }
}

// 使用示例
const callbackBot = new CallbackDingTalkBot(
  DINGTALK_WEBHOOK,
  SECRET,
  'appKey',
  'appSecret'
);

// 注册按钮点击处理器
callbackBot.on('accept', async (data) => {
  console.log('用户接受会议邀请:', data);
  await callbackBot.sendText('✅ 已确认参加，稍后会发送会议日历邀请');
  return { success: true };
});

callbackBot.on('decline', async (data) => {
  console.log('用户拒绝会议邀请:', data);
  await callbackBot.sendText('收到您的请假申请，会另行安排时间');
  return { success: true };
});

// 启动回调服务器
callbackBot.startServer(3000);
```

---

**下一节：** [c4-6 飞书机器人](./c4-6-feishu-bot.md)  
**上一节：** [c4-1 Telegram Bot](./c4-1-telegram-bot.md)
