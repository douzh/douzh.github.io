# c8-1 自然语言理解（NLU）设计

## 1. 概述

自然语言理解（Natural Language Understanding, NLU）是 AI智能体理解用户输入的核心能力。本章详解意图分类、实体抽取、情感分析等关键技术。

## 2. 意图分类

### 2.1 基于规则的意图匹配

```javascript
class RuleBasedIntentClassifier {
  constructor() {
    this.patterns = new Map();
    this.initPatterns();
  }
  
  initPatterns() {
    // 定义意图和匹配规则
    this.patterns.set('greeting', [
      /你好 | 您好|hello|hi|hey/i,
      /早上好 | 下午好 | 晚上好/i
    ]);
    
    this.patterns.set('query_weather', [
      /天气怎么样 | 什么天气 | 温度多少/i,
      /(北京 | 上海 | 广州 | 深圳).*天气/
    ]);
    
    this.patterns.set('send_email', [
      /发邮件 | 发送邮件 | 写信/i,
      /联系.*通过邮件/i
    ]);
    
    this.patterns.set('set_reminder', [
      /提醒我 | 设置提醒 | 定闹钟/i,
      /别忘了 | 记得/i
    ]);
  }
  
  classify(text) {
    const results = [];
    
    for (const [intent, patterns] of this.patterns.entries()) {
      for (const pattern of patterns) {
        if (pattern.test(text)) {
          results.push({
            intent,
            confidence: 0.8, // 规则匹配固定置信度
            matchedPattern: pattern.toString()
          });
          break;
        }
      }
    }
    
    // 返回置信度最高的
    return results.sort((a, b) => b.confidence - a.confidence)[0] || 
           { intent: 'unknown', confidence: 0 };
  }
}

// 使用示例
const classifier = new RuleBasedIntentClassifier();

console.log(classifier.classify('你好，请问北京天气怎么样？'));
// 输出：{ intent: 'query_weather', confidence: 0.8, matchedPattern: '/(北京 | 上海 | 广州 | 深圳).*天气/' }

console.log(classifier.classify('帮我给张三发邮件'));
// 输出：{ intent: 'send_email', confidence: 0.8 }
```

### 2.2 基于机器学习的意图分类

```javascript
class MLIntentClassifier {
  constructor() {
    this.model = null;
    this.vectorizer = null;
    this.intents = [];
  }
  
  // 训练模型（离线）
  async train(trainingData) {
    // trainingData: [{ text: string, intent: string }]
    
    const tfidf = require('natural').TfIdf;
    const classifier = require('natural').BayesClassifier;
    
    this.vectorizer = tfidf();
    this.model = classifier;
    
    // 准备数据
    const texts = [];
    const labels = [];
    const intentSet = new Set();
    
    for (const sample of trainingData) {
      texts.push(sample.text);
      labels.push(sample.intent);
      intentSet.add(sample.intent);
    }
    
    this.intents = Array.from(intentSet);
    
    // 添加训练样本
    for (let i = 0; i < texts.length; i++) {
      this.model.addDocument(texts[i], labels[i]);
    }
    
    // 训练
    await new Promise(resolve => this.model.train(resolve));
    
    console.log(`✅ Trained on ${texts.length} samples, ${this.intents.length} intents`);
  }
  
  // 预测意图
  predict(text) {
    if (!this.model) {
      throw new Error('Model not trained');
    }
    
    const classifications = this.model.getClassifications(text);
    
    // 返回 top-3
    return classifications.slice(0, 3).map(c => ({
      intent: c.label,
      confidence: c.value,
      rank: classifications.indexOf(c) + 1
    }));
  }
  
  // 保存模型
  save(filePath) {
    const data = {
      model: this.model.toJSON(),
      intents: this.intents
    };
    
    fs.writeFileSync(filePath, JSON.stringify(data));
  }
  
  // 加载模型
  load(filePath) {
    const data = JSON.parse(fs.readFileSync(filePath, 'utf-8'));
    this.intents = data.intents;
    // 重新加载模型...
  }
}

// 使用示例
const mlClassifier = new MLIntentClassifier();

// 训练数据
const trainingData = [
  { text: '你好', intent: 'greeting' },
  { text: '您好', intent: 'greeting' },
  { text: '北京天气怎么样', intent: 'query_weather' },
  { text: '上海明天什么天气', intent: 'query_weather' },
  { text: '帮我发邮件', intent: 'send_email' },
  { text: '给李四写信', intent: 'send_email' },
  { text: '提醒我开会', intent: 'set_reminder' },
  { text: '定个闹钟', intent: 'set_reminder' }
];

await mlClassifier.train(trainingData);

// 预测
const predictions = mlClassifier.predict('请问广州的天气如何？');
console.log(predictions);
/*
[
  { intent: 'query_weather', confidence: 0.92, rank: 1 },
  { intent: 'greeting', confidence: 0.05, rank: 2 },
  { intent: 'send_email', confidence: 0.02, rank: 3 }
]
*/
```

### 2.3 基于深度学习的意图分类

```javascript
// 使用 Transformers.js (Bert)
import { pipeline } from '@xenova/transformers';

class DeepIntentClassifier {
  constructor() {
    this.classifier = null;
  }
  
  async init() {
    // 加载预训练模型
    this.classifier = await pipeline(
      'zero-shot-classification',
      'Xenova/bert-base-uncased'
    );
    
    this.candidateLabels = [
      'greeting',
      'query_weather',
      'send_email',
      'set_reminder',
      'query_news',
      'play_music',
      'calculate',
      'translate',
      'other'
    ];
  }
  
  async classify(text) {
    const output = await this.classifier(text, this.candidateLabels, {
      topk: 5,
      hypothesis_template: "This text is about {}."
    });
    
    return output.labels.map((label, index) => ({
      intent: label,
      confidence: output.scores[index],
      rank: index + 1
    }));
  }
  
  // 多标签分类
  async classifyMultiLabel(text) {
    const output = await this.classifier(text, this.candidateLabels, {
      topk: null, // 返回所有
      multi_label: true
    });
    
    // 过滤高置信度的标签
    return output.labels.filter((_, i) => output.scores[i] > 0.5)
      .map((label, i) => ({
        intent: label,
        confidence: output.scores[i]
      }));
  }
}

// 使用示例
const deepClassifier = new DeepIntentClassifier();
await deepClassifier.init();

const result = await deepClassifier.classify('明天北京天气不错，适合出去玩');
console.log(result);
/*
[
  { intent: 'query_weather', confidence: 0.75 },
  { intent: 'other', confidence: 0.20 },
  { intent: 'greeting', confidence: 0.03 }
]
*/
```

## 3. 实体抽取

### 3.1 基于规则的实体提取

```javascript
class EntityExtractor {
  constructor() {
    this.patterns = {
      // 日期时间
      datetime: [
        /\d{4}年\d{1,2}月\d{1,2}日/g,
        /今天 | 明天 | 后天 | 昨天/g,
        /\d{1,2}[点时]:\d{2}/g,
        /上午 | 下午 | 晚上/g
      ],
      
      // 地点
      location: [
        /北京市 | 上海市 | 广州市 | 深圳市/g,
        /[A-Z][a-z]+市/g,
        /中国.*省.*市/g
      ],
      
      // 人名
      person: [
        /张 [三四五六七八九十]/g,
        /李 [三四五六七八九十]/g,
        /王 [三四五六七八九十]/g,
        /[A-Z][a-z]+ [A-Z][a-z]+/g  // 英文名
      ],
      
      // 邮箱
      email: [
        /[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/g
      ],
      
      // 电话号码
      phone: [
        /1[3-9]\d{9}/g,
        /\d{3,4}-\d{7,8}/g
      ],
      
      // 数字
      number: [
        /\d+(\.\d+)?/g,
        /一百 | 一千 | 一万/g
      ]
    };
  }
  
  extract(text) {
    const entities = [];
    
    for (const [type, patterns] of Object.entries(this.patterns)) {
      for (const pattern of patterns) {
        let match;
        while ((match = pattern.exec(text)) !== null) {
          entities.push({
            type,
            value: match[0],
            start: match.index,
            end: match.index + match[0].length,
            confidence: 0.9
          });
        }
      }
    }
    
    // 按位置排序
    return entities.sort((a, b) => a.start - b.start);
  }
}

// 使用示例
const extractor = new EntityExtractor();

const entities = extractor.extract('明天下午 3 点给张三发邮件到 zhangsan@example.com');
console.log(entities);
/*
[
  { type: 'datetime', value: '明天', start: 0, end: 2, confidence: 0.9 },
  { type: 'datetime', value: '下午 3 点', start: 2, end: 6, confidence: 0.9 },
  { type: 'person', value: '张三', start: 7, end: 9, confidence: 0.9 },
  { type: 'email', value: 'zhangsan@example.com', start: 12, end: 32, confidence: 0.9 }
]
*/
```

### 3.2 基于 CRF 的序列标注

```javascript
class CRFEntityExtractor {
  constructor() {
    this.model = null;
  }
  
  // 特征工程
  extractFeatures(tokens, index) {
    const token = tokens[index];
    const prev = tokens[index - 1] || '<BOS>';
    const next = tokens[index + 1] || '<EOS>';
    
    return {
      bias: 1,
      word: token,
      wordLower: token.toLowerCase(),
      prefix2: token.slice(0, 2),
      prefix3: token.slice(0, 3),
      suffix2: token.slice(-2),
      suffix3: token.slice(-3),
      isDigit: /\d/.test(token),
      isUpper: /^[A-Z]+$/.test(token),
      isTitle: /^[A-Z][a-z]+$/.test(token),
      prevWord: prev,
      nextWord: next,
      prevIsDigit: /\d/.test(prev),
      nextIsDigit: /\d/.test(next)
    };
  }
  
  // 训练（简化版）
  async train(trainingData) {
    // trainingData: [{ tokens: [], labels: [] }]
    const crf = require('crfsuite');
    
    const X = []; // 特征
    const y = []; // 标签
    
    for (const sentence of trainingData) {
      const features = [];
      const labels = [];
      
      for (let i = 0; i < sentence.tokens.length; i++) {
        features.push(this.extractFeatures(sentence.tokens, i));
        labels.push(sentence.labels[i]);
      }
      
      X.push(features);
      y.push(labels);
    }
    
    // 训练 CRF 模型
    const trainer = new crf.Trainer();
    trainer.setParams({
      c1: 0.1,
      c2: 0.01,
      max_iterations: 100
    });
    
    await trainer.train(X, y);
    this.model = trainer.getModel();
  }
  
  // 预测
  predict(tokens) {
    const features = tokens.map((_, i) => 
      this.extractFeatures(tokens, i)
    );
    
    return this.model.predict([features])[0];
  }
}

// BIO 标注格式示例
const trainingData = [
  {
    tokens: ['明', '天', '去', '北', '京'],
    labels: ['B-time', 'E-time', 'O', 'B-loc', 'E-loc']
  },
  {
    tokens: ['给', '张', '三', '发', '邮', '件'],
    labels: ['O', 'B-per', 'E-per', 'O', 'O', 'O']
  }
];
```

## 4. 情感分析

### 4.1 基于词典的情感计算

```javascript
class SentimentAnalyzer {
  constructor() {
    this.positiveWords = new Set([
      '好', '棒', '优秀', '出色', '完美',
      '喜欢', '爱', '满意', '高兴', '开心'
    ]);
    
    this.negativeWords = new Set([
      '差', '烂', '糟糕', '失望', '讨厌',
      '恨', '不满', '生气', '难过', '伤心'
    ]);
    
    this.negations = new Set(['不', '没', '无', '非', '未']);
    
    this.intensifiers = new Set(['非常', '特别', '极其', '十分', '很']);
  }
  
  analyze(text) {
    const words = this.tokenize(text);
    let score = 0;
    let negationActive = false;
    let intensity = 1;
    
    for (let i = 0; i < words.length; i++) {
      const word = words[i];
      
      // 检查否定词
      if (this.negations.has(word)) {
        negationActive = true;
        continue;
      }
      
      // 检查程度副词
      if (this.intensifiers.has(word)) {
        intensity = 1.5;
        continue;
      }
      
      // 计算情感分
      if (this.positiveWords.has(word)) {
        score += negationActive ? -intensity : intensity;
      } else if (this.negativeWords.has(word)) {
        score += negationActive ? intensity : -intensity;
      }
      
      // 重置状态
      negationActive = false;
      intensity = 1;
    }
    
    // 归一化到 [-1, 1]
    const normalizedScore = Math.tanh(score / 10);
    
    return {
      score: normalizedScore,
      label: normalizedScore > 0.2 ? 'positive' : 
             normalizedScore < -0.2 ? 'negative' : 'neutral',
      confidence: Math.abs(normalizedScore)
    };
  }
  
  tokenize(text) {
    // 简化的分词（实际应使用专业分词工具）
    return text.split(/[\s,，.。!?！？]+/);
  }
}

// 使用示例
const analyzer = new SentimentAnalyzer();

console.log(analyzer.analyze('这个产品非常好用，我非常喜欢'));
// { score: 0.76, label: 'positive', confidence: 0.76 }

console.log(analyzer.analyze('服务太差了，我很不满意'));
// { score: -0.76, label: 'negative', confidence: 0.76 }

console.log(analyzer.analyze('今天天气不错'));
// { score: 0.38, label: 'positive', confidence: 0.38 }
```

### 4.2 基于深度学习的情感分析

```javascript
import { pipeline } from '@xenova/transformers';

class DeepSentimentAnalyzer {
  constructor() {
    this.analyzer = null;
  }
  
  async init() {
    this.analyzer = await pipeline(
      'sentiment-analysis',
      'Xenova/distilbert-base-uncased-finetuned-sst-2-english'
    );
  }
  
  async analyze(text) {
    const result = await this.analyzer(text);
    
    return {
      label: result[0].label.toLowerCase(), // 'POSITIVE' or 'NEGATIVE'
      score: result[0].score,
      confidence: result[0].score
    };
  }
  
  // 细粒度情感分析（5 类）
  async analyzeFineGrained(text) {
    const result = await this.analyzer(text);
    
    const score = result[0].score;
    const label = result[0].label;
    
    // 映射到 5 类
    let fineLabel, fineScore;
    
    if (label === 'POSITIVE') {
      if (score > 0.9) {
        fineLabel = 'very_positive';
        fineScore = score;
      } else {
        fineLabel = 'slightly_positive';
        fineScore = score;
      }
    } else {
      if (score > 0.9) {
        fineLabel = 'very_negative';
        fineScore = 1 - score;
      } else {
        fineLabel = 'slightly_negative';
        fineScore = 1 - score;
      }
    }
    
    return {
      label: fineLabel,
      score: fineScore,
      confidence: fineScore
    };
  }
}
```

---

**下一节：** [c8-2 多轮对话管理](./c8-2-multi-turn-dialog.md)  
**上一节：** [c8 UX 设计模块](./README.md)
