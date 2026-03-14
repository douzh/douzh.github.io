# c2-5 多模型切换方案

## 1. 概述

在实际应用中，单一模型往往无法满足所有需求。通过智能路由和动态切换多个模型，可以在成本、性能和质量之间找到最佳平衡点。

## 2. 主流模型对比

### 2.1 综合能力矩阵

| 维度 | GPT-4-Turbo | Claude-3-Opus | Qwen-Max | Gemini-Pro | 说明 |
|------|-----------|--------------|----------|-----------|------|
| **推理能力** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | 复杂逻辑推理 |
| **代码能力** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | 编程任务 |
| **中文理解** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | 本土化程度 |
| **长文本** | 128K | 200K | 32K | 32K | 上下文窗口 |
| **响应速度** | ⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | 延迟表现 |
| **成本** | 💰💰💰💰 | 💰💰💰💰💰 | 💰💰💰 | 💰💰💰 | 相对价格 |
| **稳定性** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | SLA 保证 |

### 2.2 适用场景地图

```javascript
const scenarioModelMap = {
  // 高难度专业任务
  'complex_reasoning': {
    scenarios: ['数学证明', '法律分析', '医学诊断'],
    recommended: ['gpt-4-turbo', 'claude-3-opus'],
    fallback: ['gpt-4', 'claude-3-sonnet']
  },
  
  // 代码相关
  'coding': {
    scenarios: ['代码生成', 'Bug 调试', '架构设计'],
    recommended: ['gpt-4-turbo', 'claude-3-sonnet'],
    fallback: ['gpt-3.5-turbo', 'qwen-max']
  },
  
  // 内容创作
  'creative_writing': {
    scenarios: ['小说写作', '营销文案', '创意策划'],
    recommended: ['claude-3-opus', 'gpt-4-turbo'],
    fallback: ['claude-3-sonnet', 'gpt-3.5-turbo']
  },
  
  // 日常对话
  'casual_chat': {
    scenarios: ['客服问答', '闲聊', '简单咨询'],
    recommended: ['gpt-3.5-turbo', 'claude-3-haiku'],
    fallback: ['qwen-turbo', 'gemini-pro']
  },
  
  // 中文场景
  'chinese_context': {
    scenarios: ['古诗翻译', '政策解读', '文化分析'],
    recommended: ['qwen-max', 'ernie-bot-4'],
    fallback: ['gpt-4-turbo', 'claude-3']
  },
  
  // 批量处理
  'bulk_processing': {
    scenarios: ['数据标注', '内容审核', '批量翻译'],
    recommended: ['claude-3-haiku', 'gpt-3.5-turbo'],
    fallback: ['qwen-turbo']
  }
};
```

## 3. 路由策略设计

### 3.1 基于规则的路由

```javascript
class RuleBasedRouter {
  constructor() {
    this.rules = [
      {
        name: '高优先级用户',
        condition: (context) => context.userLevel === 'vip',
        model: 'gpt-4-turbo'
      },
      {
        name: '超长文本',
        condition: (context) => context.inputLength > 50000,
        model: 'claude-3-200k'
      },
      {
        name: '代码任务',
        condition: (context) => 
          context.taskType === 'coding' || 
          context.message.includes('代码'),
        model: 'gpt-4-turbo'
      },
      {
        name: '预算限制',
        condition: (context) => context.dailyBudget < 10,
        model: 'claude-3-haiku'
      },
      {
        name: '默认',
        condition: () => true,
        model: 'gpt-3.5-turbo'
      }
    ];
  }
  
  selectModel(context) {
    for (const rule of this.rules) {
      if (rule.condition(context)) {
        console.log(`Rule matched: ${rule.name}, using ${rule.model}`);
        return rule.model;
      }
    }
    
    return 'gpt-3.5-turbo'; // 兜底
  }
}
```

### 3.2 基于评分的路由

```javascript
class ScoringRouter {
  score(model, requirements) {
    const modelInfo = this.models[model];
    let score = modelInfo.baseScore;
    
    // 成本调整
    if (requirements.costSensitive) {
      score -= modelInfo.costPerToken * 1000;
    }
    
    // 速度调整
    if (requirements.urgent) {
      score += (modelInfo.speed - 50) * 0.5;
    }
    
    // 能力匹配
    if (requirements.requiredCapabilities) {
      const matchCount = requirements.requiredCapabilities.filter(
        cap => modelInfo.strengths.includes(cap)
      ).length;
      score += (matchCount / requirements.requiredCapabilities.length) * 20;
    }
    
    return score;
  }
  
  selectBest(requirements) {
    let bestModel = null;
    let bestScore = -Infinity;
    
    for (const model of Object.keys(this.models)) {
      const score = this.score(model, requirements);
      if (score > bestScore) {
        bestScore = score;
        bestModel = model;
      }
    }
    
    return bestModel;
  }
}
```

## 4. 降级与容错

### 4.1 多级降级链

```javascript
class FallbackChain {
  constructor() {
    this.chains = {
      premium: [
        'gpt-4-turbo',
        'gpt-4',
        'claude-3-sonnet',
        'gpt-3.5-turbo'
      ],
      costEffective: [
        'claude-3-haiku',
        'gpt-3.5-turbo',
        'qwen-turbo'
      ]
    };
  }
  
  async execute(prompt, chainName = 'premium') {
    const chain = this.chains[chainName];
    let lastError;
    
    for (let i = 0; i < chain.length; i++) {
      const model = chain[i];
      
      try {
        const result = await llm.generate(prompt, { model });
        result._usedModel = model;
        result._fallbackCount = i;
        return result;
        
      } catch (error) {
        lastError = error;
        console.warn(`${model} failed: ${error.message}`);
        
        if (i === chain.length - 1) {
          throw new Error(`All models failed. Last: ${error.message}`);
        }
      }
    }
  }
}
```

### 4.2 健康检查

```javascript
class ModelHealthMonitor {
  async checkAllModels() {
    const models = ['gpt-4-turbo', 'gpt-3.5-turbo', 'claude-3-haiku'];
    
    for (const model of models) {
      try {
        const start = Date.now();
        await llm.generate('Hello', { model, timeout: 5000 });
        const latency = Date.now() - start;
        
        this.healthStatus.set(model, {
          status: 'healthy',
          latency,
          lastCheck: Date.now()
        });
      } catch (error) {
        this.healthStatus.set(model, {
          status: 'unhealthy',
          latency: Infinity,
          lastCheck: Date.now()
        });
      }
    }
  }
  
  getBestModel() {
    const healthy = Array.from(this.healthStatus.entries())
      .filter(([_, s]) => s.status === 'healthy');
    
    if (healthy.length === 0) return null;
    
    // 返回延迟最低的
    return healthy.reduce((best, current) => {
      return current[1].latency < best[1].latency ? current : best;
    })[0];
  }
}
```

## 5. A/B 测试框架

### 5.1 流量分配

```javascript
class ABTestManager {
  assignVariant(experimentName, userId) {
    const experiment = this.experiments[experimentName];
    const hash = this.hashUserId(userId);
    
    let cumulative = 0;
    for (const [variant, config] of Object.entries(experiment.variants)) {
      cumulative += config.traffic;
      if (hash < cumulative) {
        return { variant, config };
      }
    }
    
    return { variant: 'A', config: experiment.variants.A };
  }
  
  recordResult(experimentName, variant, metrics) {
    const key = `${experimentName}_${variant}`;
    if (!this.results.has(key)) {
      this.results.set(key, []);
    }
    this.results.get(key).push({
      timestamp: Date.now(),
      ...metrics
    });
  }
  
  analyzeResults(experimentName) {
    const stats = {};
    for (const variant of Object.keys(this.experiments[experimentName].variants)) {
      const data = this.results.get(`${experimentName}_${variant}`) || [];
      stats[variant] = {
        sampleSize: data.length,
        averages: {}
      };
      // 计算各指标平均值
    }
    return stats;
  }
}
```

## 6. 实战案例

### 6.1 智能客服系统

```javascript
class CustomerServiceRouter {
  constructor() {
    this.router = new RuleBasedRouter();
    this.monitor = new ModelHealthMonitor();
  }
  
  async handleQuery(query, context) {
    // 简单问题用便宜模型
    if (this.isSimpleQuery(query)) {
      return this.executeWithModel('claude-3-haiku', query, context);
    }
    
    // 复杂问题或 VIP 用户用好模型
    if (context.userLevel === 'vip' || this.isComplexQuery(query)) {
      const bestModel = this.monitor.getBestModel() || 'gpt-4-turbo';
      return this.executeWithModel(bestModel, query, context);
    }
    
    // 默认使用性价比模型
    return this.executeWithModel('gpt-3.5-turbo', query, context);
  }
  
  isSimpleQuery(query) {
    const simplePatterns = [
      /你好/, /谢谢/, /再见/,
      /多少钱/, /什么时候/, /在哪里/
    ];
    return simplePatterns.some(p => p.test(query));
  }
  
  isComplexQuery(query) {
    return query.length > 100 || /[;,.]/.test(query);
  }
}
```

### 6.2 成本优化案例

某电商客服系统通过智能路由实现成本优化：

**优化前：**
- 全部使用 GPT-4-Turbo
- 日均请求：10,000 次
- 平均 Token：800
- 月度成本：$2,400

**优化后：**
```javascript
const routing = {
  '简单问候': 'claude-3-haiku',     // 30% 流量
  '常见问题': 'gpt-3.5-turbo',      // 50% 流量
  '投诉建议': 'gpt-4-turbo',        // 15% 流量
  'VIP 服务': 'gpt-4-turbo'          // 5% 流量
};
```

**效果：**
- 月度成本：$720（降低 70%）
- 平均响应时间：从 3.2s降至 1.8s
- 用户满意度：保持 95%+

## 7. 最佳实践

✅ **DO - 推荐做法：**

1. **建立模型分级制度**
   ```
   Tier 1 (高端): GPT-4, Claude-Opus - 复杂任务
   Tier 2 (中端): GPT-3.5, Claude-Sonnet - 常规任务
   Tier 3 (经济): Haiku, Qwen-Turbo - 简单任务
   ```

2. **实施健康监控**
   - 每分钟检查一次各模型状态
   - 自动切换到健康模型
   - 记录故障历史用于分析

3. **持续 A/B 测试**
   - 定期对比不同模型效果
   - 根据数据调整路由策略
   - 关注用户反馈和满意度

4. **建立降级机制**
   - 定义清晰的降级链
   - 设置合理的超时时间
   - 保留人工介入通道

❌ **DON'T - 避免做法：**

1. 不要过度依赖单一模型
2. 不要忽略小模型的能力
3. 不要为了省钱牺牲核心体验
4. 不要忘记监控和告警
5. 不要假设模型永远稳定

---

**上一节：** [c2-4 Token 优化策略](./c2-4-token-optimization.md)  
**c2 模块完**
