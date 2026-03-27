# ATS 4.0 多愿景并行管理指南

## 🚀 系统概述

**ATS 4.0** - AI 自主驱动的任务管理系统（支持多愿景并行管理）

### 核心升级

相比 3.0 版本，4.0 版本新增：
- ✅ **多愿景并行**: 同时管理多个独立或关联的愿景
- ✅ **投资组合管理**: 统一视角下的资源优化配置
- ✅ **跨愿景协作**: 愿景间的依赖管理和资源共享
- ✅ **动态资源分配**: 根据优先级和进度智能调配资源

---

## 📁 目录结构（4.0 升级版）

```
ats-4.0/
├── config.json                     # 系统配置（含多愿景配置）
├── state.json                      # 全局状态 + 各愿景状态
├── vision_template.md              # 愿景模板
├── readme.md                       # 系统说明
├── ats-portfolio-management.md     # 多愿景管理指南（本文档）
│
├── visions/                        # 【新增】各愿景独立工作区
│   ├── V001_vision_name/           # 愿景 1 工作区
│   │   ├── vision.md               # 愿景文档
│   │   ├── problems.md             # 问题拆解
│   │   ├── goals.md                # 目标定义
│   │   └── tasks.md                # 任务列表
│   ├── V002_vision_name/           # 愿景 2 工作区
│   └── ...
│
├── hub/                     # 【新增】投资组合视图
│   ├── overview.md       # 总览仪表板
│   └── resources.md      # 资源配置表
│
├── docs/                           # 六类工程文档（支持按愿景分区）
│   ├── brd/
│   │   ├── brd_V001.md             # 愿景 1 的 BRD
│   │   ├── brd_V002.md             # 愿景 2 的 BRD
│   │   └── brd_global.md           # 全局 BRD（可选）
│   ├── pdd/
│   ├── ad/
│   ├── hld/
│   ├── dd/
│   └── dp/
│
├── plans/                          # 执行计划（按愿景 + 全局）
│   ├── plan_V001_001.md            # 愿景 1 的计划
│   ├── plan_V002_001.md            # 愿景 2 的计划
│   └── plan_global_001.md          # 跨愿景计划
│
├── progress/                       # 进度记录（按愿景）
│   ├── progress_V001.md
│   ├── progress_V002.md
│   └── progress_global.md
│
├── completed/                      # 已完成归档（按愿景）
│   ├── V001/
│   └── V002/
│
└── archive/                        # 历史版本（按愿景）
    ├── V001/
    └── V002/
```

---

## 🎯 核心概念

### 1. 愿景 (Vision)

**定义**: 一个独立的战略目标或项目方向

**特点**:
- 有明确的愿景 ID（如 V001, V002）
- 独立的五层架构（Vision→Problems→Goals→Tasks→Plans）
- 独立的六类文档体系
- 可以处于 active/paused/completed 状态

**示例**:
- V001: AI 助手开发
- V002: 知识库管理系统
- V003: 数据分析平台

### 2. 投资组合 (Portfolio)

**定义**: 所有愿景的集合视图，用于全局资源优化

**功能**:
- 查看哪些愿景正在进行
- 资源如何分配
- 愿景间的依赖关系
- 整体进度和健康度

### 3. 上下文切换 (Context Switching)

**定义**: AI 在不同愿景之间切换工作的机制

**原则**:
- 一次只执行一个愿景的任务
- 切换时保存当前上下文
- 快速加载新愿景的上下文

---

## 🚀 快速开始

### 场景 1: 启动第一个愿景

```bash
# Step 1: 初始化系统
cd ats-4.0
cat config.json

# Step 2: 创建愿景工作区
New-Item -ItemType Directory -Path "visions/V001_your_vision_name"

# Step 3: 复制愿景模板
Copy-Item vision_template.md visions/V001_your_vision_name/vision.md

# Step 4: 编辑愿景文档
# 填写愿景 ID、名称、目标等信息

# Step 5: 更新全局状态
# 编辑 state.json，在 vision_portfolio.visions 中添加 V001
```

### 场景 2: 添加新愿景

```bash
# Step 1: 创建新愿景工作区
New-Item -ItemType Directory -Path "visions/V002_new_vision"

# Step 2: 初始化愿景文档
# 复制模板并填充内容

# Step 3: 分析依赖关系
# 检查是否与现有愿景有依赖或协同关系

# Step 4: 资源配置
# 评估是否需要从其他愿景调配资源

# Step 5: 更新投资组合
# 更新 state.json 和 hub/overview.md
```

### 场景 3: 在多愿景间切换

```
当前正在执行 V001 的任务 T005
    ↓
需要暂停 V001，切换到 V002 处理紧急任务
    ↓
Step 1: 保存 V001 上下文
  → 更新 state.json: active_vision_id = null
  → 记录 progress_V001.md 当前进度
  → 标记 task_T005 状态为 paused

Step 2: 加载 V002 上下文
  → 读取 visions/V002/vision.md
  → 读取 visions/V002/tasks.md
  → 更新 state.json: active_vision_id = V002

Step 3: 开始执行 V002 的任务
  → 按照 V002 的计划执行
  → 记录 progress_V002.md
```

---

## 📊 多愿景管理工作流

### 1. 愿景创建流程

```
提出新愿景
    ↓
填写 vision_template.md
    ↓
分析依赖关系（与其他愿景）
    ↓
评估资源需求
    ↓
确定优先级
    ↓
添加到投资组合（state.json）
    ↓
分配初始资源
    ↓
开始执行
```

### 2. 日常执行流程

```
AI 启动会话
    ↓
读取 state.json
  → 查看 active_vision_id
  → 如果没有，选择优先级最高的 active 愿景

Step 1: 加载愿景上下文
  → 读取 visions/{vision_id}/vision.md
  → 读取 visions/{vision_id}/tasks.md
  → 读取对应的 plan 文件

Step 2: 执行任务
  → 按照计划逐步执行
  → 记录进度到 progress/{vision_id}.md

Step 3: 更新状态
  → 更新 state.json 中的 per_vision_states
  → 如有必要，更新 overview.md
```

### 3. 资源冲突解决流程

```
发现资源冲突（如两个愿景需要同一个 AI Agent）
    ↓
查看 overview.md 了解全局资源
    ↓
评估优先级
  → P0 愿景优先
  → 截止日期近的优先
  → 依赖链上游优先

Step 1: 调整资源分配
  → 更新 hub/resources.md
  → 通知相关愿景负责人

Step 2: 更新计划
  → 调整受影响的 plan 文件
  → 记录变更原因

Step 3: 同步状态
  → 更新 state.json
```

---

## 🔧 配置文件说明

### config.json（4.0 新增字段）

```json
{
  "multi_vision_system": {
    "enabled": true,                    // 启用多愿景
    "max_visions": 10,                  // 最大愿景数
    "concurrent_execution": true,       // 支持并发执行
    "resource_allocation": "dynamic",   // 动态资源分配
    "vision_templates": [...]           // 支持的愿景模板类型
  },
  
  "portfolio_management": {
    "enabled": true,                    // 启用投资组合管理
    "resource_pooling": true,           // 资源池化
    "dependency_tracking": true,        // 依赖追踪
    "cross_vision_collaboration": true  // 跨愿景协作
  }
}
```

### state.json（4.0 结构）

```json
{
  "vision_portfolio": {
    "total_visions": 2,
    "active_visions": ["V001", "V002"],
    "completed_visions": [],
    "paused_visions": [],
    "visions": {
      "V001": {
        "name": "AI 助手开发",
        "status": "active",
        "priority": "P0",
        "progress": 35
      },
      "V002": {
        "name": "知识库管理",
        "status": "active",
        "priority": "P1",
        "progress": 20
      }
    }
  },
  
  "per_vision_states": {
    "V001": {
      "current_mode": "execution",
      "active_task": "T005",
      "last_updated": "2026-03-27T10:00:00Z"
    },
    "V002": {
      "current_mode": "planning",
      "active_task": null,
      "last_updated": "2026-03-27T09:00:00Z"
    }
  },
  
  "execution_context": {
    "current_vision_id": "V001",  // 当前正在执行的愿景
    "current_task": "T005"
  }
}
```

---

## 📈 投资组合视图

### overview.md 模板

```markdown
# 投资组合总览

## 全局指标

| 指标 | 数值 |
|------|------|
| 总愿景数 | 2 |
| 进行中 | 2 |
| 已完成 | 0 |
| 已暂停 | 0 |
| 总体进度 | 27.5% |

## 愿景列表

| 愿景 ID | 名称 | 优先级 | 状态 | 进度 | 负责人 |
|---------|------|--------|------|------|--------|
| V001 | AI 助手开发 | P0 | active | 35% | |
| V002 | 知识库管理 | P1 | active | 20% | |

## 资源分配

| 资源类型 | 总量 | 已分配 | 剩余 |
|----------|------|--------|------|
| AI Agents | 5 | 4 | 1 |
| 计算资源 | 100% | 80% | 20% |

## 依赖关系图

V002 --> V001 (技术依赖)

## 风险与问题

- 资源冲突：V001 和 V002 需要同一个 AI Agent
- 进度风险：V001 进度滞后 2 天
```

---

## 🎓 最佳实践

### ✅ 推荐做法

1. **愿景命名规范**
   - 使用 `V001_short_name` 格式
   - 保持名称简洁明了

2. **定期审查投资组合**
   - 每周审查各愿景进度
   - 及时调整资源分配
   - 清理已完成/已取消的愿景

3. **依赖管理**
   - 明确记录愿景间的依赖关系
   - 优先执行依赖链上游的愿景
   - 避免循环依赖

4. **上下文切换**
   - 切换前完整保存当前状态
   - 记录详细的交接笔记
   - 避免频繁切换（降低效率）

### ❌ 避免做法

1. **同时执行太多愿景**
   - 建议最多 3-5 个 active 愿景
   - 过多会导致上下文切换成本过高

2. **资源过度分散**
   - 不要平均分配资源
   - 优先保证 P0 愿景

3. **忽视依赖关系**
   - 未识别依赖就贸然开始
   - 导致后期阻塞

---

## 🔍 常见问题

### Q1: 最多可以同时运行多少个愿景？

**A**: 技术上无限制，但建议：
- P0 愿景：1-2 个
- P1 愿景：2-3 个
- 总计 active 愿景：不超过 5 个

### Q2: 如何处理愿景间的资源竞争？

**A**:
1. 查看优先级：P0 > P1 > P2
2. 查看紧急程度：截止日期近的优先
3. 查看依赖关系：上游优先
4. 协商调整：必要时重新分配

### Q3: 可以在不同愿景间共享任务吗？

**A**: 不建议。每个任务应明确属于一个愿景。如果有跨愿景的工作，应该：
- 创建一个新的跨愿景任务
- 或者将任务归属于主要受益的愿景

### Q4: 如何追踪跨愿景的进展？

**A**: 
- 使用 overview.md 作为总览
- 定期更新各愿景的进度百分比
- 记录关键里程碑完成情况

---

## 🔄 版本历史

- **v4.0** (2026-03-27): 新增多愿景并行管理能力
  - 愿景工作区隔离
  - 投资组合管理
  - 跨愿景协作
  - 动态资源分配
  
- **v3.0** (2026-03-27): 六类工程文档同步机制

---

## 📞 支持资源

### 模板文件

- `vision_template.md` - 愿景模板
- `visions/V001_*/*` - 愿景工作区示例

### 配置文件

- `config.json` - 多愿景配置
- `state.json` - 全局 + 各愿景状态

### 示例文件

- `hub/overview.md` - 投资组合总览
- `hub/resources.md` - 资源配置表

---

**文档版本**: v1.0  
**最后更新**: 2026-03-27  
**维护者**: AI 任务管理系统  
**系统版本**: ATS 4.0

