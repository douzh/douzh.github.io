# ATS 任务管理系统使用指南

## 📖 系统概述

**ATS (AI Task System)** - AI 自主驱动的任务管理系统 v3.0

本系统通过五层架构和六类工程文档的深度融合，实现 AI 独立执行复杂多步骤开发任务。

---

## 🎯 核心理念

### 五层架构（从抽象到具体）

```
Vision (愿景) → Why         为什么要做
    ↓
Problems (问题) → What's Wrong  什么问题
    ↓
Goals (目标) → What to Do     要做什么
    ↓
Tasks (任务) → How to Do      如何做
    ↓
Plans (计划) → Step by Step   逐步执行
```

### 六类工程文档同步机制

| 层级 | 对应文档 | 说明 |
|------|----------|------|
| Vision/Problems/Goals | BRD/PDD/AD | 业务、产品、架构视角 |
| Goals/Tasks | HLD/DD/DP | 概要设计、开发计划 |
| Tasks/Plans | DD/DP | 详细设计、执行计划 |

**记忆公式**: Why → What's Wrong → What to Do → How to Do → Step by Step + 文档同步

---

## 🚀 快速开始

### 1. 初始化项目

```bash
# 进入 ats-3.0 目录
cd ats-3.0

# 查看当前状态
cat state.json

# 了解项目愿景
cat vision.md
```

### 2. AI 标准工作流程

#### 第一次启动（新项目）

```
Step 1: 阅读配置文件
  → cat config.json
  → 了解项目基本信息和配置

Step 2: 阅读愿景文档
  → cat vision.md
  → 理解项目目标和核心价值

Step 3: 检查当前状态
  → cat state.json
  → 确认 current_mode 字段

Step 4: 根据模式执行相应操作
  → vision_definition: 开始定义愿景
  → problem_analysis: 拆解问题
  → goal_setting: 设定目标
  → task_breakdown: 分解任务
  → planning: 制定计划
  → execution: 执行任务
```

#### 中断后恢复（接手项目）

```
Step 1: 读取状态文件
  → cat state.json
  → 了解当前模式和进度

Step 2: 检查活动任务
  → 如果 active_task 不为 null
  → 读取对应的 task 文件
  → 读取对应的 plan 文件

Step 3: 查看进度记录
  → cat progress/progress_XXX.md
  → 了解执行历史

Step 4: 评估当前状态
  → 是否需要调整计划？
  → 是：更新 plan 文件
  → 否：继续执行

Step 5: 更新状态
  → 更新 state.json
  → 记录交接信息
```

---

## 📋 核心文件说明

### 配置文件

| 文件 | 用途 | 更新频率 |
|------|------|----------|
| `config.json` | 系统配置 | 初始设置后少改动 |
| `state.json` | 实时状态 | 每次会话必更新 |

### 五层架构文件

| 文件 | 层级 | 内容 | 更新时间 |
|------|------|------|----------|
| `vision.md` | L1 | 项目愿景 | 项目初期 |
| `problems.md` | L2 | 问题拆解 | 愿景确认后 |
| `goals.md` | L3 | 目标定义 | 问题确认后 |
| `tasks.md` | L4 | 任务列表 | 目标确认后 |
| `plans/*.md` | L5 | 执行计划 | 任务确认后 |

### 六类工程文档

| 文档类型 | 路径 | 对应层级 |
|----------|------|----------|
| BRD | `docs/brd/` | Vision/Problems/Goals |
| PDD | `docs/pdd/` | Vision/Problems/Goals |
| AD | `docs/ad/` | Vision/Problems/Goals |
| HLD | `docs/hld/` | Goals/Tasks |
| DD | `docs/dd/` | Tasks/Plans |
| DP | `docs/dp/` | Goals/Tasks/Plans |

### 进度与归档

| 文件夹 | 用途 | 命名规范 |
|--------|------|----------|
| `progress/` | 进度记录 | progress_T001.md |
| `plans/` | 执行计划 | plan_001.md |
| `completed/` | 已完成任务 | task_XXX_done.md |
| `archive/` | 历史版本 | archive_date_description.md |

---

## 🔄 标准操作流程

### 流程 1: 从 Vision 到 Goals 的拆分

```
1. 确认愿景 (vision.md)
   ↓ 同步创建：docs/brd/brd_vision.md
              docs/pdd/pdd_vision.md
              docs/ad/ad_vision.md

2. 拆解问题 (problems.md)
   ↓ 同步创建：docs/brd/brd_problems.md
              docs/pdd/pdd_problems.md
              docs/ad/ad_problems.md

3. 设定目标 (goals.md)
   ↓ 同步创建：docs/brd/brd_goals.md
              docs/pdd/pdd_goals.md
              docs/ad/ad_goals.md
              docs/hld/hld_goals.md
              docs/dp/dp_goals.md
```

### 流程 2: 从 Goals 到 Tasks 的分解

```
1. 选择目标 (goals.md 中的 G001)
   ↓

2. 分解任务 (tasks.md)
   ↓ 同步创建：docs/hld/hld_tasks.md
              docs/dd/dd_tasks.md
              docs/dp/dp_tasks.md

3. 为每个任务创建执行计划
   ↓ 在 plans/ 创建 plan_001.md
   ↓ 在 progress/ 创建 progress_001.md
```

### 流程 3: 任务执行流程

```
1. 读取任务文件 (tasks.md#任务 -1)
   ↓

2. 读取执行计划 (plans/plan_001.md)
   ↓

3. 按步骤执行并记录进度
   ↓ 更新 progress/progress_001.md
   ↓ 每完成一个步骤就更新 state.json

4. 完成任务后
   ↓ 更新 tasks.md 中该任务状态为 completed
   ↓ 更新 state.json
   ↓ 将任务相关文件移动到 completed/
```

### 流程 4: 中断恢复流程

```
1. 新 AI 读取所有相关文件
   → vision.md (了解愿景)
   → config.json (获取配置)
   → state.json (了解状态)
   → tasks.md (查看任务列表)

2. 定位当前工作
   → 检查 active_task 字段
   → 如果有值，找到对应任务文件
   → 如果没有，从 available_tasks 选择

3. 了解执行情况
   → 读取对应的 plan 文件
   → 读取对应的 progress 文件
   → 查看已完成步骤和待完成步骤

4. 决定下一步行动
   → 继续执行原计划？
   → 还是需要调整计划？
   → 如果需要调整，更新 plan 文件并记录原因

5. 开始执行
   → 按照 plan 的步骤执行
   → 实时更新 progress 文件
   → 完成后更新 state.json
```

---

## 📊 状态管理

### state.json 关键字段

```json
{
  "current_mode": "vision_definition",  // 当前阶段
  "active_task": "T001",                // 当前任务
  "available_tasks": ["T001", "T002"],  // 可用任务
  "completed_tasks": [],                // 已完成
  "document_sync": {                    // 文档同步状态
    "brd_version": "v1.0",
    "pdd_version": "v1.0",
    "last_sync": "2026-03-27"
  }
}
```

### 模式转换

```
vision_definition → problem_analysis → goal_setting
       ↓
task_breakdown → planning → execution → completion
```

---

## 🎓 最佳实践

### ✅ 推荐做法

1. **文件即上下文**
   - 所有信息都保存在文件中
   - AI 通过读写文件理解和更新状态
   - 不要依赖内存或口头传达

2. **及时更新**
   - 每次会话前后都要更新 state.json
   - 执行过程中实时记录 progress
   - 任务完成后立即归档

3. **文档同步**
   - 每层拆分时同步生成对应文档
   - 文档版本与执行进度保持一致
   - 定期审查文档完整性

4. **版本控制**
   - 重要变更使用版本号标记
   - 历史记录放入 archive/
   - 保持主分支简洁清晰

### ❌ 避免做法

1. **跳过阅读直接执行**
   - 必须先完整阅读相关文件
   - 理解上下文后再行动

2. **忘记更新状态**
   - 执行后不更新 state.json
   - 进度记录滞后于实际工作

3. **文档不同步**
   - 只更新代码不更新文档
   - 文档版本混乱

4. **多人同时修改同一文件**
   - 多 AI 协作时要明确分工
   - 使用文件锁或协调机制

---

## 🔍 常见问题

### Q1: 如何开始一个新项目？

**A**: 
1. 复制 ats-3.0 模板到新项目
2. 编辑 config.json 填写项目信息
3. 编辑 vision.md 定义项目愿景
4. 按照五层架构逐步推进

### Q2: 如何在多个 AI 之间交接？

**A**:
1. 离任 AI 更新 state.json 和 handover_notes
2. 记录当前进度和待办事项
3. 继任 AI 阅读所有相关文件
4. 确认理解后开始工作

### Q3: 如何处理计划变更？

**A**:
1. 在 plan 文件中记录变更原因
2. 更新版本号
3. 同步更新相关文档
4. 通知相关 AI

### Q4: 文档太多太乱怎么办？

**A**:
1. 严格按照模板创建文档
2. 使用统一的命名规范
3. 定期清理和归档
4. 维护文档索引

---

## 📞 支持资源

### 模板文件

- `config.json` - 配置模板
- `state.json` - 状态模板
- `plans/plan_template.md` - 计划模板
- `progress/progress_template.md` - 进度模板
- `docs/*/xxx_template.md` - 六类文档模板

### 示例文件

- `vision.md` - 愿景示例
- `problems.md` - 问题示例
- `goals.md` - 目标示例
- `tasks.md` - 任务示例

---

## 🔄 版本历史

- **v3.0** (2026-03-27): 新增六类工程文档同步机制
- **v2.0** (2026-03-09): 更新术语"使命"为"目标"
- **v1.0** (2026-03-05): 初始版本

---

**文档版本**: v1.0  
**最后更新**: 2026-03-27  
**维护者**: AI 任务管理系统
