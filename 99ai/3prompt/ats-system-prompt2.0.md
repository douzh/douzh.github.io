# AI 任务管理系统（ATS） - 完整创建提示词

> **用途**: 用于让 AI 重新创建完整的任务管理系统  
> **版本**: 2.0  
> **最后更新**: 2026-03-09  
> **系统简称**: ATS（AI Task System / 智管系统）

---

## 🤖 完整提示词

请将以下内容完整发送给另一个 AI：

```
# 任务：创建 AI 自主驱动的任务管理系统（ATS）

你好！我需要你帮我创建一个 AI 自主驱动的任务管理系统。

**全称**: AI Task System  
**简称**: ATS / 智管系统  

这个系统允许 AI 独立执行复杂的多步骤开发任务，支持中断后无缝恢复，多 AI 协作，以及动态调整计划。

## 🎯 核心要求

### 1. 系统设计理念
创建一个五层架构的任务管理系统，从抽象到具体：
- **Vision（愿景）**: 项目的终极目标
- **Problem（问题）**: 需要解决的核心问题
- **Goal（目标）**: 从问题分解出的大目标
- **Task（任务）**: 可独立执行的工作单元
- **Plan（计划）**: 任务的具体执行步骤

记忆公式：Why → What's Wrong → What to Do → How to Do → Step by Step

### 2. 必须实现的关键能力
✅ **中断恢复**: AI 可以在任何时间点中断，并能在之后快速恢复
✅ **上下文传承**: 新 AI 可以通过读取文件在 5 分钟内完全接手项目
✅ **动态调整**: 计划可以根据实际情况灵活调整
✅ **进度追踪**: 实时记录每个任务的执行进度
✅ **自动归档**: 任务完成后自动更新所有相关文件
✅ **多 AI 协作**: 不同 AI 可以交替或并行工作
```
## 📁 目录结构要求

请创建以下目录结构（以 `ats/` 为根目录）：

```
ats/
├── README.md                 # 系统说明文档
├── config.json              # 项目全局配置
├── state.json               # 当前系统状态
├── vision.md                # 项目愿景和长期目标
├── problems/                # 问题定义目录
│   └── problem_001.md      # 第一个问题定义
├── goals/                   # 目标目录
│   └── goal_001.json       # 第一个目标定义
├── tasks/                   # 任务定义目录
│   ├── task_001.json       # 第一个任务
│   ├── task_002.json       # 第二个任务
│   └── task_003.json       # 第三个任务
├── plans/                   # 执行计划目录
│   └── plan_001.json       # 第一个任务的计划
├── progress/                # 执行进度快照目录
│   └── task_001_progress.json  # 第一个任务的进度
├── completed/               # 已完成目标归档目录
└── archive/                 # 已完成任务归档目录
```

## 📝 文件格式详细规范

### 1. config.json - 项目配置
```json
{
  "project_name": "你的项目名称",
  "description": "项目描述",
  "tech_stack": ["技术 1", "技术 2"],
  "working_directory": "你的工作目录路径",
  "task_system_version": "2.0",
  "architecture": {
    "type": "Agent-based",
    "decision_model": "Decision-driven",
    "execution_flow": "Vision -> Problem -> Goal -> Task -> Plan"
  },
  "created_at": "2026-03-09T00:00:00Z",
  "last_updated": "2026-03-09T00:00:00Z"
}
```

### 2. state.json - 系统状态
```json
{
  "current_mode": "development",
  "active_goal": "goal_001",
  "active_task": null,
  "status": "idle",
  "last_checkpoint": "2026-03-09T00:00:00Z",
  "session_start": null,
  "total_tasks_completed": 0,
  "total_goals_completed": 0,
  "system_status": {
    "task_system_ready": true,
    "vision_defined": true,
    "first_task_completed": false,
    "layered_structure_complete": true,
    "processes_documented": true
  },
  "next_recommended_action": "开始执行 task_001",
  "available_tasks": ["task_001", "task_002", "task_003"]
}
```

### 3. vision.md - 项目愿景
```markdown
# 项目愿景

## 核心目标
用一句话描述项目的终极目标。

## 长期方向
- 方向 1
- 方向 2

## 价值主张
项目为用户提供的核心价值。

## 第一阶段目标（MVP）
1. 目标 1
2. 目标 2

## 成功指标
- ✅ 指标 1
- ✅ 指标 2
```

### 4. problems/problem_XXX.md - 问题定义
```markdown
# 问题编号：PROB-001

## 问题描述
清晰描述需要解决的问题（1-2 句话）。

## 背景
为什么这个问题重要？当前遇到了什么困难？

## 影响范围
- 影响的系统或模块 1
- 影响的系统或模块 2

## 期望结果
问题解决后应该达到什么状态？

## 关联目标
- GOAL-001: 目标名称（进行中）

## 优先级
**HIGH/MEDIUM/LOW** - 说明原因

## 验收标准
- [ ] 标准 1
- [ ] 标准 2
```

### 5. goals/goal_XXX.json - 目标定义
```json
{
  "id": "goal_001",
  "title": "目标的简洁标题",
  "related_problem": "problem_001",
  "description": "详细描述这个目标要实现什么",
  "success_criteria": [
    "成功标准 1",
    "成功标准 2",
    "成功标准 3"
  ],
  "tasks": [
    "task_001",
    "task_002",
    "task_003"
  ],
  "priority": "critical|high|medium|low",
  "status": "pending|in_progress|completed",
  "progress_percent": 0,
  "created_at": "2026-03-09T00:00:00Z",
  "updated_at": "2026-03-09T00:00:00Z",
  "estimated_completion": "2026-03-09T12:00:00Z",
  "notes": "额外的说明信息"
}
```

### 6. tasks/task_XXX.json - 任务定义
```json
{
  "id": "task_001",
  "title": "任务的简洁标题",
  "related_goal": "goal_001",
  "description": "详细描述任务要做什么",
  "goal": "任务的具体目标",
  "acceptance_criteria": [
    "验收标准 1",
    "验收标准 2",
    "验收标准 3"
  ],
  "estimated_effort": "1h|2h|30m",
  "priority": "high|medium|low",
  "status": "pending|in_progress|completed",
  "tags": ["标签 1", "标签 2"],
  "created_at": "2026-03-09T00:00:00Z",
  "completed_at": null,
  "final_notes": null
}
```

### 7. plans/plan_XXX.json - 执行计划
```json
{
  "task_id": "task_001",
  "version": 1,
  "last_updated": "2026-03-09T00:00:00Z",
  "steps": [
    {
      "step": 1,
      "action": "步骤名称",
      "description": "详细描述这一步要做什么",
      "status": "pending|in_progress|completed",
      "completed_at": null,
      "started_at": null
    },
    {
      "step": 2,
      "action": "步骤名称",
      "description": "详细描述",
      "status": "pending"
    }
  ],
  "notes": "计划的额外说明"
}
```

### 8. progress/task_XXX_progress.json - 进度快照
```json
{
  "task_id": "task_001",
  "checkpoint": 1,
  "timestamp": "2026-03-09T00:00:00Z",
  "current_step": 1,
  "completed_steps": [],
  "progress_percent": 0,
  "notes": "当前进度说明",
  "files_modified": [],
  "blockers": [],
  "next_action": "下一步行动"
}
```

## 🚀 初始化任务示例

请为"建立 AI 任务管理系统"这个目标创建至少 3 个初始任务：

### task_001: 创建系统说明文档
- 创建 README.md
- 详细说明系统用途和目录结构
- 提供文件格式示例

### task_002: 设计任务接收流程
- 定义 AI 接收新任务的标准流程
- 创建检查清单
- 明确必须读取的配置文件

### task_003: 设计中断恢复机制
- 定义如何定位断点
- 建立进度文件的读取流程
- 提供计划调整的决策树

## ⚙️ 创建步骤

请按以下步骤执行：

1. **创建目录结构**
   - 创建所有必需的目录
   - 确保目录层级正确

2. **创建配置文件**
   - config.json: 填写你的项目信息
   - state.json: 初始化系统状态
   - vision.md: 定义项目愿景

3. **创建问题定义**
   - problem_001.md: 描述为什么要建立这个系统

4. **创建目标定义**
   - goal_001.json: 定义第一个大目标
   - 包含至少 3 个子任务

5. **创建任务定义**
   - task_001.json ~ task_003.json
   - 每个任务都要有清晰的验收标准

6. **创建执行计划**
   - plan_001.json: 为 task_001 制定详细步骤
   - 至少包含 5 个步骤

7. **初始化进度文件**
   - task_001_progress.json: 记录初始状态

8. **创建用户指南**（可选）
   - USER_GUIDE.md: 帮助用户理解如何使用系统

## ⚠️ 关键注意事项

### 必须遵守的规则
✅ 所有 JSON 文件必须使用 UTF-8 编码
✅ 时间戳必须使用 ISO 8601 格式
✅ ID 命名必须统一（goal_XXX, task_XXX, plan_XXX）
✅ 文件路径使用正斜杠 `/` 或双反斜杠 `\\`
✅ 保持字段命名一致性（使用下划线分隔）

### 术语使用
✅ 使用 "Goal（目标）" 而不是 "Mission（使命）"
✅ 使用 "related_goal" 而不是 "related_mission"
✅ 使用 "active_goal" 而不是 "active_mission"
✅ 使用 "total_goals_completed" 而不是 "total_missions_completed"

### 状态管理
✅ 任务状态只能是：pending | in_progress | completed | blocked | cancelled
✅ 目标状态只能是：pending | in_progress | completed | on_hold
✅ 计划步骤状态只能是：pending | in_progress | completed | skipped | blocked

## 📋 验证清单

创建完成后，请验证：
- [ ] 所有目录已创建
- [ ] 所有 JSON 文件格式正确
- [ ] 所有 Markdown 文件格式规范
- [ ] ID 引用一致（如 related_goal 指向存在的 goal）
- [ ] state.json 中的 available_tasks 与实际任务文件匹配
- [ ] 至少有一个完整的任务流程（goal → task → plan → progress）

## 💡 预期效果

创建完成后，一个新 AI 应该能够：
1. 读取 vision.md 了解项目愿景
2. 读取 config.json 获取项目配置
3. 读取 state.json 知道当前状态
4. 如果有 active_task，读取对应文件继续执行
5. 如果没有活动任务，从 available_tasks 中选择下一个任务
6. 通过 reading progress 文件了解执行进度
7. 按照 plan 文件逐步执行

## 🎯 核心工作流程

### 完整任务流程
```
识别问题 → 创建 problem_XXX.md
    ↓
定义目标 → 创建 goal_XXX
    ↓
拆分任务 → 创建 task_XXX
    ↓
制定计划 → 创建 plan_XXX
    ↓
设置状态 → 更新 state.json
    ↓
执行任务 → 按 steps 执行
    ↓
更新进度 → 更新 progress
    ↓
任务完成？→ 是：归档任务
    ↓
目标完成？→ 是：归档目标
```

### 中断恢复流程
```
读取 state.json
    ↓
有 active_task?
    ↓
是：读取进度文件
    ↓
评估当前状态
    ↓
计划需要调整？
    ↓
是：更新 plan_XXX
    ↓
继续执行
```

## 🎓 补充说明

### 系统核心理念
- **文件即上下文**: 所有信息都保存在文件中，AI 通过读写文件来理解和更新状态
- **分层管理**: 从抽象到具体，每一层都有明确的职责
- **中断恢复**: 通过进度快照，AI 可以在任何时候中断和恢复
- **动态调整**: 计划可以随时更新版本，适应变化

### 关键特性
1. **分层管理**
   - Vision → Problems → Goals → Tasks → Plans
   - 从抽象到具体，便于理解和执行

2. **动态调整**
   - 计划可以随时更新版本
   - 支持插入新步骤
   - 支持重新排序

3. **完整上下文**
   - 每个层级都有清晰的关联
   - 进度文件保存完整执行历史
   - 新 AI 可以快速接手

4. **自动化友好**
   - JSON 格式便于解析
   - 状态机明确
   - 支持自动归档

---

**现在请开始创建这个任务管理系统。如果在创建过程中有任何疑问，请先询问我确认后再继续。**
```

---

## 📌 使用说明

### 如何使用此提示词

1. **完整复制**: 将上面的完整提示词（从"任务：创建 AI 自主驱动的任务管理系统"到最后）复制

2. **发送给 AI**: 将提示词发送给另一个 AI 助手

3. **监督执行**: AI 会按照提示词逐步创建系统，你可以随时检查进度

4. **验证结果**: 使用验证清单检查 AI 创建的系统是否完整正确

### 适用场景

- ✅ 重新初始化整个任务管理系统
- ✅ 在新项目中建立相同的任务系统
- ✅ 培训新的 AI 助手理解任务系统
- ✅ 系统损坏后的重建
- ✅ 多 AI 协作项目的标准化

### 自定义选项

如需调整，可以修改：
- `config.json` 模板中的项目信息
- 初始任务的内容和数量
- 技术栈配置
- 特定领域的要求

### 版本历史

- **v2.0** (2026-03-09): 更新术语"使命"为"目标"
- **v1.0** (2026-03-05): 初始版本

---

**文档位置**: `doc/system-recreation-prompt.md`  
**创建日期**: 2026-03-09  
**维护者**: AI 任务管理系统
