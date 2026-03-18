# AI任务管理系统（ATS 3.0）- 完整重建提示词

> **用途**: 用于让 AI 重新创建完整的 ATS 3.0 任务管理系统（含六类工程文档）  
> **版本**: 3.0  
> **最后更新**: 2026-03-18  
> **系统简称**: ATS（AI Task System / 智管系统）

---

## 🤖 完整提示词

请将以下内容完整发送给另一个 AI：

```
# 任务：创建 ATS 3.0 - AI 自主驱动的任务管理系统（含六类工程文档）

你好！我需要你帮我创建一个 ATS 3.0 版本的 AI 自主驱动的任务管理系统。

**全称**: AI Task System  
**简称**: ATS / 智管系统  
**版本**: 3.0（新增六类工程文档同步机制）

这个系统允许 AI 独立执行复杂的多步骤开发任务，支持中断后无缝恢复、多 AI协作、动态调整计划，并深度融合软件工程文档体系。

## 🎯 核心要求

### 1. 系统设计理念（3.0 升级版）

创建一个**五层架构 + 六类工程文档同步**的任务管理系统：

**五层架构**（从抽象到具体）:
- **Vision（愿景）**: 项目的终极目标 → Why
- **Problems（问题）**: 需要解决的核心问题 → What's Wrong
- **Goals（目标）**: 从问题分解出的大目标 → What to Do
- **Tasks（任务）**: 可独立执行的工作单元 → How to Do
- **Plans（计划）**: 任务的具体执行步骤 → Step by Step

**六类工程文档**（与五层映射同步）:
- **BRD** (业务需求文档) - Vision/Problems/Goals层
- **PDD** (产品设计文档) - Vision/Problems/Goals层
- **AD** (架构文档) - Vision/Problems/Goals层
- **HLD** (概要设计文档) - Goals/Tasks层
- **DD** (详细设计文档) - Tasks/Plans层
- **DP** (开发计划文档) - Goals/Tasks/Plans层

记忆公式：Why → What's Wrong → What to Do → How to Do → Step by Step + 文档同步

### 2. 必须实现的关键能力（3.0 增强）

✅ **文档同步**: 每层拆分动作同步生成对应文档章节  
✅ **中断恢复**: AI 可以在任何时间点中断，并能在之后快速恢复  
✅ **上下文传承**: 新 AI 可以通过读取文件在 5 分钟内完全接手项目  
✅ **动态调整**: 计划可以根据实际情况灵活调整，文档同步更新  
✅ **进度追踪**: 实时记录每个任务的执行进度，文档状态一致  
✅ **自动归档**: 任务完成后自动更新所有相关文件  
✅ **多 AI协作**: 不同 AI 可以交替或并行工作，以文档为唯一载体  
✅ **全链路追溯**: 任意层级修改可快速追溯关联关系
```

## 📁 目录结构要求（3.0 完整版）

请创建以下目录结构（以 `ats/` 为根目录）：

```
ats/
├── ats_task_management.md    # ATS 任务总控文档（3.0 新增）
├── README.md                 # 系统说明文档
├── config.json              # 项目全局配置
├── state.json               # 当前系统状态
├── vision.md                # 项目愿景和长期目标
├── docs/                    # 六类工程文档目录（3.0 新增）
│   ├── reqs/               # 需求文档（BRD）
│   │   ├── brd_vision.md
│   │   ├── brd_problems.md
│   │   ├── brd_goals.md
│   │   └── history/
│   ├── prod/               # 产品文档（PDD）
│   │   ├── pdd_vision.md
│   │   ├── pdd_problems.md
│   │   ├── pdd_goals.md
│   │   └── history/
│   ├── arch/               # 架构文档（AD）
│   │   ├── ad_vision.md
│   │   ├── ad_problems.md
│   │   ├── ad_goals.md
│   │   └── history/
│   ├── design/             # 设计文档（HLD）
│   │   ├── hld_goals.md
│   │   ├── hld_tasks.md
│   │   └── history/
│   ├── detail/             # 详细设计文档（DD）
│   │   ├── dd_tasks.md
│   │   ├── dd_plans.md
│   │   └── history/
│   └── plan/               # 开发计划文档（DP）
│       ├── dp_goals.md
│       ├── dp_tasks.md
│       ├── dp_plans.md
│       └── history/
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
├── archive/                 # 已完成任务归档目录
└── history/                 # ATS 文档历史版本（3.0 新增）
```

## 📝 文件格式详细规范（3.0 版）

### 1. config.json - 项目配置（3.0 升级版）

```json
{
  "project_name": "你的项目名称",
  "description": "项目描述",
  "tech_stack": ["技术 1", "技术 2"],
  "working_directory": "你的工作目录路径",
  "task_system_version": "3.0",
  "architecture": {
    "type": "Agent-based",
    "decision_model": "Decision-driven",
    "execution_flow": "Vision -> Problem -> Goal -> Task -> Plan",
    "document_sync": "BRD/PDD/AD/HLD/DD/DP"
  },
  "document_system": {
    "enabled": true,
    "types": ["BRD", "PDD", "AD", "HLD", "DD", "DP"],
    "location": "ats/docs/"
  },
  "created_at": "2026-03-18T00:00:00Z",
  "last_updated": "2026-03-18T00:00:00Z"
}
```

### 2. state.json - 系统状态（3.0 升级版）

```json
{
  "current_mode": "vision_definition",
  "active_goal": null,
  "active_task": null,
  "status": "waiting_for_vision",
  "last_checkpoint": "2026-03-18T00:00:00Z",
  "session_start": null,
  "total_tasks_completed": 0,
  "total_goals_completed": 0,
  "system_status": {
    "task_system_ready": true,
    "vision_defined": false,
    "first_task_completed": false,
    "layered_structure_complete": true,
    "processes_documented": true,
    "document_system_v3_enabled": true,
    "all_six_doc_types_created": true
  },
  "next_recommended_action": "请用户定义项目愿景",
  "available_tasks": []
}
```

### 3. vision.md - 项目愿景（引导式）

```markdown
# 项目愿景

> **状态**: ⏳ 待定义  
> **最后更新**: 2026-03-18 (重置)

---

## 📝 愿景描述

**请在此处描述您的项目愿景：**

```
请描述你的 AI 项目愿景，需包含：
① 核心价值（解决什么问题、创造什么价值）
② 目标场景/用户群体
③ 长期状态
④ 最终交付物边界
```

---

## 🎯 核心目标

（待填写）

## 🌟 长期方向

（待填写）

## 💎 价值主张

（待填写）

## 📦 第一阶段目标（MVP）

（待填写）

## ✅ 成功指标

（待填写）
```

### 4. ats_task_management.md - ATS 任务总控文档（3.0 新增）

创建详细的总控文档，包含：
- 五层架构与六类文档映射关系
- 完整目录结构说明
- 核心工作流程（含文档同步）
- 当前项目状态（各层进度）
- 多AI协作规范
- 动态调整记录
- 快速索引

### 5. 六类工程文档模板（3.0 核心）

#### BRD 模板（愿景章节示例）
```markdown
# 业务需求文档 (BRD) - 愿景需求章节

**项目名称**: [项目名]  
**文档版本**: V3.0.0  
**创建日期**: 2026-03-18  
**状态**: ⏳ 待定义

---

## 1. 愿景需求章节

> **说明**: 本章节内容将在愿景确认后自动填充

### 1.1 愿景描述

（待填写 - 等待用户输入愿景）

### 1.2 核心价值

（待填写）

...

---

## 关联关系

- **上级**: 无（顶层文档）
- **下级**: [问题需求章节](./brd_problems.md#2-问题需求章节)
- **关联 ATS 层级**: [Vision(愿景层)](../../vision.md)

---

## 修订历史

|版本|日期|修订内容|修订人|
|---|---|---|---|
|V3.0.0|2026-03-18|重置为空，等待新愿景|AI Assistant|
```

类似地创建：
- PDD (愿景产品章节)
- AD (愿景架构章节)
- BRD/PDD/AD (问题章节)
- BRD/PDD/AD/HLD/DP (目标章节)
- HLD/DD/DP (任务章节)
- DD/DP (计划章节)

## 🚀 初始化任务示例（3.0 版）

请为"建立 ATS 3.0 任务管理系统"这个目标创建至少 3 个初始任务：

### task_001: 创建系统说明文档和总控文档
- 创建 README.md（包含 3.0 特性）
- 创建 ats_task_management.md（总控文档）
- 详细说明五层架构与六类文档映射关系

### task_002: 设计愿景重建流程
- 定义愿景引导话术
- 创建愿景确认机制
- 建立文档同步填充规则

### task_003: 设计文档同步机制
- 定义每层拆分时的文档生成规则
- 建立文档关联关系维护机制
- 提供版本管理和历史归档流程

## ⚙️ 创建步骤（3.0 完整版）

请按以下步骤执行：

1. **创建目录结构**
   - 创建所有必需的目录（含 docs/下 6 个子目录）
   - 确保目录层级正确
   - 为每个文档目录创建 history/子目录

2. **创建配置文件**
   - config.json: 填写你的项目信息（版本 3.0）
   - state.json: 初始化系统状态（vision_definition 模式）
   - vision.md: 创建引导式愿景模板

3. **创建总控文档**
   - ats_task_management.md: 详细说明五层架构与六类文档映射
   - 包含完整的工作流程和协作规范

4. **创建六类工程文档框架**
   - BRD: brd_vision.md（愿景需求章节）
   - PDD: pdd_vision.md（愿景产品章节）
   - AD: ad_vision.md（愿景架构章节）
   - 其他文档创建框架，标记为"待填充"

5. **创建问题定义**
   - problem_001.md: 描述为什么要建立这个系统

6. **创建目标定义**
   - goal_001.json: 定义第一个大目标
   - 包含至少 3 个子任务

7. **创建任务定义**
   - task_001.json ~ task_003.json
   - 每个任务都要有清晰的验收标准

8. **创建执行计划**
   - plan_001.json: 为 task_001 制定详细步骤
   - 至少包含 7 个步骤

9. **初始化进度文件**
   - task_001_progress.json: 记录初始状态

10. **创建操作手册**
    - docs/vision_operation_manual.md: 愿景交付物操作手册

## ⚠️ 关键注意事项（3.0 版）

### 必须遵守的规则
✅ 所有 JSON 文件必须使用 UTF-8 编码  
✅ 时间戳必须使用 ISO 8601 格式  
✅ ID 命名必须统一（goal_XXX, task_XXX, plan_XXX）  
✅ 文件路径使用正斜杠 `/` 或双反斜杠 `\\`  
✅ 保持字段命名一致性（使用下划线分隔）  
✅ **六类文档必须与五层架构逐层对应**  
✅ **文档底部必须标注关联关系**  

### 术语使用
✅ 使用 "Goal（目标）" 而不是 "Mission（使命）"  
✅ 使用 "related_goal" 而不是 "related_mission"  
✅ 使用 "active_goal" 而不是 "active_mission"  
✅ 使用 "total_goals_completed" 而不是 "total_missions_completed"  
✅ 使用 "document_sync" 表示六类文档同步机制  

### 状态管理
✅ 任务状态只能是：pending | in_progress | completed | blocked | cancelled  
✅ 目标状态只能是：pending | in_progress | completed | on_hold  
✅ 计划步骤状态只能是：pending | in_progress | completed | skipped | blocked  
✅ 文档状态只能是：待定义 | 进行中 | 已完成 | 已跳过 | 已阻塞  

### 文档同步规则
✅ 愿景确认后同步填充 BRD/PDD/AD愿景章节  
✅ 问题拆解后同步填充 BRD/PDD/AD问题章节  
✅ 目标拆分后同步填充 BRD/PDD/AD/HLD/DP目标章节  
✅ 任务创建后同步填充 HLD/DD/DP任务章节  
✅ 计划制定后同步填充 DD/DP计划章节  

## 📋 验证清单（3.0 版）

创建完成后，请验证：
- [ ] 所有目录已创建（13 个必需目录 + 6 个 docs 子目录）
- [ ] 所有 JSON 文件格式正确
- [ ] 所有 Markdown 文件格式规范
- [ ] ID 引用一致（如 related_goal 指向存在的 goal）
- [ ] state.json 中的 available_tasks 与实际任务文件匹配
- [ ] 至少有一个完整的任务流程（goal → task → plan → progress）
- [ ] **六类工程文档框架已创建**
- [ ] **文档间关联关系正确标注**
- [ ] **ats_task_management.md 包含完整映射关系**
- [ ] config.json 版本为 3.0，包含 document_system 配置

## 💡 预期效果（3.0 增强版）

创建完成后，一个新 AI 应该能够：

1. 读取 vision.md 了解项目愿景（引导式）
2. 读取 config.json 获取项目配置（含文档系统）
3. 读取 state.json 知道当前状态（vision_definition 模式）
4. 如果有 active_task，读取对应文件继续执行
5. 如果没有活动任务，从 available_tasks 中选择下一个任务
6. 通过 reading progress 文件了解执行进度
7. 按照 plan 文件逐步执行
8. **读取 ats_task_management.md 了解五层架构与六类文档映射**
9. **根据当前层级读取对应的 BRD/PDD/AD/HLD/DD/DP 文档**
10. **执行过程中同步更新文档状态**

## 🎯 核心工作流程（3.0 文档同步版）

### 完整任务流程（含文档同步）
```
1. Vision(愿景) 确认
   ↓ 同步创建：brd_vision.md, pdd_vision.md, ad_vision.md
2. Problems(问题) 拆解
   ↓ 同步创建：brd_problems.md, pdd_problems.md, ad_problems.md
3. Goals(目标) 拆分
   ↓ 同步创建：brd_goals.md, pdd_goals.md, ad_goals.md, hld_goals.md, dp_goals.md
4. Tasks(任务) 拆解
   ↓ 同步创建：hld_tasks.md, dd_tasks.md, dp_tasks.md
5. Plans(计划) 制定
   ↓ 同步创建：dd_plans.md, dp_plans.md
6. 执行计划
   ↓ 同步更新：dp_plans.md 状态 + progress/
7. 任务完成
   ↓ 归档：completed/, archive/
```

### 中断恢复流程（3.0 增强）
```
读取 ats_task_management.md
    ↓
定位当前层级（Vision/Problems/Goals/Tasks/Plans）
    ↓
加载对应文档：BRD/PDD/AD/HLD/DD/DP
    ↓
读取 state.json 和 progress/
    ↓
评估当前状态
    ↓
计划需要调整？
    ↓
是：更新 plan_XXX + 同步更新文档
    ↓
继续执行
```

## 🎓 补充说明（3.0 新增）

### 系统核心理念
- **文件即上下文**: 所有信息都保存在文件中，AI 通过读写文件来理解和更新状态
- **分层管理**: 从抽象到具体，每一层都有明确的职责
- **文档同步**: 每层拆分动作同步生成对应文档章节
- **中断恢复**: 通过进度快照和文档记录，AI 可以在任何时候中断和恢复
- **动态调整**: 计划可以随时更新版本，文档同步更新
- **多 AI协作**: 以 Markdown 文档为唯一协作载体

### 关键特性（3.0 增强）
1. **分层管理 + 文档同步**
   - Vision → Problems → Goals → Tasks → Plans
   - 每层同步生成 BRD/PDD/AD/HLD/DD/DP对应章节
   - 从抽象到具体，便于理解和执行

2. **动态调整 + 文档版本**
   - 计划可以随时更新版本
   - 支持插入新步骤
   - 支持重新排序
   - 文档同步更新版本

3. **完整上下文 + 全链路追溯**
   - 每个层级都有清晰的关联
   - 文档间树状关联，可汇总、可追溯
   - 进度文件保存完整执行历史
   - 新 AI 可以快速接手

4. **自动化友好**
   - JSON 格式便于解析
   - 状态机明确
   - 支持自动归档
   - 文档状态与执行进度一致

---

**现在请开始创建这个 ATS 3.0 任务管理系统。如果在创建过程中有任何疑问，请先询问我确认后再继续。**
```

---

## 📌 使用说明

### 如何使用此提示词

1. **完整复制**: 将上面的完整提示词（从"任务：创建 ATS 3.0..."到最后）复制

2. **发送给 AI**: 将提示词发送给另一个 AI 助手

3. **监督执行**: AI 会按照提示词逐步创建 ATS 3.0 系统，你可以随时检查进度

4. **验证结果**: 使用验证清单检查 AI 创建的系统是否完整正确

### 适用场景（3.0）

- ✅ 重新初始化整个 ATS 3.0 任务管理系统
- ✅ 在新项目中建立 ATS 3.0 标准系统
- ✅ 培训新的 AI 助手理解 ATS 3.0 系统
- ✅ 系统损坏后的重建
- ✅ 多AI协作项目的标准化
- ✅ 从 ATS 2.0 升级到 3.0

### 与 2.0 的区别

|特性|2.0|3.0|
|---|---|---|
|文档体系|无文档关联|六类工程文档同步|
|关联逻辑|五层独立运行|五层与六类文档树状关联|
|执行闭环|仅任务执行|执行过程同步更新文档|
|协作适配|单 AI/人工|多 AI 以文档为载体|
|容错恢复|仅任务进度|全文档版本回溯|
|交付物|仅执行结果|文档 + 结果双交付|

### 自定义选项

如需调整，可以修改：
- `config.json` 模板中的项目信息
- 初始任务的内容和数量
- 技术栈配置
- 文档类型（可增加其他文档类型）
- 特定领域的要求

### 版本历史

- **v3.0** (2026-03-18): 新增六类工程文档同步机制，深度融合五层架构
- **v2.0** (2026-03-09): 更新术语"使命"为"目标"
- **v1.0** (2026-03-05): 初始版本

---

**文档位置**: `ats-system-prompt3.md`  
**创建日期**: 2026-03-18  
**维护者**: AI任务管理系统  
**系统版本**: ATS 3.0
