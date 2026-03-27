# ATS 4.0 - 完整重建提示词

> **用途**: 用于让 AI 重新创建或验证完整的 ATS 4.0 多愿景任务管理系统
> **版本**: 4.0
> **最后更新**: 2026-03-27
> **系统简称**: ATS（AI Task System / 智管系统）

---

## 🤖 完整提示词

请将以下内容完整发送给另一个 AI：

```
# 任务：验证/创建 ATS 4.0 - AI 自主驱动的多愿景任务管理系统

你好！我需要你帮我验证或创建一个 ATS 4.0 版本的 AI 自主驱动的多愿景任务管理系统。

**全称**: AI Task System (Multi-Vision Edition)
**简称**: ATS 4.0 / 智管系统 4.0
**版本**: 4.0（新增多愿景并行管理能力）

这个系统是在 ATS 3.0（五层架构 + 六类工程文档）基础上的重大升级，支持：
- ✅ 同时管理多个独立或关联的项目（愿景）
- ✅ 中断后无缝恢复工作
- ✅ 多 AI 高效协作
- ✅ 动态资源分配和优化
- ✅ 完整的工程文档体系

## 🎯 核心要求

### 1. 系统设计理念（4.0 升级版）

创建一个**多愿景并行 + 五层架构 + 六类工程文档同步**的任务管理系统：

#### 多愿景管理（4.0 新增）
- 支持同时管理最多 10 个项目（愿景）
- 每个项目有独立的工作区和状态追踪
- 项目间可以有依赖和协作关系
- 统一的全局总览视图，全局资源优化

#### 五层架构（从抽象到具体）
记忆公式：**Why → What's Wrong → What to Do → How to Do → Step by Step**

- **Vision（愿景）**: 项目的终极目标 → Why（为什么要做）
- **Problems（问题）**: 需要解决的核心问题 → What's Wrong（有什么问题）
- **Goals（目标）**: 从问题分解出的大目标 → What to Do（要做什么）
- **Tasks（任务）**: 可独立执行的工作单元 → How to Do（怎么做）
- **Plans（计划）**: 任务的具体执行步骤 → Step by Step（一步步做）

#### 六类工程文档（与五层映射同步）
每层拆分动作同步生成对应文档章节：

| 文档类型 | 英文全称 | 映射层级 | 记录内容 |
|----------|----------|----------|----------|
| BRD | Business Requirement Document | Vision/Problems/Goals | 业务需求和价值 |
| PDD | Product Design Document | Vision/Problems/Goals | 产品设计方案 |
| AD | Architecture Document | Vision/Problems/Goals | 系统架构设计 |
| HLD | High-Level Design | Goals/Tasks | 概要设计和模块划分 |
| DD | Detailed Design | Tasks/Plans | 详细设计和代码实现 |
| DP | Development Plan | Goals/Tasks/Plans | 开发计划和资源安排 |

### 2. 必须实现的关键能力（4.0 增强版）

✅ **多愿景并行**: 支持同时管理多个项目，每个项目独立运行
✅ **全局总览管理**: 统一视角查看所有项目的状态、进度和资源分配
✅ **跨愿景协作**: 项目间的依赖管理和资源共享
✅ **动态资源分配**: 根据优先级和进度智能调配 AI 和时间资源
✅ **上下文切换**: AI 在不同项目间快速切换，保存和加载上下文
✅ **文档同步**: 每层拆分动作同步生成对应文档章节
✅ **中断恢复**: AI 可以在任何时间点中断，并能在之后快速恢复
✅ **全链路追溯**: 任意层级修改可快速追溯关联关系

## 📁 目录结构要求（4.0 完整版）

请创建以下目录结构（以 `ats-4.0/` 为根目录）：

```
ats-4.0/
├── README.md                      # 系统说明（通俗易懂版）
├── QUICK-NAV.md                   # 快速导航
├── config.json                    # 系统配置（含多愿景配置）
├── state.json                     # 全局状态 + 各愿景状态
│
├── docs/                          # 文档库
│   ├── guide/                     # 使用指南
│   │   ├── readme.md             # 系统概述
│   │   ├── ats-portfolio-management.md  # 多愿景管理详细指南
│   │   └── upgrade-verification.md      # 升级验证清单
│   └── templates/                 # 模板文件
│       ├── vision_template.md    # 愿景模板
│       ├── brd_template.md       # BRD 模板
│       ├── pdd_template.md       # PDD 模板
│       ├── ad_template.md        # AD 模板
│       ├── hld_template.md       # HLD 模板
│       ├── dd_template.md        # DD 模板
│       ├── dp_template.md        # DP 模板
│       └── readme.md             # 模板使用指南
│
├── visions/                       # 各愿景独立工作区（核心！）
│   ├── V001_example/             # 示例愿景（已创建）
│   │   ├── vision.md            # 愿景文档（项目终极目标）
│   │   ├── problems.md          # 问题拆解（要解决的痛点）
│   │   ├── goals.md             # 目标定义（具体目标）
│   │   ├── tasks.md             # 任务列表（可执行单元）
│   │   └── docs/                # 六类文档（按愿景分区）
│   │       ├── brd/brd_V001.md  # 业务需求文档
│   │       ├── pdd/pdd_V001.md  # 产品设计文档
│   │       ├── ad/ad_V001.md    # 架构文档
│   │       ├── hld/hld_V001.md  # 概要设计文档
│   │       ├── dd/dd_V001.md    # 详细设计文档
│   │       └── dp/dp_V001.md    # 开发计划文档
│   └── V002_your_vision/         # 新愿景（待创建）
│
├── hub/                           # 全局管理中心（全局总览视图）
│   ├── overview.md               # 全局总览（所有项目的仪表板）
│   └── resources.md              # 资源配置表（AI、时间等）
│
├── plans/                         # 执行计划（按愿景 + 任务）
│   ├── plan_template.md          # 计划模板
│   ├── plan_V001_001.md          # V001 的第一个计划
│   └── plan_V002_001.md          # V002 的第一个计划
│
├── progress/                      # 进度记录（按愿景）
│   ├── progress_template.md      # 进度模板
│   ├── progress_V001.md          # V001 的进度记录
│   └── progress_V002.md          # V002 的进度记录
│
├── completed/                     # 已完成归档（按愿景分区）
│   └── archive/                   # 历史版本归档（按愿景分区）
│       └── 
```

## 🔧 配置文件格式

### config.json

```json
{
  "project_name": "ATS 4.0 多愿景任务管理系统",
  "version": "4.0",
  "description": "AI 自主驱动的任务管理系统（支持多愿景并行管理）",
  
  "multi_vision_system": {
    "enabled": true,
    "max_visions": 10,
    "concurrent_execution": true,
    "resource_allocation": "dynamic"
  },
  
  "document_system": {
    "enabled": true,
    "types": ["BRD", "PDD", "AD", "HLD", "DD", "DP"],
    "per_vision_docs": true,
    "sync_enabled": true
  },
  
  "portfolio_management": {
    "enabled": true,
    "resource_pooling": true,
    "dependency_tracking": true,
    "cross_vision_collaboration": true
  }
}
```

**关键字段说明**：
- `multi_vision_system.enabled`: 必须为 true，启用多愿景管理
- `multi_vision_system.max_visions`: 最多支持的愿景数量（建议 10 个）
- `document_system.types`: 六类工程文档类型
- `portfolio_management`: 全局总览管理相关配置

### state.json

```json
{
  "vision_portfolio": {
    "total_visions": 2,
    "active_visions": ["V001", "V002"],
    "visions": {
      "V001": {
        "name": "AI 助手开发",
        "status": "active",
        "priority": "P0",
        "progress": 35
      },
      "V002": {
        "name": "知识库管理",
        "status": "paused",
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
    }
  },
  
  "execution_context": {
    "current_vision_id": "V001",
    "current_task": "T005"
  }
}
```

**关键字段说明**：
- `vision_portfolio.visions`: 所有愿景的列表，每个愿景有独立状态
- `per_vision_states`: 每个愿景的当前执行状态（模式、活动任务等）
- `execution_context.current_vision_id`: 当前正在执行的愿景 ID（用于上下文切换）

## ✅ 验证清单

创建完成后，请逐项检查：

### 文件结构验证

- [ ] config.json 包含 multi_vision_system 配置
- [ ] state.json 包含 vision_portfolio 和 per_vision_states
- [ ] visions/ 目录下有示例愿景 V001_example
- [ ] hub/overview.md 和 hub/resources.md 存在
- [ ] docs/templates/ 包含全部 7 个模板文件
- [ ] plans/ 和 progress/ 是目录而非文件
- [ ] 每个愿景工作区包含 vision/problems/goals/tasks

### 功能验证

- [ ] 支持创建多个愿景（至少 10 个）
- [ ] 每个愿景有独立的状态追踪
- [ ] 全局总览视图显示所有愿景的全局状态
- [ ] 资源可以动态分配和调整
- [ ] 愿景间可以建立依赖关系
- [ ] AI 可以在不同愿景间切换上下文
- [ ] 六类文档模板与五层架构正确映射
- [ ] 进度记录支持按愿景分开追踪

### 文档一致性验证

- [ ] 所有模板中的路径引用正确（指向 visions/V001_xxx/）
- [ ] 模板支持跨愿景文档关联（xxx_global.md）
- [ ] 配置文件中的路径与实际目录结构一致
- [ ] 使用指南中的示例与实际情况相符

## 💡 预期效果（4.0 增强版）

创建完成后，一个新 AI 应该能够：

### 基础能力

1. ✅ 读取 README.md 理解系统核心理念（通俗易懂）
2. ✅ 读取 config.json 获取多愿景配置
3. ✅ 读取 state.json 了解当前正在执行哪个愿景的哪个任务
4. ✅ 通过 hub/overview.md 查看所有愿景的全局状态
5. ✅ 通过 hub/resources.md 了解资源分配情况

### 多愿景操作

6. ✅ 创建新愿景（复制模板，填写信息）
7. ✅ 在多个愿景间切换上下文（保存→加载→继续）
8. ✅ 查看愿景间的依赖关系
9. ✅ 根据优先级调整资源分配

### 任务执行

10. ✅ 从当前愿景的 tasks.md 选择任务
11. ✅ 创建对应的执行计划（plans/）
12. ✅ 记录执行进度（progress/）
13. ✅ 根据需要同步更新六类文档

### 文档同步

14. ✅ 理解五层架构与六类文档的映射关系
15. ✅ 在愿景层级创建对应的 BRD/PDD/AD
16. ✅ 在任务层级创建对应的 HLD/DD/DP
17. ✅ 保持文档与执行进度一致

### 中断恢复

18. ✅ 读取管理指南了解当前状态
19. ✅ 加载对应愿景的上下文
20. ✅ 阅读进度记录了解执行情况
21. ✅ 5 分钟内完全接手并继续执行

## 🎯 核心工作流程（4.0 多愿景版）

### 愿景创建流程

```
1. 提出新愿景（新项目想法）
   ↓
2. 创建愿景工作区（visions/V00X_xxx/）
   ↓
3. 填写愿景文档（vision.md）
   ↓
4. 分析与其他愿景的依赖关系
   ↓
5. 评估资源需求
   ↓
6. 确定优先级（P0/P1/P2）
   ↓
7. 添加到全局总览（更新 state.json）
   ↓
8. 分配初始资源（更新 hub/resources.md）
   ↓
9. 开始执行五层架构流程
```

### 多愿景执行流程

```
AI 启动会话
    ↓
读取 state.json
  → 查看 current_vision_id
  → 如果没有，选择优先级最高的 active 愿景
    ↓
加载愿景上下文
  → 读取 visions/{vision_id}/vision.md
  → 读取 visions/{vision_id}/tasks.md
  → 读取对应的计划文件
    ↓
执行任务
  → 按照计划逐步执行
  → 记录进度到 progress/{vision_id}.md
  → 同步更新六类文档
    ↓
更新状态
  → 更新 state.json 中的 per_vision_states
  → 如有必要，更新 hub/overview.md
```

### 上下文切换流程

```
当前正在执行 V001 的任务 T005
需要切换到 V002 处理紧急任务
    ↓
Step 1: 保存 V001 上下文
  → 更新 state.json: current_vision_id = null
  → 记录 progress_V001.md 当前进度
  → 标记 task_T005 状态为 paused
    ↓
Step 2: 加载 V002 上下文
  → 读取 visions/V002/vision.md
  → 读取 visions/V002/tasks.md
  → 更新 state.json: current_vision_id = V002
    ↓
Step 3: 开始执行 V002 的任务
  → 按照 V002 的计划执行
  → 记录 progress_V002.md
    ↓
Step 4: 完成后切回 V001（可选）
  → 保存 V002 状态
  → 加载 V001 的上下文
  → 继续执行 T005
```

### 文档同步流程

```
Vision 层确认
  ↓ 同步创建
  → BRD: docs/brd/brd_V001.md
  → PDD: docs/pdd/pdd_V001.md
  → AD: docs/ad/ad_V001.md
  
Problems 层拆解
  ↓ 同步更新
  → BRD/PDD/AD 的问题章节
  
Goals 层设定
  ↓ 同步创建
  → BRD/PDD/AD 的目标章节
  → HLD: docs/hld/hld_V001.md
  → DP: docs/dp/dp_V001.md
  
Tasks 层分解
  ↓ 同步创建
  → HLD 的任务章节
  → DD: docs/dd/dd_V001.md
  → 更新 DP
  
Plans 层制定
  ↓ 同步创建
  → DD 的计划章节
  → 更新 DP
  
执行计划
  ↓ 同步更新
  → DP 的进度状态
  → progress/progress_V001.md
```

## 🎯 补充说明（4.0 新增）

### 系统核心理念

- **文件即上下文**: 所有信息都保存在文件中，AI 通过读写文件来理解和更新状态
- **愿景隔离**: 每个愿景有独立工作区，互不干扰
- **全局总览视角**: 统一视图管理所有愿景，优化资源配置
- **分层管理**: 从抽象到具体，每一层都有明确的职责
- **文档同步**: 每层拆分动作同步生成对应文档章节
- **中断恢复**: 通过进度快照和文档记录，AI 可以在任何时候中断和恢复
- **动态调整**: 计划可以随时更新版本，文档同步更新
- **多 AI 协作**: 以 Markdown 文档为唯一协作载体

### ⚠️ 重要注意事项

1. **Markdown 格式**: 所有文档（plans, progress）都使用 `.md` 格式，不是 JSON
2. **技术标识保留**: 
   - `vision_portfolio` - JSON 字段名保持不变（技术实现）
   - `portfolio_management` - 配置项名称保持不变
   - `ats-portfolio-management.md` - 文件名保持不变
   - 但在文档说明中使用中文：**全局总览**、**多愿景管理**

3. **愿景工作区结构**: 
   - 每个愿景必须有 vision/problems/goals/tasks 四个核心文件
   - 六类文档在 `docs/` 子目录下按类型分区
   - 文档命名：`{type}_{VISION_ID}.md`

4. **配置文件关系**:
   - `config.json` = 系统设置（最大愿景数、启用的功能等）
   - `state.json` = 实时状态（当前正在做什么、进度如何）
   - 两个文件必须保持一致性

### 关键特性（4.0 增强）

1. **多愿景隔离 + 全局视角**
   - 每个愿景独立工作区
   - 全局总览提供统一视图
   - 资源池化管理

2. **动态资源分配**
   - 根据优先级分配 AI 和时间
   - 支持资源冲突检测和解决
   - 时间资源分周管理

3. **上下文快速切换**
   - 保存当前愿景状态
   - 加载新愿景上下文
   - 进度不丢失

4. **跨愿景协作**
   - 愿景间依赖管理
   - 资源共享机制
   - 协同关系记录

5. **完整文档体系**
   - 六类工程文档同步
   - 按愿景分区管理
   - 版本控制

### 与 3.0 的区别

| 特性 | ATS 3.0 | ATS 4.0 |
|------|---------|---------|
| 愿景数量 | 单愿景 | 多愿景（最多 10 个） |
| 资源管理 | 静态分配 | 动态调配池 |
| 文档组织 | 扁平结构 | 按愿景分区 |
| 协作能力 | 有限 | 跨愿景协作 |
| 适用范围 | 单一项目 | 多项目并行 |
| 全局视图 | 无 | 完整全局总览 |
| 上下文切换 | 不支持 | 快速切换 |

## 📌 使用说明

### 如何使用此提示词

1. **完整复制**: 将上面的完整提示词（从"任务：验证/创建 ATS 4.0..."到最后）复制

2. **发送给 AI**: 将提示词发送给另一个 AI 助手

3. **监督执行**: AI 会按照提示词验证或创建 ATS 4.0 系统

4. **验证结果**: 使用验证清单检查系统是否完整正确

### 适用场景（4.0）

- ✅ 初始化整个 ATS 4.0 多愿景任务管理系统
- ✅ 在新项目中建立 ATS 4.0 标准系统
- ✅ 培训新的 AI 助手理解 ATS 4.0 系统
- ✅ 系统损坏后的重建
- ✅ 多 AI 协作项目的标准化
- ✅ 从 ATS 3.0 升级到 4.0
- ✅ 验证现有系统是否符合 4.0 标准

### 自定义选项

如需调整，可以修改：
- `config.json` 中的项目信息和最大愿景数
- 初始愿景的内容和数量
- 技术栈配置
- 文档模板的具体格式
- 特定领域的要求

### 版本历史

- **v4.0** (2026-03-27): 新增多愿景并行管理能力
  - 愿景工作区隔离
  - 全局总览管理
  - 跨愿景协作
  - 动态资源分配
  
- **v3.0** (2026-03-18): 新增六类工程文档同步机制
- **v2.0** (2026-03-09): 更新术语"使命"为"目标"
- **v1.0** (2026-03-05): 初始版本

---

**现在请开始验证或创建这个 ATS 4.0 多愿景任务管理系统。如果在过程中有任何疑问，请先询问我确认后再继续。**

---

**文档位置**: `ats-system-prompt4.md`  
**创建日期**: 2026-03-27  
**维护者**: AI 任务管理系统  
**系统版本**: ATS 4.0
```
