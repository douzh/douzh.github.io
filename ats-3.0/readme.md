# ATS 3.0 - AI 任务管理系统

## 🎯 系统概述

**ATS (AI Task System)** - AI 自主驱动的任务管理系统 v3.0

### 核心特性

- ✅ **五层架构**: Vision → Problems → Goals → Tasks → Plans
- ✅ **六类文档**: BRD/PDD/AD/HLD/DD/DP同步机制
- ✅ **中断恢复**: 任意时间点中断后快速恢复
- ✅ **多 AI 协作**: 以文档为载体的无缝交接
- ✅ **动态调整**: 计划和文档可灵活更新
- ✅ **全链路追溯**: 层级间树状关联

## 📁 目录结构

```
ats-3.0/
├── config.json                 # 系统配置
├── state.json                  # 实时状态
├── vision.md                   # 项目愿景
├── problems.md                 # 问题拆解
├── goals.md                    # 目标定义
├── tasks.md                    # 任务列表
├── plans/                      # 执行计划
│   └── plan_XXX.md
├── progress/                   # 进度记录
│   └── progress_XXX.md
├── docs/                       # 六类工程文档
│   ├── brd/                    # 业务需求文档
│   ├── pdd/                    # 产品设计文档
│   ├── ad/                     # 架构文档
│   ├── hld/                    # 概要设计文档
│   ├── dd/                     # 详细设计文档
│   └── dp/                     # 开发计划文档
├── completed/                  # 已完成任务归档
└── archive/                    # 历史版本归档
```

## 🚀 快速开始

### 1. 启动 ATS 系统

```bash
# 读取配置文件
cat ats-3.0/config.json

# 查看当前状态
cat ats-3.0/state.json

# 了解项目愿景
cat ats-3.0/vision.md
```

### 2. AI 工作流程

```
读取 vision.md → 理解项目目标
读取 config.json → 获取配置信息
读取 state.json → 了解当前状态
    ↓
如果有 active_task:
  → 读取对应 task 文件
  → 读取对应 plan 文件
  → 继续执行
如果没有活动任务:
  → 从 available_tasks 选择下一个
  → 创建执行计划
  → 开始执行
```

### 3. 中断恢复流程

```
读取 ats-task-management.md
    ↓
定位当前层级
    ↓
加载对应文档 (BRD/PDD/AD/HLD/DD/DP)
    ↓
读取 state.json 和 progress/
    ↓
评估状态并继续执行
```

## 📊 五层架构与六类文档映射

| 层级 | 英文 | 中文 | 对应文档 | 说明 |
|------|------|------|----------|------|
| L1 | Vision | 愿景 | BRD/PDD/AD | Why - 为什么要做 |
| L2 | Problems | 问题 | BRD/PDD/AD | What's Wrong - 什么问题 |
| L3 | Goals | 目标 | BRD/PDD/AD/HLD/DP | What to Do - 要做什么 |
| L4 | Tasks | 任务 | HLD/DD/DP | How to Do - 如何做 |
| L5 | Plans | 计划 | DD/DP | Step by Step - 逐步执行 |

## 🔧 配置文件说明

### config.json

```json
{
  "project_name": "项目名称",
  "version": "3.0",
  "document_system": {
    "enabled": true,
    "types": ["BRD", "PDD", "AD", "HLD", "DD", "DP"],
    "sync_enabled": true
  },
  "state_management": {
    "current_mode": "vision_definition"
  }
}
```

### state.json

记录项目实时状态，包括：
- 当前模式（current_mode）
- 各阶段状态（vision_definition, problem_analysis等）
- 文档同步版本（document_sync）
- AI 会话信息（ai_session）

## 📝 使用指南

### 创建新项目

1. 复制 `ats-3.0/` 模板到新项目
2. 编辑 `config.json` 填写项目信息
3. 编辑 `vision.md` 定义项目愿景
4. 开始问题拆解和目标设定

### 任务执行

1. 从 `tasks.md` 选择任务
2. 在 `plans/` 创建执行计划
3. 在 `progress/` 记录执行进度
4. 同步更新对应文档（BRD/PDD/AD/HLD/DD/DP）

### 中断恢复

1. 新 AI 读取所有相关文件
2. 通过 `state.json` 了解当前状态
3. 通过 `progress/` 了解执行历史
4. 继续未完成的计划

## 🎓 最佳实践

### ✅ 推荐做法

- 每次 AI 会话前完整阅读相关文件
- 及时更新 state.json 和 progress 文件
- 文档变更时同步更新版本号
- 任务完成后立即归档

### ❌ 避免做法

- 不要跳过文档阅读直接执行
- 不要手动修改 JSON 文件而不更新相关字段
- 不要忘记更新文档同步状态
- 不要在多个 AI 同时修改同一文件

## 🔄 版本历史

- **v3.0** (2026-03-27): 新增六类工程文档同步机制
- **v2.0** (2026-03-09): 更新术语"使命"为"目标"
- **v1.0** (2026-03-05): 初始版本

## 📞 支持

- 系统文档：`ats-task-management.md`
- 配置示例：`config.json`
- 状态模板：`state.json`

---

**系统版本**: ATS 3.0  
**创建日期**: 2026-03-27  
**维护者**: AI 任务管理系统
