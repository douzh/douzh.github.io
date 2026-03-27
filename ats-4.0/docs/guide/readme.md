# ATS 4.0 - 多愿景任务管理系统

## 🎯 系统概述

**ATS 4.0 (AI Task System)** - AI 自主驱动的多愿景任务管理系统

### 核心特性（4.0 升级版）

- ✅ **多愿景并行**: 同时管理多个独立或关联的愿景
- ✅ **五层架构**: Vision → Problems → Goals → Tasks → Plans
- ✅ **六类文档**: BRD/PDD/AD/HLD/DD/DP同步机制
- ✅ **全局总览管理**: 统一视角下的资源优化配置
- ✅ **中断恢复**: 任意时间点中断后快速恢复
- ✅ **跨愿景协作**: 愿景间的依赖管理和资源共享
- ✅ **动态资源分配**: 根据优先级和进度智能调配资源

---

## 📁 目录结构

```
ats-4.0/
├── config.json                     # 系统配置（含多愿景配置）
├── state.json                      # 全局状态 + 各愿景状态
├── vision_template.md              # 愿景模板
├── readme.md                       # 本文档
├── ats-portfolio-management.md     # 多愿景管理指南
│
├── visions/                        # 各愿景独立工作区
│   ├── V001_vision_name/           # 愿景 1 工作区
│   │   ├── vision.md               # 愿景文档
│   │   ├── problems.md             # 问题拆解
│   │   ├── goals.md                # 目标定义
│   │   └── tasks.md                # 任务列表
│   └── V002_vision_name/           # 愿景 2 工作区
│
├── hub/                     # 全局总览视图
│   ├── overview.md       # 总览仪表板
│   └── resources.md      # 资源配置表
│
├── docs/                           # 六类工程文档（按愿景分区）
│   ├── brd/                        # 业务需求文档
│   ├── pdd/                        # 产品设计文档
│   ├── ad/                         # 架构文档
│   ├── hld/                        # 概要设计文档
│   ├── dd/                         # 详细设计文档
│   └── dp/                         # 开发计划文档
│
├── plans/                          # 执行计划
├── progress/                       # 进度记录
├── completed/                      # 已完成归档
└── archive/                        # 历史版本
```

---

## 🚀 快速开始

### 1. 添加第一个愿景

```bash
# Step 1: 进入 ats-4.0 目录
cd ats-4.0

# Step 2: 查看系统配置
cat config.json

# Step 3: 创建愿景工作区
New-Item -ItemType Directory -Path "visions/V001_your_vision"

# Step 4: 复制愿景模板
Copy-Item ../vision_template.md visions/V001_your_vision/vision.md

# Step 5: 编辑愿景文档，填写愿景信息
```

### 2. 更新全局总览

编辑 `state.json`，在 `vision_portfolio.visions` 中添加新愿景：

```json
{
  "vision_portfolio": {
    "visions": {
      "V001": {
        "name": "您的愿景名称",
        "status": "active",
        "priority": "P0",
        "progress": 0
      }
    }
  }
}
```

### 3. 开始执行

按照五层架构逐步推进：

```
Vision (愿景) 
  ↓ 在 visions/V001/vision.md 中定义
Problems (问题)
  ↓ 创建 visions/V001/problems.md
Goals (目标)
  ↓ 创建 visions/V001/goals.md
Tasks (任务)
  ↓ 创建 visions/V001/tasks.md
Plans (计划)
  ↓ 在 plans/ 创建执行计划
```

---

## 🎯 核心概念

### 愿景 (Vision)

一个独立的战略目标或项目方向，例如：
- V001: AI 助手开发
- V002: 知识库管理系统
- V003: 数据分析平台

每个愿景都有：
- 独立的 ID 和名称
- 独立的五层架构文件
- 独立的六类文档
- 独立的状态追踪

### 全局总览 (Portfolio)

所有愿景的集合视图，用于：
- 全局资源优化
- 愿景间依赖管理
- 整体进度追踪
- 冲突检测和解决

### 上下文切换

AI 在不同愿景间切换工作的机制：
- 一次只执行一个愿景的任务
- 切换时保存当前上下文
- 快速加载新愿景的上下文

---

## 📊 与 3.0 的区别

| 特性 | ATS 3.0 | ATS 4.0 |
|------|---------|---------|
| 愿景数量 | 单愿景 | 多愿景（最多 10 个） |
| 资源管理 | 静态分配 | 动态调配 |
| 文档组织 | 扁平结构 | 按愿景分区 |
| 协作能力 | 有限 | 跨愿景协作 |
| 适用范围 | 单一项目 | 多项目并行 |

---

## 🔧 配置文件

### config.json

```json
{
  "multi_vision_system": {
    "enabled": true,
    "max_visions": 10,
    "concurrent_execution": true
  },
  
  "document_system": {
    "enabled": true,
    "types": ["BRD", "PDD", "AD", "HLD", "DD", "DP"],
    "per_vision_docs": true
  },
  
  "portfolio_management": {
    "enabled": true,
    "resource_pooling": true,
    "dependency_tracking": true
  }
}
```

### state.json

包含三个层次的状态：
1. **全局状态**: 全局总览整体状态
2. **愿景状态**: 每个愿景的独立状态
3. **执行上下文**: 当前正在执行的愿景和任务

---

## 📖 使用文档

| 文档 | 用途 |
|------|------|
| [ats-portfolio-management.md](./ats-portfolio-management.md) | 多愿景管理详细指南 |
| [hub/overview.md](../hub/overview.md) | 全局总览总览 |
| [hub/resources.md](../hub/resources.md) | 资源配置表 |
| [visions/V001_example/vision.md](../visions/V001_example/vision.md) | 愿景示例 |

---

## 🎓 最佳实践

### ✅ 推荐做法

1. **愿景命名**: 使用 `V001_short_name` 格式
2. **优先级管理**: P0 愿景不超过 2 个
3. **定期审查**: 每周更新全局总览状态
4. **依赖管理**: 明确记录愿景间关系
5. **上下文切换**: 避免频繁切换，降低效率

### ❌ 避免做法

1. 同时执行太多愿景（建议≤5 个 active）
2. 资源平均分配（优先保证 P0）
3. 忽视依赖关系（导致后期阻塞）
4. 文档不同步（失去可追溯性）

---

## 🔄 版本历史

- **v4.0** (2026-03-27): 新增多愿景并行管理能力
- **v3.0** (2026-03-27): 六类工程文档同步机制
- **v2.0** (2026-03-09): 更新术语
- **v1.0** (2026-03-05): 初始版本

---

## 📞 支持

- **管理指南**: [ats-portfolio-management.md](./ats-portfolio-management.md)
- **愿景模板**: [vision_template.md](./vision_template.md)
- **示例愿景**: [visions/V001_example/](./visions/V001_example/)

---

**系统版本**: ATS 4.0  
**创建日期**: 2026-03-27  
**维护者**: AI 任务管理系统  
**最后更新**: 2026-03-27

