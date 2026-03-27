# 文档模板使用指南

## 📋 模板列表

ATS 4.0 提供以下标准文档模板，支持多愿景并行管理：

### 核心模板

| 模板文件 | 用途 | 说明 |
|----------|------|------|
| [vision_template.md](./vision_template.md) | 愿景定义 | 定义愿景的终极目标、范围、资源配置 |
| [brd_template.md](./brd_template.md) | 业务需求 | 业务需求分析、投资回报、风险评估 |
| [pdd_template.md](./pdd_template.md) | 产品设计 | 产品功能规划、用户体验设计 |
| [ad_template.md](./ad_template.md) | 架构设计 | 系统架构、技术选型、数据架构 |
| [hld_template.md](./hld_template.md) | 概要设计 | 系统总体设计、模块划分 |
| [dd_template.md](./dd_template.md) | 详细设计 | 类设计、数据库设计、接口设计 |
| [dp_template.md](./dp_template.md) | 开发计划 | 执行步骤、时间规划、资源需求 |

---

## 🎯 多愿景文档命名规范

### 按愿景分区的文档命名

每个愿景的文档应遵循以下命名规范：

```
docs/
├── brd/
│   ├── brd_V001.md          # 愿景 V001 的 BRD
│   ├── brd_V002.md          # 愿景 V002 的 BRD
│   └── brd_global.md        # 全局 BRD（可选）
├── pdd/
│   ├── pdd_V001.md
│   └── pdd_V002.md
├── ad/
│   ├── ad_V001.md
│   └── ad_V002.md
├── hld/
│   ├── hld_V001.md
│   └── hld_V002.md
├── dd/
│   ├── dd_V001.md
│   └── dd_V002.md
└── dp/
    ├── dp_V001.md
    └── dp_V002.md
```

### 跨愿景文档

对于涉及多个愿景的全局性文档，使用 `_global` 后缀：

- `brd_global.md` - 全局业务需求
- `ad_global.md` - 全局架构设计
- `dp_global.md` - 全局开发计划

---

## 🚀 使用流程

### 1. 创建新愿景

```bash
# Step 1: 创建愿景工作区
New-Item -ItemType Directory -Path "visions/V001_your_vision"

# Step 2: 复制愿景模板
Copy-Item ../docs/templates/vision_template.md visions/V001_your_vision/vision.md

# Step 3: 编辑愿景文档
# 填写愿景 ID、名称、目标等信息
```

### 2. 创建六类工程文档

为每个愿景创建完整的六类文档：

```bash
# 在愿景工作区创建文档目录
New-Item -ItemType Directory -Path "visions/V001_your_vision/brd"
New-Item -ItemType Directory -Path "visions/V001_your_vision/pdd"
New-Item -ItemType Directory -Path "visions/V001_your_vision/ad"
New-Item -ItemType Directory -Path "visions/V001_your_vision/hld"
New-Item -ItemType Directory -Path "visions/V001_your_vision/dd"
New-Item -ItemType Directory -Path "visions/V001_your_vision/dp"

# 复制模板并填充内容
Copy-Item ../../docs/templates/brd_template.md visions/V001_your_vision/brd/brd_V001.md
Copy-Item ../../docs/templates/pdd_template.md visions/V001_your_vision/pdd/pdd_V001.md
# ... 其他文档
```

### 3. 替换模板变量

在所有模板中，需要替换以下变量：

- `{VISION_ID}` → 实际愿景 ID（如 V001）
- `[方括号内容]` → 实际内容
- `YYYY-MM-DD` → 实际日期
- `[填写负责人]` → 实际负责人

---

## 📖 文档关联关系

### 五层架构文档流

```
Vision (愿景)
  ↓ 指导
Problems (问题)
  ↓ 驱动
Goals (目标)
  ↓ 分解
Tasks (任务)
  ↓ 执行
Plans (计划)
```

### 六类工程文档流

```
BRD (业务需求)
  ↓ 指导
PDD (产品设计)
  ↓ 指导
AD (架构设计)
  ↓ 指导
HLD (概要设计)
  ↓ 指导
DD (详细设计)
  ↓ 指导
DP (开发计划)
```

### 文档路径规范

在愿景 V001 的文档中，使用以下相对路径：

```markdown
# 从 docs/brd/brd_V001.md 引用

## 上游文档（愿景工作区）
[../visions/V001/vision.md](../../visions/V001/vision.md)
[../visions/V001/problems.md](../../visions/V001/problems.md)
[../visions/V001/goals.md](../../visions/V001/goals.md)
[../visions/V001/tasks.md](../../visions/V001/tasks.md)

## 同级文档（同目录）
[./pdd_V001.md](./pdd_V001.md)
[./ad_V001.md](./ad_V001.md)

## 下游文档
[../../plans/plan_V001_001.md](../../plans/plan_V001_001.md)
[../../progress/progress_V001.md](../../progress/progress_V001.md)
```

---

## 🎓 最佳实践

### ✅ 推荐做法

1. **文档同步更新**
   - 保持六类文档与五层架构的一致性
   - 定期审查和更新文档

2. **版本控制**
   - 使用版本文档号（v1.0, v1.1, v2.0）
   - 记录所有变更历史

3. **跨愿景协作**
   - 明确标注全局文档和愿景专属文档
   - 在文档中说明与其他愿景的关系

4. **模板使用**
   - 始终基于模板创建文档
   - 保持文档格式统一

### ❌ 避免做法

1. 文档与实际脱节
2. 缺少必要的审批流程
3. 忽视文档间的关联关系
4. 不使用统一的命名规范

---

## 🔍 常见问题

### Q1: 什么时候需要创建全局文档？

**A**: 当多个愿景共享相同的业务需求、架构设计或开发计划时，可以创建全局文档。例如：
- 多个愿景使用相同的技术架构 → `ad_global.md`
- 多个愿景属于同一个大项目 → `brd_global.md`

### Q2: 如何管理文档版本？

**A**: 
- 小改动：更新版本号（v1.0 → v1.1）
- 大改动：更新主版本号（v1.0 → v2.0）
- 在变更历史中记录每次修改

### Q3: 愿景完成后文档如何处理？

**A**: 
- 将完成的愿景文档移动到 `completed/V001/` 目录
- 保持文档结构完整
- 添加完成标记和日期

---

## 📞 支持资源

### 相关文档

- [系统概述](./guide/readme.md)
- [多愿景管理指南](./guide/ats-portfolio-management.md)
- [升级验证清单](./guide/upgrade-verification.md)

### 示例参考

- [V001 示例愿景](../visions/V001_example/vision.md)

---

**文档版本**: v1.0  
**最后更新**: 2026-03-27  
**维护者**: ATS 4.0 系统
