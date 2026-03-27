# ATS 4.0 - 快速导航

## 🎯 系统入口

**ATS 4.0** - AI 自主驱动的多愿景任务管理系统

### 核心文档

| 文档类型 | 路径 | 说明 |
|----------|------|------|
| 📘 **使用指南** | [docs/guide/readme.md](./docs/guide/readme.md) | 系统概述和快速开始 |
| 📗 **管理指南** | [docs/guide/ats-portfolio-management.md](./docs/guide/ats-portfolio-management.md) | 多愿景管理详细指南 |
| 📙 **升级验证** | [docs/guide/upgrade-verification.md](./docs/guide/upgrade-verification.md) | ATS 4.0 升级验证清单 |

---

## 📁 目录结构

```
ats-4.0/
├── QUICK-NAV.md              # 本文档 - 快速导航
├── config.json               # 系统配置
├── state.json                # 实时状态
│
├── docs/                     # 📚 文档库
│   ├── guide/                # 使用指南
│   │   ├── readme.md
│   │   ├── ats-portfolio-management.md
│   │   └── upgrade-verification.md
│   └── templates/            # 模板文件
│       └── vision_template.md
│
├── visions/                  # 🎯 愿景工作区（每个愿景独立）
│   ├── V001_example/         # 愿景 1 示例
│   │   ├── vision.md         # 愿景文档
│   │   ├── problems.md       # 问题拆解
│   │   ├── goals.md          # 目标定义
│   │   ├── tasks.md          # 任务列表
│   │   ├── brd/              # 业务需求文档
│   │   ├── pdd/              # 产品设计文档
│   │   ├── ad/               # 架构文档
│   │   ├── hld/              # 概要设计文档
│   │   ├── dd/               # 详细设计文档
│   │   └── dp/               # 开发计划文档
│   └── V002_your_vision/     # 愿景 2（待创建）
│
├── hub/                    # 🎯 全局管理中心
│   ├── overview.md         # 全局总览
│   └── resources.md        # 资源配置
│
├── plans/                    # 📋 执行计划（按愿景 + 任务）
├── progress/                 # 📊 进度记录（按愿景 + 任务）
├── completed/                # ✅ 已完成归档
└── archive/                  # 🗄️ 历史版本
```

---

## 🚀 快速操作

### 1. 创建新愿景

```powershell
# Step 1: 创建愿景工作区
New-Item -ItemType Directory -Path "visions/V002_your_vision"

# Step 2: 复制愿景模板
Copy-Item docs/templates/vision_template.md visions/V002_your_vision/vision.md

# Step 3: 编辑愿景文档
# 打开 visions/V002_your_vision/vision.md 填写信息
```

### 2. 查看投资组合

```powershell
# 查看所有愿景的全局状态
cat hub/overview.md
```

### 3. 切换愿景上下文

```
当前在 V001 → 切换到 V002
1. 保存 V001 进度
2. 更新 state.json: active_vision_id = "V002"
3. 读取 V002 的 vision.md
```

---

## 📊 当前状态

### 愿景列表

| 愿景 ID | 名称 | 状态 | 进度 | 文档链接 |
|---------|------|------|------|----------|
| V001 | 示例愿景 | active | 0% | [vision.md](./visions/V001_example/vision.md) |

### 资源配置

查看完整资源配置：[resources.md](./hub/resources.md)

---

## 🎓 新手引导

### 第一次使用

1. **阅读系统概述**
   - [docs/guide/readme.md](./docs/guide/readme.md)
   
2. **查看示例愿景**
   - [visions/V001_example/vision.md](./visions/V001_example/vision.md)

3. **创建第一个愿景**
   - 按照快速开始指南操作

4. **开始执行**
   - 按照五层架构逐步推进

### 从 ATS 3.0 升级

如果您之前使用 ATS 3.0：

1. 您的内容现在应该移动到 `visions/V001_legacy/`
2. 使用方式与 3.0 完全相同
3. 可以随时添加 V002、V003 等新愿景

详细升级说明：[docs/guide/upgrade-verification.md](./docs/guide/upgrade-verification.md)

---

## 🔧 配置文件

### config.json

```json
{
  "multi_vision_system": {
    "enabled": true,
    "max_visions": 10
  },
  "document_system": {
    "per_vision_docs": true
  }
}
```

### state.json

包含全局状态和各愿景独立状态。

---

## 📞 帮助

### 查找文档

- **系统使用**: [docs/guide/readme.md](./docs/guide/readme.md)
- **详细指南**: [docs/guide/ats-portfolio-management.md](./docs/guide/ats-portfolio-management.md)
- **模板文件**: [docs/templates/vision_template.md](./docs/templates/vision_template.md)

### 常见问题

详见 [docs/guide/ats-portfolio-management.md](./docs/guide/ats-portfolio-management.md) 的"常见问题"章节。

---

**系统版本**: ATS 4.0  
**最后更新**: 2026-03-27  
**导航维护**: 随系统更新同步
