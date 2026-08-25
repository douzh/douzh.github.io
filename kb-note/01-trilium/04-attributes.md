# Trilium 属性系统:标签/关系 与 标签定义/关系定义

[返回 Trilium 档案](README.md) | [数据库表设计与存储逻辑](03-db-design.md) | [可编程性说明](10-scripting.md) | [vs Notion 功能对标](11-vs-notion.md) | [返回总览](../note-soft.md)

> 四者的关系一句话:**标签/关系是"数据",标签定义/关系定义是"数据的模式(schema)"**——定义本身也是一条属性,只是名字带前缀、挂在父笔记/模板上、作用于子笔记。

## 一、概念区分

| | 是什么 | 挂在哪 | 回答的问题 | 存储形态 |
| --- | --- | --- | --- | --- |
| **标签** | 数据实例(标量键值对) | 笔记本身 | 这篇笔记**有什么属性**(分类/状态/数值) | `(noteId, name, value)` |
| **标签定义** | 模式(规则) | 父笔记/模板 | 子笔记**允许/应该**有哪些标签、什么类型、单值还是多值、是否表单化 | 也是一行 attribute,name 为 `label:xxx` |
| **关系** | 数据实例(带名三元组) | 笔记本身 | 这篇笔记**和谁**有什么关系 | `(noteId, name, 目标noteId)` |
| **关系定义** | 模式(规则) | 父笔记/模板 | 子笔记允许哪些关系、是否表单化、目标约束 | 也是一行 attribute,name 为 `relation:xxx` |

书写语法:`#` = 标签,`~` = 关系;定义就是名字加 `label:` / `relation:` 前缀。

## 二、标签 vs 标签定义

**标签(用)**——直接挂在一篇笔记上:

```
#book                    ← 无值标签,纯分类
#year=1918               ← 带值标签
#status=reading
```
搜索:`#book #year=1918`、`#year>=1900`

**标签定义(先声明)**——挂在模板或父笔记上,声明"子笔记可以有什么字段":

```
#label:year=number,promoted
#label:genre=enum(小说,散文,诗歌),promoted,multi
#label:finished=boolean,promoted
```

可声明的**值类型**:`text`(默认)/ `number` / `boolean`(复选框)/ `date` / `datetime` / `url` / `enum(a,b,c)`(下拉);**修饰符**:`promoted`(表单化显示)、`multi` / `single`(多值/单值)。

**定义带来的三个效果**(都是给"子笔记"的):
1. **表单化**:勾了 `promoted` 的字段固定显示在子笔记顶部,像 Notion 的属性栏——数字出数字输入框、date 出日期选择器、enum 出下拉
2. **输入约束/自动补全**:类型校验 + 枚举下拉,防止 `#year=一九一八` 这种脏数据
3. **供给视图**:promoted 属性可直接作为 Collections 表格的列、看板的分组依据(v0.104+ 官方能力)

**关键区别再强调一遍**:定义放在模板/父笔记上,**值**由每篇子笔记自己填——定义不携带数据,只携带"字段规则"。

## 三、关系 vs 关系定义

**关系(用)**——挂在一篇笔记上,指向另一篇笔记:

```
~author=@鲁迅            ← "本文的作者是鲁迅"
~publishedIn=@新青年
```
搜索:`~author=@鲁迅`(查鲁迅写的所有);反向"谁引用了鲁迅"用 SQL 查 value=鲁迅noteId。

**关系定义(先声明)**:

```
#relation:author=promoted,multi
#relation:publishedIn=promoted
```

效果:子笔记顶部出现**笔记选择器**字段(点开搜索选择目标笔记),而不是手写 `~`;`multi` 允许选多个目标。部分版本还支持 `inverse`(反向别名:A~author→B 时自动维护 B 侧显示"作品")。

## 四、端到端示例:藏书管理

**第 1 步,建"书籍模板"笔记**,挂上定义:

```
#label:year=number,promoted
#label:genre=enum(小说,散文,诗歌),promoted
#relation:author=promoted
#relation:inCollection=promoted
```

**第 2 步,建《狂人日记》**(套用该模板,或作为模板笔记的子笔记):
- 顶部自动出现 4 个表单字段,填:year=1918、genre=小说、author→选择《鲁迅》、inCollection→选择《呐喊》
- 底层实际生成:

```
《狂人日记》
  #year=1918
  #genre=小说
  ~author=<鲁迅noteId>
  ~inCollection=<呐喊noteId>
```

**第 3 步,检索与消费**:

```
搜索:#genre=小说 ~author=@鲁迅        ← 鲁迅的短篇小说
SQL:SELECT notes.title FROM notes
     JOIN attributes ON notes.noteId=attributes.noteId
     WHERE attributes.name='year' AND attributes.value='1918'
```

**第 4 步,可视化**:建一个 Relation Map 笔记,挂 `~map:author` `~map:inCollection`,作者/文集关系自动渲染成图。

## 五、两个易混点

1. **定义 ≠ 继承**:`isInheritable`(可继承)是另一回事——它是把**标签/关系本身连同值**沿子树下传(如父笔记挂可继承的 `#type=文集`,所有子笔记自动是文集);定义只传"字段规则",不传值。
2. **系统保留名**:`~template`(套模板)、`~renderNote`、`~run`、`~internalLink`(正文链接自动生成的系统关系)等被 Trilium 保留,自定义时避开——本库实测存在这些名字(见 [db-design.md](03-db-design.md) 第 5 节)。
