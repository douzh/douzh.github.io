# Neo4j 快速参考卡

## 🚀 快速启动

```bash
# 查看 Neo4j 状态
./neo4j-manage.sh status

# 启动 Neo4j
./neo4j-manage.sh start

# 停止 Neo4j
./neo4j-manage.sh stop

# 重启 Neo4j
./neo4j-manage.sh restart
```

---

## 🌐 访问信息

| 项目 | 值 |
|------|-----|
| **浏览器地址** | http://localhost:7474 |
| **Bolt 协议** | bolt://localhost:7687 |
| **用户名** | `neo4j` |
| **密码** | `password` |
| **版本** | 4.4.2 (Community) |

---

## 📁 重要目录

```
~/neo4j/
├── data/         # 数据库文件（最重要！）
├── logs/         # 日志文件
├── import/       # CSV 数据导入目录
└── plugins/      # APOC 等插件
```

---

## 🔧 常用命令速查

### 容器管理
```bash
docker ps | grep neo4j           # 查看运行状态
docker logs neo4j                # 查看日志
docker logs -f neo4j             # 实时日志
docker exec -it neo4j bash       # 进入容器
```

### 使用管理脚本
```bash
./neo4j-manage.sh info           # 显示连接信息
./neo4j-manage.sh version        # 查看版本
./neo4j-manage.sh logs           # 实时日志
./neo4j-manage.sh reset-pwd      # 重置密码
./neo4j-manage.sh help           # 帮助信息
```

---

## 💡 Cypher 查询示例

```cypher
// 1. 测试查询
RETURN "Hello Neo4j" AS message;

// 2. 创建节点
CREATE (p:Person {name: "张三", age: 30})
RETURN p;

// 3. 创建关系
CREATE (a:Person {name: "李四"})-[:FRIENDS]->(b:Person {name: "王五"})
RETURN a, b;

// 4. 查询节点
MATCH (p:Person) RETURN p;

// 5. 条件查询
MATCH (p:Person) WHERE p.age > 25 RETURN p.name, p.age;

// 6. 删除所有数据（谨慎使用！）
MATCH (n) DETACH DELETE n;

// 7. 查看数据库信息
CALL dbms.components() YIELD name, versions RETURN *;
```

---

## 🐛 故障排查

### 无法访问？
```bash
# 检查容器状态
./neo4j-manage.sh status

# 查看日志
./neo4j-manage.sh logs

# 检查端口
lsof -i :7474
lsof -i :7687
```

### 忘记密码？
```bash
./neo4j-manage.sh reset-pwd
```

### 清理重新开始？
```bash
docker stop neo4j
docker rm neo4j
rm -rf ~/neo4j/data/*
./neo4j-manage.sh start
```

---

## 📚 学习路径

1. **初学者**: https://graphacademy.neo4j.com/courses/beginning-neo4j/
2. **Cypher 查询**: https://neo4j.com/docs/cypher-manual/current/
3. **官方文档**: https://neo4j.com/docs/
4. **社区论坛**: https://community.neo4j.com/

---

## 🔗 相关文档

- [详细安装指南](neo4j-installation-guide.md)
- [Docker 设置报告](neo4j-docker-setup.md)
- [管理脚本](neo4j-manage.sh)

---

**提示**: 将此文件保存为书签，方便随时查阅！
