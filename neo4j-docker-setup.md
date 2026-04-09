# Neo4j Docker 安装完成报告

## ✅ 安装成功！

Neo4j 社区版已成功通过 Docker 安装并运行。

---

## 📊 当前配置信息

- **Neo4j 版本**: 4.4.2
- **容器名称**: neo4j
- **状态**: 运行中 ✅

### 访问地址

- **Neo4j Browser**: http://localhost:7474
- **Bolt 协议端口**: localhost:7687

### 登录凭据

- **用户名**: `neo4j`
- **密码**: `password`

> ⚠️ **重要**: 首次登录后，系统会要求您修改密码

---

## 📁 数据持久化目录

所有数据都保存在您的主目录下，即使删除容器数据也不会丢失：

```
~/neo4j/
├── data/      # 数据库文件
├── logs/      # 日志文件
├── import/    # 数据导入目录
└── plugins/   # 插件目录
```

---

## 🔧 常用管理命令

### 查看容器状态
```bash
docker ps | grep neo4j
```

### 查看实时日志
```bash
docker logs -f neo4j
```

### 停止 Neo4j
```bash
docker stop neo4j
```

### 启动 Neo4j
```bash
docker start neo4j
```

### 重启 Neo4j
```bash
docker restart neo4j
```

### 查看容器详细信息
```bash
docker inspect neo4j
```

### 进入容器内部
```bash
docker exec -it neo4j bash
```

### 删除容器（数据保留）
```bash
docker stop neo4j
docker rm neo4j
```

### 重新创建容器
```bash
docker run \
    --name neo4j \
    -p 7474:7474 -p 7687:7687 \
    -d \
    -v $HOME/neo4j/data:/data \
    -v $HOME/neo4j/logs:/logs \
    -v $HOME/neo4j/import:/var/lib/neo4j/import \
    -v $HOME/neo4j/plugins:/plugins \
    --env NEO4J_AUTH=neo4j/password \
    neo4j:latest
```

---

## 🚀 快速开始

### 1. 访问 Neo4j Browser

在浏览器中打开：http://localhost:7474

使用以下凭据登录：
- 用户名：`neo4j`
- 密码：`password`

### 2. 测试连接

在 Neo4j Browser 中运行以下 Cypher 查询：

```cypher
// 测试查询
RETURN "Hello Neo4j" AS message;

// 查看数据库信息
CALL dbms.components() YIELD name, versions RETURN *;

// 创建测试节点
CREATE (n:Person {name: "Test User", age: 25})
RETURN n;

// 查询节点
MATCH (n:Person) RETURN n;
```

### 3. 导入项目数据

您的项目中有一个 Python 脚本可以导入数据到 Neo4j：

```bash
# 查看导入脚本
cat import_10one_to_neo4j.py

# 运行导入脚本（需要先安装 neo4j Python 驱动）
pip install neo4j
python import_10one_to_neo4j.py
```

---

## 🔐 安全管理

### 修改密码

**方法 1：通过 Neo4j Browser**
1. 首次登录时会提示修改密码
2. 或在 Browser 中运行：`:server change-password`

**方法 2：通过命令行**
```bash
docker exec -it neo4j neo4j-admin dbms set-initial-password new_password
docker restart neo4j
```

### 限制访问IP

如果需要限制只能从本地访问，修改启动命令中的端口映射：
```bash
# 只允许本地访问
-p 127.0.0.1:7474:7474 -p 127.0.0.1:7687:7687
```

---

## 📝 配置文件位置

Neo4j 配置文件位于容器内：`/var/lib/neo4j/conf/neo4j.conf`

如需修改配置：

```bash
# 复制配置文件到本地
docker cp neo4j:/var/lib/neo4j/conf/neo4j.conf ~/neo4j/neo4j.conf

# 编辑配置文件
vi ~/neo4j/neo4j.conf

# 将修改后的配置复制回容器
docker cp ~/neo4j/neo4j.conf neo4j:/var/lib/neo4j/conf/neo4j.conf

# 重启容器
docker restart neo4j
```

---

## 🔄 升级到最新版本

由于网络问题，当前无法直接拉取最新镜像。您可以稍后尝试：

```bash
# 1. 停止并删除当前容器
docker stop neo4j
docker rm neo4j

# 2. 拉取最新镜像
docker pull neo4j:latest

# 3. 重新启动容器（数据会保留）
docker run \
    --name neo4j \
    -p 7474:7474 -p 7687:7687 \
    -d \
    -v $HOME/neo4j/data:/data \
    -v $HOME/neo4j/logs:/logs \
    -v $HOME/neo4j/import:/var/lib/neo4j/import \
    -v $HOME/neo4j/plugins:/plugins \
    --env NEO4J_AUTH=neo4j/password \
    neo4j:latest
```

---

## 🐛 故障排查

### 问题 1：无法访问 http://localhost:7474

**检查容器是否运行：**
```bash
docker ps | grep neo4j
```

**检查端口是否被占用：**
```bash
lsof -i :7474
lsof -i :7687
```

**查看容器日志：**
```bash
docker logs neo4j
```

### 问题 2：内存不足

Neo4j 可能需要较多内存，可以调整 JVM 堆大小：

```bash
# 编辑配置文件
docker exec -it neo4j vi /var/lib/neo4j/conf/neo4j.conf

# 修改以下参数：
# server.memory.heap.initial_size=512m
# server.memory.heap.max_size=1g

# 重启容器
docker restart neo4j
```

### 问题 3：忘记密码

```bash
# 重置密码
docker exec -it neo4j neo4j-admin dbms set-initial-password new_password
docker restart neo4j
```

### 问题 4：容器无法启动

```bash
# 查看详细错误
docker logs neo4j

# 检查数据目录权限
ls -la ~/neo4j/

# 修复权限
chmod -R 777 ~/neo4j/
```

---

## 📚 学习资源

- **Neo4j 官方文档**: https://neo4j.com/docs/
- **Cypher 查询语言手册**: https://neo4j.com/docs/cypher-manual/
- **GraphAcademy 免费课程**: https://graphacademy.neo4j.com/
- **Neo4j 社区论坛**: https://community.neo4j.com/

---

## 🎯 下一步建议

1. **熟悉 Cypher 查询语言**
   - 学习基本的 CREATE、MATCH、RETURN 语句
   - 理解节点、关系、属性的概念

2. **导入项目数据**
   - 查看并运行 `import_10one_to_neo4j.py` 脚本
   - 将您的知识库数据导入 Neo4j

3. **探索图数据库应用**
   - 知识图谱构建
   - 社交网络分析
   - 推荐系统
   - 路径查找

4. **集成到您的应用**
   - 使用 Neo4j Python Driver
   - 使用 Neo4j JavaScript Driver
   - 通过 REST API 访问

---

## ✨ 总结

Neo4j 已经成功安装并运行！您现在可以：

✅ 通过浏览器访问 http://localhost:7474  
✅ 使用用户名 `neo4j` 和密码 `password` 登录  
✅ 开始探索图数据库的强大功能  
✅ 导入您的项目数据  

祝您使用愉快！🎉
