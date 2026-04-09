# Neo4j 社区版安装指南

## 系统要求

- **操作系统**: macOS 14.5 (Sonoma)
- **Java**: JDK 17 或 JDK 21（必需）
- **内存**: 至少 4GB RAM
- **磁盘空间**: 至少 2GB 可用空间

## 当前状态

- ✅ Docker 已安装（但未运行）
- ⚠️ Java 版本: 1.8.0_212（需要升级到 JDK 17+）
- ❌ Homebrew 存在网络问题，无法正常安装包
- ❌ 网络连接不稳定，无法从官方源下载

---

## 推荐安装方法

### 方法 1：使用 Docker（⭐ 最推荐）

**优点**：无需配置 Java，一键启动，环境隔离

#### 步骤：

1. **启动 Docker Desktop**
   ```bash
   # 在应用程序中找到 Docker Desktop 并启动
   # 等待菜单栏中的 Docker 图标变为正常运行状态
   ```

2. **验证 Docker 运行**
   ```bash
   docker --version
   docker info
   ```

3. **拉取并运行 Neo4j**
   ```bash
   # 创建数据目录
   mkdir -p ~/neo4j/{data,logs,import,plugins}
   
   # 运行 Neo4j 容器
   docker run \
       --name neo4j \
       -p 7474:7474 -p 7687:7687 \
       -d \
       -v $HOME/neo4j/data:/data \
       -v $HOME/neo4j/logs:/logs \
       -v $HOME/neo4j/import:/var/lib/neo4j/import \
       -v $HOME/neo4j/plugins:/plugins \
       --env NEO4J_AUTH=neo4j/your_password \
       neo4j:latest
   ```

4. **访问 Neo4j Browser**
   - 打开浏览器访问：http://localhost:7474
   - 用户名：`neo4j`
   - 密码：`your_password`（首次登录会要求修改）

5. **常用 Docker 命令**
   ```bash
   # 查看 Neo4j 容器状态
   docker ps | grep neo4j
   
   # 查看日志
   docker logs neo4j
   
   # 停止 Neo4j
   docker stop neo4j
   
   # 启动 Neo4j
   docker start neo4j
   
   # 删除容器（数据保留在卷中）
   docker rm neo4j
   
   # 更新到最新版本
   docker pull neo4j:latest
   docker stop neo4j
   docker rm neo4j
   # 重新运行上面的 docker run 命令
   ```

---

### 方法 2：手动安装（需要稳定的网络）

**前提条件**：需要先安装 JDK 17 或 21

#### 步骤：

1. **安装 JDK 21**
   
   选项 A - 使用 Homebrew（需要修复网络问题）：
   ```bash
   brew install openjdk@21
   ```
   
   选项 B - 手动下载：
   - 访问：https://adoptium.net/temurin/releases/?version=21
   - 下载 macOS x64 版本的 JDK 21
   - 安装下载的 .pkg 文件

2. **配置 Java 环境变量**
   ```bash
   # 编辑 ~/.zshrc
   echo 'export JAVA_HOME=$(/usr/libexec/java_home -v 21)' >> ~/.zshrc
   echo 'export PATH="$JAVA_HOME/bin:$PATH"' >> ~/.zshrc
   source ~/.zshrc
   
   # 验证
   java -version
   # 应该显示 java version "21.x.x"
   ```

3. **下载 Neo4j 社区版**
   
   访问 Neo4j 下载中心：
   - https://neo4j.com/download-center/#community
   - 选择 "Linux / Mac" 版本
   - 下载最新的 `.tar.gz` 文件
   
   或使用命令行（如果网络允许）：
   ```bash
   cd ~/Applications
   curl -L -o neo4j.tar.gz "https://neo4j.com/artifact.php?name=neo4j-community-2026.03.1-unix.tar.gz"
   ```

4. **解压和安装**
   ```bash
   cd ~/Applications
   tar -xzf neo4j.tar.gz
   mv neo4j-community-* neo4j
   ```

5. **配置 Neo4j**
   ```bash
   cd ~/Applications/neo4j
   
   # 编辑配置文件
   vi conf/neo4j.conf
   
   # 确保以下配置项未被注释：
   # dbms.default_listen_address=0.0.0.0
   # server.bolt.listen_address=:7687
   # server.http.listen_address=:7474
   ```

6. **设置初始密码**
   ```bash
   bin/neo4j-admin dbms set-initial-password your_password
   ```

7. **启动 Neo4j**
   ```bash
   # 前台运行（用于测试）
   bin/neo4j console
   
   # 或后台运行
   bin/neo4j start
   ```

8. **访问 Neo4j Browser**
   - 打开浏览器访问：http://localhost:7474
   - 用户名：`neo4j`
   - 密码：`your_password`

9. **配置环境变量（可选）**
   ```bash
   echo 'export NEO4J_HOME=~/Applications/neo4j' >> ~/.zshrc
   echo 'export PATH="$NEO4J_HOME/bin:$PATH"' >> ~/.zshrc
   source ~/.zshrc
   ```

---

### 方法 3：修复 Homebrew 后安装

如果网络问题解决，可以尝试：

```bash
# 1. 修复 Homebrew git 配置
git -C "/usr/local/Homebrew" remote set-url origin https://github.com/Homebrew/brew

# 2. 更新 Homebrew
brew update

# 3. 安装 Neo4j（会自动安装依赖 openjdk@21）
brew install neo4j

# 4. 启动 Neo4j 服务
brew services start neo4j

# 5. 访问 http://localhost:7474
```

---

## 常见问题

### Q1: Docker 无法启动
**解决方案**：
- 确保已安装 Docker Desktop for Mac
- 检查系统偏好设置 -> 安全性与隐私 -> 允许 Docker
- 重启 Docker Desktop

### Q2: 端口冲突
如果 7474 或 7687 端口被占用：
```bash
# 查找占用端口的进程
lsof -i :7474
lsof -i :7687

# 修改 Docker 端口映射
docker run -p 7475:7474 -p 7688:7687 ... neo4j:latest
```

### Q3: 内存不足
Neo4j 默认可能需要较多内存，可以调整：
```bash
# Docker 方式：添加内存限制
docker run --memory=4g ... neo4j:latest

# 手动安装：编辑 conf/neo4j.conf
# server.memory.heap.initial_size=512m
# server.memory.heap.max_size=1g
```

### Q4: 忘记密码
```bash
# Docker 方式
docker exec -it neo4j neo4j-admin dbms set-initial-password new_password

# 手动安装
bin/neo4j-admin dbms set-initial-password new_password
```

---

## 验证安装

无论使用哪种方法，都可以通过以下方式验证：

1. **浏览器访问**：http://localhost:7474
2. **运行 Cypher 查询**：
   ```cypher
   RETURN "Hello Neo4j" AS message;
   ```
3. **检查版本**：
   ```cypher
   CALL dbms.components() YIELD name, versions RETURN *;
   ```

---

## 下一步

安装完成后，您可以：

1. 学习 Cypher 查询语言：https://neo4j.com/docs/cypher-manual/
2. 导入您的项目中的数据（参考项目中的 `import_10one_to_neo4j.py`）
3. 探索图数据库的功能和应用场景

---

## 技术支持

- Neo4j 官方文档：https://neo4j.com/docs/
- Neo4j 社区论坛：https://community.neo4j.com/
- GraphAcademy 免费课程：https://graphacademy.neo4j.com/
