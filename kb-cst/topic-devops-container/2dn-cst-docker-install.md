# 常用中间件安装

## redis

```
# 拉取redis镜像
docker pull redis

# 启动容器的时候，并为其设置密码
docker run -d --name myredis -p 6379:6379 redis --requirepass "123456"
```
