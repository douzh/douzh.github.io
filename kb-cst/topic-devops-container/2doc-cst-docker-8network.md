容器间要能相互通信，需要同在一个网络中。
docker容器在创建时若不指定网络驱动时会默认归属到bridge网络。
使用 docker inspect 指令查看两个容器是否同属一个network，如果不是，使用docker network将两个容器连接起来，使他们在同一个网络network里即可。
查看docker 网络:
docker network ls
首先创建一个网络：
docker network create networkName
将容器连到创建的网络中（每个容器都要连到这个网络里）：
docker network connect networkName containerName
查看网络内的容器信息：
docker network inspect networkName
使用docker network --help 获取更多相关操作详情。
可以在运行容器时直接指定连接network：
docker run --network networkName imageName
