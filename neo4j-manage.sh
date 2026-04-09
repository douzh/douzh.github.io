#!/bin/bash

# Neo4j Docker 管理脚本
# 使用方法: ./neo4j-manage.sh [start|stop|restart|logs|status|version]

CONTAINER_NAME="neo4j"
NEO4J_IMAGE="docker.1panel.live/library/neo4j:latest"
NEO4J_VERSION="2026.03.1"

# 颜色定义
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

show_help() {
    echo -e "${YELLOW}Neo4j Docker 管理脚本${NC}"
    echo ""
    echo "用法: $0 [命令]"
    echo ""
    echo "可用命令:"
    echo "  start       启动 Neo4j 容器"
    echo "  stop        停止 Neo4j 容器"
    echo "  restart     重启 Neo4j 容器"
    echo "  status      查看容器状态"
    echo "  logs        查看实时日志"
    echo "  version     查看 Neo4j 版本"
    echo "  info        显示连接信息"
    echo "  reset-pwd   重置密码"
    echo "  help        显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 start"
    echo "  $0 logs"
}

start_neo4j() {
    echo -e "${YELLOW}正在启动 Neo4j...${NC}"
    
    # 检查容器是否存在
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker start ${CONTAINER_NAME}
        echo -e "${GREEN}✓ Neo4j 已启动${NC}"
    else
        # 创建数据目录
        mkdir -p ~/neo4j/{data,logs,import,plugins}
        
        echo -e "${YELLOW}创建新容器...${NC}"
        docker run \
            --name ${CONTAINER_NAME} \
            -p 7474:7474 -p 7687:7687 \
            -d \
            -v $HOME/neo4j/data:/data \
            -v $HOME/neo4j/logs:/logs \
            -v $HOME/neo4j/import:/var/lib/neo4j/import \
            -v $HOME/neo4j/plugins:/plugins \
            --env NEO4J_AUTH=neo4j/password \
            ${NEO4J_IMAGE}
        
        echo -e "${GREEN}✓ Neo4j 容器已创建并启动${NC}"
    fi
    
    echo ""
    echo -e "${GREEN}访问地址: http://localhost:7474${NC}"
    echo -e "${GREEN}用户名: neo4j${NC}"
    echo -e "${GREEN}密码: password${NC}"
}

stop_neo4j() {
    echo -e "${YELLOW}正在停止 Neo4j...${NC}"
    docker stop ${CONTAINER_NAME}
    echo -e "${GREEN}✓ Neo4j 已停止${NC}"
}

restart_neo4j() {
    echo -e "${YELLOW}正在重启 Neo4j...${NC}"
    docker restart ${CONTAINER_NAME}
    echo -e "${GREEN}✓ Neo4j 已重启${NC}"
}

show_status() {
    echo -e "${YELLOW}Neo4j 容器状态:${NC}"
    docker ps -a --filter "name=${CONTAINER_NAME}" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
}

show_logs() {
    echo -e "${YELLOW}Neo4j 实时日志 (Ctrl+C 退出):${NC}"
    docker logs -f ${CONTAINER_NAME}
}

show_version() {
    echo -e "${YELLOW}Neo4j 版本信息:${NC}"
    docker exec ${CONTAINER_NAME} neo4j --version
}

show_info() {
    echo -e "${GREEN}═══════════════════════════════════════${NC}"
    echo -e "${GREEN}  Neo4j 连接信息${NC}"
    echo -e "${GREEN}═══════════════════════════════════════${NC}"
    echo ""
    echo -e "${YELLOW}浏览器访问:${NC} http://localhost:7474"
    echo -e "${YELLOW}Bolt 协议:${NC}    bolt://localhost:7687"
    echo ""
    echo -e "${YELLOW}用户名:${NC} neo4j"
    echo -e "${YELLOW}密码:${NC}   password"
    echo ""
    echo -e "${YELLOW}数据目录:${NC} ~/neo4j/"
    echo -e "  ├── data/      (数据库文件)"
    echo -e "  ├── logs/      (日志文件)"
    echo -e "  ├── import/    (数据导入)"
    echo -e "  └── plugins/   (插件)"
    echo ""
    echo -e "${GREEN}═══════════════════════════════════════${NC}"
}

reset_password() {
    read -sp "请输入新密码: " new_password
    echo ""
    
    docker exec -it ${CONTAINER_NAME} neo4j-admin dbms set-initial-password ${new_password}
    docker restart ${CONTAINER_NAME}
    
    echo -e "${GREEN}✓ 密码已重置${NC}"
    echo -e "${YELLOW}请使用新密码登录${NC}"
}

# 主程序
case "${1}" in
    start)
        start_neo4j
        ;;
    stop)
        stop_neo4j
        ;;
    restart)
        restart_neo4j
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs
        ;;
    version)
        show_version
        ;;
    info)
        show_info
        ;;
    reset-pwd)
        reset_password
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        show_help
        exit 1
        ;;
esac
