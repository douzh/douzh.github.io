# websocket demo

## springboot demo

-   打开多个浏览器界面
-   向指定uid发消息
-   不指定uid向所有用户在消息
-   消息通过console打印

引入依赖

``` xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter</artifactId>
</dependency>
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-websocket</artifactId>
</dependency>
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-thymeleaf</artifactId>
</dependency>
<dependency>
  <groupId>org.projectlombok</groupId>
  <artifactId>lombok</artifactId>
  <version>1.18.28</version>
  <scope>provided</scope>
</dependency>
<dependency>
  <groupId>com.alibaba</groupId>
  <artifactId>fastjson</artifactId>
  <version>2.0.40</version>
</dependency>
```

配置webconfig

``` java
@Configuration
public class WebSocketConfig implements ServletContextInitializer {
    /**
     * 这个bean的注册,用于扫描带有@ServerEndpoint的注解成为websocket,如果你使用外置的tomcat就不需要该配置文件
     */
    @Bean
    public ServerEndpointExporter serverEndpointExporter() {
        return new ServerEndpointExporter();
    }

    @Override
    public void onStartup(ServletContext servletContext) throws ServletException {

    }
}
```

开发功能

``` java
package com.onekbase.demo.websocket.model;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.websocket.*;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * WebSocket的操作类
 * html页面与之关联的接口
 * var reqUrl = "http://localhost:8081/websocket/" + cid;
 * socket = new WebSocket(reqUrl.replace("http", "ws"));
 */
@ServerEndpoint("/websocket/{sid}")
@Component
@Slf4j
public class WebSocketServer {

    /**
     * 静态变量，用来记录当前在线连接数，线程安全的类。
     */
    private static AtomicInteger onlineSessionClientCount = new AtomicInteger(0);

    /**
     * 存放所有在线的客户端
     */
    private static Map<String, Session> onlineSessionClientMap = new ConcurrentHashMap<>();

    /**
     * 连接sid和连接会话
     */
    private String sid;
    private Session session;

    /**
     * 连接建立成功调用的方法。由前端<code>new WebSocket</code>触发
     *
     * @param sid     每次页面建立连接时传入到服务端的id，比如用户id等。可以自定义。
     * @param session 与某个客户端的连接会话，需要通过它来给客户端发送消息
     */
    @OnOpen
    public void onOpen(@PathParam("sid") String sid, Session session) {
        /**
         * session.getId()：当前session会话会自动生成一个id，从0开始累加的。
         */
        log.info("连接建立中 ==> session_id = {}， sid = {}", session.getId(), sid);
        //加入 Map中。将页面的sid和session绑定或者session.getId()与session
        //onlineSessionIdClientMap.put(session.getId(), session);
        onlineSessionClientMap.put(sid, session);

        //在线数加1
        onlineSessionClientCount.incrementAndGet();
        this.sid = sid;
        this.session = session;
        sendToOne(sid, "连接成功");
        log.info("连接建立成功，当前在线数为：{} ==> 开始监听新连接：session_id = {}， sid = {},。", onlineSessionClientCount, session.getId(), sid);
    }

    /**
     * 连接关闭调用的方法。由前端<code>socket.close()</code>触发
     *
     * @param sid
     * @param session
     */
    @OnClose
    public void onClose(@PathParam("sid") String sid, Session session) {
        //onlineSessionIdClientMap.remove(session.getId());
        // 从 Map中移除
        onlineSessionClientMap.remove(sid);

        //在线数减1
        onlineSessionClientCount.decrementAndGet();
        log.info("连接关闭成功，当前在线数为：{} ==> 关闭该连接信息：session_id = {}， sid = {},。", onlineSessionClientCount, session.getId(), sid);
    }

    /**
     * 收到客户端消息后调用的方法。由前端<code>socket.send</code>触发
     * * 当服务端执行toSession.getAsyncRemote().sendText(xxx)后，前端的socket.onmessage得到监听。
     *
     * @param message
     * @param session
     */
    @OnMessage
    public void onMessage(String message, Session session) {
        /**
         * html界面传递来得数据格式，可以自定义.
         * {"sid":"user-1","message":"hello websocket"}
         */
        JSONObject jsonObject = JSON.parseObject(message);
        String toSid = jsonObject.getString("sid");
        String msg = jsonObject.getString("message");
        log.info("服务端收到客户端消息 ==> fromSid = {}, toSid = {}, message = {}", sid, toSid, message);

        /**
         * 模拟约定：如果未指定sid信息，则群发，否则就单独发送
         */
        if (toSid == null || toSid == "" || "".equalsIgnoreCase(toSid)) {
            sendToAll(msg);
        } else {
            sendToOne(toSid, msg);
        }
    }

    /**
     * 发生错误调用的方法
     *
     * @param session
     * @param error
     */
    @OnError
    public void onError(Session session, Throwable error) {
        log.error("WebSocket发生错误，错误信息为：" + error.getMessage());
        error.printStackTrace();
    }

    /**
     * 群发消息
     *
     * @param message 消息
     */
    private void sendToAll(String message) {
        // 遍历在线map集合
        onlineSessionClientMap.forEach((onlineSid, toSession) -> {
            // 排除掉自己
            if (!sid.equalsIgnoreCase(onlineSid)) {
                log.info("服务端给客户端群发消息 ==> sid = {}, toSid = {}, message = {}", sid, onlineSid, message);
                toSession.getAsyncRemote().sendText(message);
            }
        });
    }

    /**
     * 指定发送消息
     *
     * @param toSid
     * @param message
     */
    private void sendToOne(String toSid, String message) {
        // 通过sid查询map中是否存在
        Session toSession = onlineSessionClientMap.get(toSid);
        if (toSession == null) {
            log.error("服务端给客户端发送消息 ==> toSid = {} 不存在, message = {}", toSid, message);
            return;
        }
        // 异步发送
        log.info("服务端给客户端发送消息 ==> toSid = {}, message = {}", toSid, message);
        toSession.getAsyncRemote().sendText(message);
        /*
        // 同步发送
        try {
            toSession.getBasicRemote().sendText(message);
        } catch (IOException e) {
            log.error("发送消息失败，WebSocket IO异常");
            e.printStackTrace();
        }*/
    }

}
```

添加前端跳转

``` java
@Controller
@RequestMapping("/demo")
public class DemoController {
    /**
     * 跳转到websocketDemo.html页面，携带自定义的cid信息。
     * http://localhost:8081/demo/user-1
     */
    @GetMapping("/{cid}")
    public String toWebSocketDemo(@PathVariable String cid, Model model) {
        model.addAttribute("cid", cid);
        return "websocketDemo";
    }
}
```

前端界面在resources/templates/websocketDemo.html

``` html
<!DOCTYPE html>
<html xmlns:th="http://www.thymeleaf.org">
<head>
    <meta charset="UTF-8">
    <title>ws demo</title>
</head>
<body>
传递来的数据值cid: <input type="text" th:value="${cid}" id="cid"/>
<p>【toUserId】： <div><input id="toUserId" name="toUserId" type="text" value="user-1"></div>
<p>【toUserId】： <div><input id="contentText" name="contentText" type="text" value="hello websocket"></div>
<p>【操作】：
<div>
    <button type="button" onclick="sendMessage()">发送消息</button>
</div>
</body>

<script type="text/javascript">
    var socket;
    if (typeof (WebSocket) == "undefined") {
        console.log("您的浏览器不支持WebSocket");
    } else {
        console.log("您的浏览器支持WebSocket");
        //实现化WebSocket对象，指定要连接的服务器地址与端口  建立连接

        var cid = document.getElementById("cid").value;
        console.log("cid-->" + cid);
        var reqUrl = "http://dev-im.com/websocket/" + cid;
        socket = new WebSocket(reqUrl.replace("http", "ws"));
        //打开事件
        socket.onopen = function () {
            console.log("Socket 已打开");
            //socket.send("这是来自客户端的消息" + location.href + new Date());
        };
        //获得消息事件
        socket.onmessage = function (msg) {
            console.log("onmessage--" + msg.data);
            //发现消息进入    开始处理前端触发逻辑
        };
        //关闭事件
        socket.onclose = function () {
            console.log("Socket已关闭");
        };
        //发生了错误事件
        socket.onerror = function () {
            alert("Socket发生了错误");
            //此时可以尝试刷新页面
        }
        //离开页面时，关闭socket
        //jquery1.8中已经被废弃，3.0中已经移除
        // $(window).unload(function(){
        //     socket.close();
        //});
    }

    function sendMessage() {
        if (typeof (WebSocket) == "undefined") {
            console.log("您的浏览器不支持WebSocket");
        } else {
            // console.log("您的浏览器支持WebSocket");
            var toUserId = document.getElementById('toUserId').value;
            var contentText = document.getElementById('contentText').value;
            var msg = '{"sid":"' + toUserId + '","message":"' + contentText + '"}';
            console.log(msg);
            socket.send(msg);
        }
    }

</script>
</html>
```

application.yml中thymeleaf配置
```
spring:
    thymeleaf:
    mode: HTML
    cache: true
    prefix: classpath:/templates/
    encoding: UTF-8
    suffix: .html
    check-template-location: true
    template-resolver-order: 1
```
如果不用thymeleaf，直接打开html界面，可以简单改下html中cid取值方式，从url参数传

``` javascript
console.log("您的浏览器支持WebSocket");
//实现化WebSocket对象，指定要连接的服务器地址与端口  建立连接
let params = new URLSearchParams(window.location.search);
var cid = params.get('qcid');
let input = document.getElementById('cid');
input.value = cid;
console.log("cid-->" + cid);
var reqUrl = "http://dev-im.com/websocket/" + cid;
```

## netty demo

-   前端发送消息到后端：{"unionId":111}
-   后端定时向前端发消息

引入依赖

``` xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter</artifactId>
</dependency>
<dependency>
  <groupId>io.netty</groupId>
  <artifactId>netty-all</artifactId>
  <version>4.1.77.Final</version>
</dependency>
<dependency>
  <groupId>org.projectlombok</groupId>
  <artifactId>lombok</artifactId>
  <version>1.18.28</version>
  <scope>provided</scope>
</dependency>
<dependency>
  <groupId>com.alibaba</groupId>
  <artifactId>fastjson</artifactId>
  <version>2.0.40</version>
</dependency>
```

netty启动类

``` java
@Slf4j
@Component
public class WebsocketInitialization {
    @Resource
    private WebsocketChannelInitializer websocketChannelInitializer;

    @Value("${websocket.port}")
    private Integer port;

    @Async
    public void init() throws InterruptedException {
        EventLoopGroup bossGroup = new NioEventLoopGroup(1);
        EventLoopGroup workerGroup = new NioEventLoopGroup();
        try {
            ServerBootstrap serverBootstrap = new ServerBootstrap();
            serverBootstrap.group(bossGroup, workerGroup);
            serverBootstrap.channel(NioServerSocketChannel.class);
            //可以对入站\出站事件进行日志记录，从而方便我们进行问题排查。
            serverBootstrap.handler(new LoggingHandler(LogLevel.INFO));
            //设置自定义的通道初始化器，用于入站操作
            serverBootstrap.childHandler(websocketChannelInitializer);
            ChannelFuture channelFuture = serverBootstrap.bind(port).sync();
            channelFuture.channel().closeFuture().sync();
        } finally {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }
}

@Component
public class WebsocketChannelInitializer extends ChannelInitializer<SocketChannel> {

    @Autowired
    private WebSocketHandler webSocketHandler;

    @Value("${websocket.url}")
    private String websocketUrl;

    @Override
    protected void initChannel(SocketChannel socketChannel) throws Exception {

        //获取pipeline通道
        ChannelPipeline pipeline = socketChannel.pipeline();
        //因为基于http协议，使用http的编码和解码器
        pipeline.addLast(new HttpServerCodec());
        //是以块方式写，添加ChunkedWriteHandler处理器
        pipeline.addLast(new ChunkedWriteHandler());
        /*
          说明
          1\. http数据在传输过程中是分段, HttpObjectAggregator ，就是可以将多个段聚合
          2\. 这就就是为什么，当浏览器发送大量数据时，就会发出多次http请求
        */
        pipeline.addLast(new HttpObjectAggregator(8192));
        /* 说明
           1\. 对应websocket ，它的数据是以 帧(frame) 形式传递
           2\. 可以看到WebSocketFrame 下面有六个子类
           3\. 浏览器请求时 ws://localhost:7000/msg 表示请求的uri
           4\. WebSocketServerProtocolHandler 核心功能是将 http协议升级为 ws协议 , 保持长连接
           5\. 是通过一个 状态码 101
        */
        pipeline.addLast(new WebSocketServerProtocolHandler(websocketUrl));
        //自定义的handler ，处理业务逻辑
        pipeline.addLast(webSocketHandler);
    }
}

@Slf4j
@Component
public class WebsocketApplication {
    @Resource
    private WebsocketInitialization websocketInitialization;
    @PostConstruct
    public void start() {
        try {
            log.info(Thread.currentThread().getName() + ":websocket启动中......");
            websocketInitialization.init();
            log.info(Thread.currentThread().getName() + ":websocket启动成功！！！");
        } catch (Exception e) {
            log.error("websocket发生错误：",e);
        }
    }
}

@SpringBootApplication
public class WebSocketNettyApp {
    public static void main(String[] args) {
        ConfigurableApplicationContext cxt = SpringApplication.run(WebSocketNettyApp.class, args);
    }
}

```

业务处理类

``` java
@Data
public class MessageRequest implements Serializable {

    private Long unionId;

    private Integer current = 1;

    private Integer size = 10;
}


@Slf4j
@Component
@ChannelHandler.Sharable//保证处理器，在整个生命周期中就是以单例的形式存在，方便统计客户端的在线数量
    public class WebSocketHandler extends SimpleChannelInboundHandler<TextWebSocketFrame> {

    //通道map，存储channel，用于群发消息，以及统计客户端的在线数量，解决问题问题三，如果是集群环境使用redis的hash数据类型存储即可
    private static Map<String, Channel> channelMap = new ConcurrentHashMap<>();
    //任务map，存储future，用于停止队列任务
    private static Map<String, Future> futureMap = new ConcurrentHashMap<>();
    //存储channel的id和用户主键的映射，客户端保证用户主键传入的是唯一值，解决问题四，如果是集群中需要换成redis的hash数据类型存储即可
    private static Map<String, Long> clientMap = new ConcurrentHashMap<>();

    /**
     * 客户端发送给服务端的消息
     *
     * @param ctx
     * @param msg
     * @throws Exception
     */
    @Override
    protected void channelRead0(ChannelHandlerContext ctx, TextWebSocketFrame msg) throws Exception {

        try {
            //接受客户端发送的消息
            MessageRequest messageRequest = JSON.parseObject(msg.text(), MessageRequest.class);

            //每个channel都有id，asLongText是全局channel唯一id
            String key = ctx.channel().id().asLongText();
            //存储channel的id和用户的主键
            clientMap.put(key, messageRequest.getUnionId());
            log.info("接受客户端的消息......" + ctx.channel().remoteAddress() + "-参数[" + messageRequest.getUnionId() + "]");

            if (!channelMap.containsKey(key)) {
                //使用channel中的任务队列，做周期循环推送客户端消息，解决问题二和问题五
                Future future = ctx.channel().eventLoop().scheduleAtFixedRate(new WebsocketRunnable(ctx, messageRequest), 0, 10, TimeUnit.SECONDS);
                //存储客户端和服务的通信的Chanel
                channelMap.put(key, ctx.channel());
                //存储每个channel中的future，保证每个channel中有一个定时任务在执行
                futureMap.put(key, future);
            } else {
                //每次客户端和服务的主动通信，和服务端周期向客户端推送消息互不影响 解决问题一
                ctx.channel().writeAndFlush(new TextWebSocketFrame(Thread.currentThread().getName() + "服务器时间" + LocalDateTime.now() + "wdy"));
            }

        } catch (Exception e) {

            log.error("websocket服务器推送消息发生错误：", e);

        }
    }

    /**
     * 客户端连接时候的操作
     *
     * @param ctx
     * @throws Exception
     */
    @Override
    public void handlerAdded(ChannelHandlerContext ctx) throws Exception {
        log.info("一个客户端连接......" + ctx.channel().remoteAddress() + Thread.currentThread().getName());
    }

    /**
     * 客户端掉线时的操作
     *
     * @param ctx
     * @throws Exception
     */
    @Override
    public void handlerRemoved(ChannelHandlerContext ctx) throws Exception {

        String key = ctx.channel().id().asLongText();
        //移除通信过的channel
        channelMap.remove(key);
        //移除和用户绑定的channel
        clientMap.remove(key);
        //关闭掉线客户端的future
        Optional.ofNullable(futureMap.get(key)).ifPresent(future -> {
                future.cancel(true);
                futureMap.remove(key);
            });
        log.info("一个客户端移除......" + ctx.channel().remoteAddress());
        ctx.close(); //关闭连接
    }

    /**
     * 发生异常时执行的操作
     *
     * @param ctx
     * @param cause
     * @throws Exception
     */
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        String key = ctx.channel().id().asLongText();
        //移除通信过的channel
        channelMap.remove(key);
        //移除和用户绑定的channel
        clientMap.remove(key);
        //移除定时任务
        Optional.ofNullable(futureMap.get(key)).ifPresent(future -> {
                future.cancel(true);
                futureMap.remove(key);
            });
        //关闭长连接
        ctx.close();
        log.info("异常发生 " + cause.getMessage());
    }

    public static Map<String, Channel> getChannelMap() {
        return channelMap;
    }

    public static Map<String, Future> getFutureMap() {
        return futureMap;
    }

    public static Map<String, Long> getClientMap() {
        return clientMap;
    }
}


@Slf4j
public class WebsocketRunnable implements Runnable {

    private ChannelHandlerContext channelHandlerContext;

    private MessageRequest messageRequest;

    public WebsocketRunnable(ChannelHandlerContext channelHandlerContext,MessageRequest messageRequest) {
        this.channelHandlerContext = channelHandlerContext;
        this.messageRequest = messageRequest;
    }

    @Override
    public void run() {
        try {
            System.out.println(Thread.currentThread().getName()+"--"+LocalDateTime.now().toString());
            channelHandlerContext.channel().writeAndFlush(new TextWebSocketFrame("from netty:"+LocalDateTime.now().toString()));
        } catch (Exception e) {
            log.error("websocket服务器推送消息发生错误：",e);
        }
    }
}
```

前端界面

``` html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Title</title>
</head>
<body>
<script>
    var socket;
    //判断当前浏览器是否支持websocket
    if(window.WebSocket) {
        socket = new WebSocket("ws://localhost:8999/msg");
        //相当于channelReado, ev 收到服务器端回送的消息
        socket.onmessage = function (ev) {
            var rt = document.getElementById("responseText");
            rt.value = rt.value + "\n" + ev.data;
        }

        //相当于连接开启(感知到连接开启)
        socket.onopen = function (ev) {
            var rt = document.getElementById("responseText");
            rt.value = "连接开启了.."
        }

        //相当于连接关闭(感知到连接关闭)
        socket.onclose = function (ev) {

            var rt = document.getElementById("responseText");
            rt.value = rt.value + "\n" + "连接关闭了.."
        }
    } else {
        alert("当前浏览器不支持websocket")
    }

    //发送消息到服务器
    function send(websocketMessage) {
        if(!window.socket) { //先判断socket是否创建好
            return;
        }
        if(socket.readyState == WebSocket.OPEN) {
            //通过socket 发送消息
            socket.send(websocketMessage)
        } else {
            alert("连接没有开启");
        }
    }
</script>
<form onsubmit="return false">
    <textarea name="websocketMessage" style="height: 300px; width: 300px"></textarea>
    <input type="button" value="发生消息" onclick="send(this.form.websocketMessage.value)">
    <textarea id="responseText" style="height: 300px; width: 300px"></textarea>
    <input type="button" value="清空内容" onclick="document.getElementById('responseText').value=''">
</form>
</body>
</html>

```
