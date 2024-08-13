# 开源IM资料收集

<https://gitee.com/explore/im?_from=gitee_search>

## 相关资料

网站：http://www.52im.net/

-   [新手入门一篇就够：从零开发移动端IM](http://www.52im.net/thread-464-1-1.html)

## wildfirechat java 国内

<https://wildfirechat.cn/>

github: <https://github.com/wildfirechat/server> 服务端: java 客户端:
Electron(windows/mac/linux),android,ios,web

简介:

非常活跃的开源IM解决方案，社区版免费，专业版收费（号称支持百万并发和集群部署）。
优点：客户端支持全面，功能成熟完整BUG少。和微信的实现非常接近，底层也是使用的微信跨平台通信库Mars，在弱网（2.5G，E信号）下理论上拥有和微信一样优秀的表现。
缺点：社区版服务端性能不好（嵌入式数据库H2、不支持集群等），只适合学习和试用。

## MobileIMSDK java 国内

一个原创多端IM通信层框架，轻量级、高度提炼，历经8年、久经考验。可能是市面上唯一同时支持UDP+TCP+WebSocket三种协议的同类开源框架，支持
iOS、Android、Java、H5、小程序、Uniapp，服务端基于Netty。

语言：java github: <https://github.com/JackJiang2011/MobileIMSDK> 简介:

著名http://www.52im.net/站长作品，有丰富的文档和学习资源。并且也包含免费版和收费精编注释
只不过要花钱购买源码，可能更适合企业吧。

## cim java 国内

<https://farsunset.com/>

<https://gitee.com/farsunset/cim>

侣信

侣信主要是面向企业用户的产品，所有用户共享企业通讯录和组织架构，适合开发企业、组织内部项目,客户端不可注册，不可添加好友

和信

和信类似钉钉通讯录结构，支持注册添加好友，同时也支持共享企业通讯录和组织架构

## J-IM java 国内

<https://gitee.com/xchao/j-im>

## V-IM java 国内

<https://gitee.com/alyouge/V-IM>

## TeamTalk c++ 国内

github: <https://github.com/meili/TeamTalk> 服务端: c++ 客户端:
windows,android,ios,mac,web管理后台

简介:

TeamTalk是一款蘑菇街开源的企业内部即时通讯软件，目前支持pc、安卓、IOS、Mac和web多个终端。
该项目适合学习和入门，客户端BUG很多，服务端比较稳定。

我维护的分支（有详细的服务端编译部署文档）：
<https://github.com/xmcy0011/TeamTalk>

## OpenIM go 国内

<https://www.openim.online/zh>

github: <https://github.com/OpenIMSDK/Open-IM-Server> 服务端：go
客户端：Flutter，Android，IOS，Uniapp，Electron……

简介：

作者来自于微信团队，所以天然的使用了收件箱机制和微服务架构，值得学习研究，商用有待考量，目前还在不断更新中

## fastim java

<https://github.com/zhangyaoo/fastim>

基于Netty高可用分布式即时通讯系统，支持长连接网关管理、单聊、群聊、离线消息、消息推送消息、消息已读未读、消息未读数、红包、消息漫游等功能，支持集群部署的分布式架构。
