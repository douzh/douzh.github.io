---
title: Python cookie管理
date: 2024-03-15
type: 计算机 
tags: 计算机/编程语言/python
---

# cookiejar

```python
from urllib import request
from http import cookiejar

def getCookie():
    url = "https://xueqiu.com/"
    #声明一个CookieJar对象实例来保存cookie
    cookie = cookiejar.CookieJar()
    #利用urllib.request库的HTTPCookieProcessor对象来创建cookie处理器,也就CookieHandler
    handler = request.HTTPCookieProcessor(cookie)
    #通过CookieHandler创建opener
    opener = request.build_opener(handler)
    req = request.Request(url)
    req.add_header('user-agent',"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36")
    #此处的open方法打开网页
    response = opener.open(req)
    #打印cookie信息
    for item in cookie:
        print('Name = %s' % item.name)
        print('Value = %s' % item.value)


if __name__ == '__main__':

    getCookie()

```
## requests

```python
import requests

def getCookie():
    url = "https://xueqiu.com/"
    Hostreferer = {
        #'Host':'***',
        'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36'
    }
    #urllib或requests在打开https站点是会验证证书。 简单的处理办法是在get方法中加入verify参数，并设为False
    html = requests.get(url, headers=Hostreferer, verify=False)
    for item in html.cookies:
        print('Name = %s' % item.name)
        print('Value = %s' % item.value)

if __name__ == '__main__':
    getCookie()

```

