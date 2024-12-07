
# elasticsearch 

<https://www.elastic.co/cn/>

<https://www.elastic.co/guide/index.html>

[Elasticsearch: 权威指南](https://www.elastic.co/guide/cn/elasticsearch/guide/current/index.html)

[Kibana 用户手册](https://www.elastic.co/guide/cn/kibana/current/index.html)

[Elasticsearch 教程](https://www.cainiaojc.com/elasticsearch/elasticsearch-sql-access.html)

## 安装

### docker安装

<http://localhost:9200/>

    docker pull elasticsearch:6.8.13
    docker pull elasticsearch:7.9.3

    docker run -d --name es793 -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" elasticsearch:7.9.3
    docker run -d --name es6813 -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" elasticsearch:6.8.13

## restful

### 文档元数据

一个文档不仅仅包含它的数据 ，也包含 元数据，有关文档的信息。

三个必须的元数据元素如下：

-   ~index~ 文档在哪存放
-   ~type~ 文档表示的对象类别
-   ~id~ 文档唯一标识

### 索引

索引设置由setting和mapping组成

``` example
get member

get member/_mapping

get member/_settings
```

索引列表

``` example
GET /_cat/indices
```

### 基础命令

1.  创建或更新

    类型通过返回create值判断

    ``` example
    PUT /{index}/{type}/{id}
    {
      "field": "value",
      ...
    }
    ```

    不带ID创建

    ``` example
    POST /website/blog/
    { ... }
    ```

    带ID创建

    ``` example
    PUT /website/blog/123?op_type=create
    { ... }

    PUT /website/blog/123/_create
    { ... }
    ```

2.  更新文档

    version会变

    ``` example
    PUT /website/blog/123
    {
      "title": "My first blog entry",
      "text":  "I am starting to get the hang of this...",
      "date":  "2014/01/02"
    }
    ```

3.  取回文档

    ``` example
    GET /website/blog/123?pretty

    GET /website/blog/123?_source=title,text

    GET /website/blog/123/_source
    ```

4.  文档是否存在

    ``` example
    HEAD /website/blog/123
    ```

5.  删除文档

    ``` example
    DELETE /website/blog/123
    ```

    ``` example
    GET store_main_info/_doc/123456

    delete store_main_info/_doc/123456
    ```

6.  乐观锁控制

    ``` example
    PUT /website/blog/1?version=1 
    {
      "title": "My first blog entry",
      "text":  "Starting to get the hang of this..."
    }
    ```

    使用外部版本号

    外部版本号的处理方式和我们之前讨论的内部版本号的处理方式有些不同，Elasticsearch 不是检查当前 `_version` 和请求中指定的版本号是否相同，而是检查当前 `_version` 是否 *小于* 指定的版本号。

    如果请求成功，外部的版本号作为文档的新 `_version` 进行存储。

    ``` example
    PUT /website/blog/2?version=5&version_type=external
    {
      "title": "My first external blog entry",
      "text":  "Starting to get the hang of this..."
    }
    ```

7.  部分更新

    `update` 请求最简单的一种形式是接收文档的一部分作为 `doc` 的参数，它只是与现有的文档进行合并。对象被合并到一起，覆盖现有的字段，增加新的字段。

    ``` example
    POST /website/blog/1/_update
    {
       "doc" : {
          "tags" : [ "testing" ],
          "views": 0
       }
    }
    ```

### 使用脚本

脚本可以在 `update` API中用来改变 `_source` 的字段内容，它在更新脚本中称为 `ctx._source` 。

``` example
POST /website/blog/1/_update
{
   "script" : "ctx._source.views+=1"
}
```

### SQL的使用

查询结果类似命令行终端。

``` example
GET /_xpack/sql?format=txt
{
  "query":"select count(*) from order_main"
}

GET /_xpack/sql?format=txt
{
  "query":"select * from order_main where uuid='ca5144c07246439d87c8fb08c0dc5be0'"
}
```

### SQL翻译为DSL

``` example
GET /_xpack/sql/translate
{
  "query":"select * from order_main where orderState=2 and createOpeTime>'2020-09-01 00:00:00' and createOpeTime<'2020-11-01 00:00:00'"

}
```

主要为query部分，这可以直接用于~search的查询~。

``` example
"query" : {
    "bool" : {
      "must" : [
        {
          "term" : {
            "orderState" : {
              "value" : 2,
              "boost" : 1.0
            }
          }
        },
        {
          "range" : {
            "createOpeTime" : {
              "from" : "2020-09-01 00:00:00",
              "to" : "2020-11-01 00:00:00",
              "include_lower" : false,
              "include_upper" : false,
              "boost" : 1.0
            }
          }
        }
      ],
      "adjust_pure_negative" : true,
      "boost" : 1.0
    }
  }
```

### ~search~

返回集群中所有索引下的所有文档：

``` example
GET /_search
GET /_search?size=5
GET /_search?size=5&from=5
GET /_search?size=5&from=10
```

轻量搜索

``` example
GET /_all/tweet/_search?q=tweet:elasticsearch
```

``` example
GET order_main/_search
{
  "query" : {
    ...
  }
}
```

-   /~search~ 在所有的索引中搜索所有的类型
-   /gb/~search~ 在 `gb` 索引中搜索所有的类型
-   /gb,us/~search~ 在 `gb` 和 `us` 索引中搜索所有的文档
-   /g\,u\/~search~ 在任何以 `g` 或者 `u` 开头的索引中搜索所有的类型
-   /gb/user/~search~ 在 `gb` 索引中搜索 `user` 类型
-   /gb,us/user,tweet/~search~ 在 `gb` 和 `us` 索引中搜索 `user` 和 `tweet` 类型
-   /~all~/user,tweet/~search~ 在所有的索引中搜索 `user` 和 `tweet` 类型

### 整体更新或新建

一般是先GET出来，修改后再PUT。

``` example
PUT store_main_info/_doc/123456
{
    ....
}
```

### ~updatebyquery~ 按查询条件更新指定字段

``` example
GET order_main/_update_by_query
{
  "query" : {
    "terms" : {
      "uuid" : [
        "ca5144c07246439d87c8fb08c0dc5be0",
        "fc15dc9ce04948e49ea117d44d3cf22c"
      ],
      "boost" : 1.0
    }
  },
    "script": {
        "source": "ctx._source['orderState'] =4"
    }
}
```

### ~deletebyquery~ 按查询条件删除文档

``` example
POST testdoct/_delete_by_query
{
    "query": {
      "range": {
         "id": {
            "from": 0,
            "to": null,
            "include_lower": false,
            "include_upper": false,
            "boost": 1
         }
      }
   }
}
```

### curl

``` example
curl --header "Content-Type:application/json" -XPUT "http://localhost:9200/movies/movie/1" -d'
{
    "title": "The Godfather",
    "director": "Francis Ford Coppola",
    "year": 1972
}'
```

``` example
curl -XGET "http://localhost:9200/movies/movie/1"
```

``` example
curl --header "Content-Type:application/json" -XPUT "http://localhost:9200/movies/movie/1" -d'
{
    "title": "The Godfather",
    "director": "Francis Ford Coppola",
    "year": 1972,
    "genres": ["Crime", "Drama"]
}'
```

``` example
curl -XDELETE "http://localhost:9200/movies/movie/1"
```
