## maven

### 版本查询

<https://mvnrepository.com>

## 接口

### 接口验证

引入依赖

``` xml
<dependency>
  <groupId>org.hibernate.validator</groupId>
  <artifactId>hibernate-validator</artifactId>
   <version>6.2.0.Final</version>
</dependency>
```

在入参添加注解

``` java
@NotBlank(message = "状态不能为空")
@Pattern(regexp = "[Y|N]", message = "任务状态码值错误")
@ApiModelProperty(value="任务状态 S 成功 E 失败", required = true,example="S")
private String status;

@NotBlank(message = "任务描述不能为空")
@Size(max = 30, message = "任务描述超长")
@ApiModelProperty(value="任务描述", required = true,example="成功")
private String descr;
```

在接口上添加@Valid注解

``` java
public R<String> updateTask(@Valid @RequestBody TaskParam param);
```

## mybatis

### 打印sql

在application.yml配置文件中增加MyBatis的configuration配置：

``` yaml
mybatis:
  configuration:
    log-impl: org.apache.ibatis.logging.stdout.StdOutImpl
```

### 分页PageHelper

引入依赖

``` xml
<!-- https://mvnrepository.com/artifact/com.github.pagehelper/pagehelper-spring-boot-starter -->
<dependency>
    <groupId>com.github.pagehelper</groupId>
    <artifactId>pagehelper-spring-boot-starter</artifactId>
    <version>1.2.12</version>
</dependency>
```

配置

    pagehelper:
      helper-dialect: mysql
      reasonable: true
      support-methods-arguments: true
      params: count=countSql

在查询前设置分页

``` java
PageHelper.startPage(pageNum, pageSize);
List<User> users = userMapper.getUsers();
PageInfo<User> PageInfo = new PageInfo<User>(users);
```

注意，如果要关闭count需要设置第三个参数

如查不想返回PageInfo对象，可以自己封装一下

``` java

  public class PageConvertor {

      private PageConvertor() {
      }

      public static <T> PageVo<T> convert(List<T> list) {
          PageInfo<T> pageInfo = new PageInfo<>(list);
          PageVo<T> pageVo = new PageVo<>();
          pageVo.setTotalRecord(pageInfo.getTotal());
          pageVo.setPageNum(pageInfo.getPageNum());
          pageVo.setPageRecordCount(pageInfo.getPageSize());
          pageVo.setTotalPageCount(pageInfo.getPages());
          pageVo.setList(pageInfo.getList());
          return pageVo;
      }

  }

@Data
@ApiModel(value = "PageVo",description = "分页响应实体")
public class PageVo<T> implements Serializable {

        @ApiModelProperty(value="当前页",example="1")
        private int pageNum = 1;

        @ApiModelProperty(value="每页条数",example="20")
        public int pageRecordCount = 20;

        @ApiModelProperty(value="数据")
        private List<T> list;

        @ApiModelProperty(value="数据")
        private long totalRecord;

        @ApiModelProperty(value="总页数", example = "100")
        private int totalPageCount;

        public PageVo() {
        }

        public PageVo(List<T> list) {
                this.list = list;
        }

        public PageVo(int pageNum, int totalRecord, int pageRecordCount, List<T> list) {
                this.pageNum = pageNum;
                this.totalRecord = totalRecord;
                this.pageRecordCount = pageRecordCount;
                this.list = list;
                this.totalPageCount =totalRecord/pageRecordCount+(totalRecord%pageRecordCount>0?1:0);
        }
}

```

## swagger

### excel导出

设置返回类型 produces 的值

值可以取 org.springframework.http.MediaType

excel返回设置 produces = "application/octet-stream"
