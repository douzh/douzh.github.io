[toc]

RestTemplate是Spring提供的用于访问Rest服务的客户端，RestTemplate提供了多种便捷访问远程Http服务的方法，能够大大提高客户端的编写效率。

#### RestTemplate

构造函数有三个：

1. 空构建函数添加了默认的HttpMessageConverter
2. requestFactory在空构建函数基础上添加了请求构造器
3. Converter构建函数一般用不到，可以参考转器相关文章

[HttpMessageConverter是这样转换数据的](https://www.jianshu.com/p/3e1de3d02dd8)

```
public RestTemplate() 

public RestTemplate(ClientHttpRequestFactory requestFactory)

public RestTemplate(List<HttpMessageConverter<?>> messageConverters)
```

#### ClientHttpRequestFactory

RestTemplate对象在底层通过使用java.net包下的实现创建HTTP请求，可以通过使用ClientHttpRequestFactory指定不同的HTTP请求方式。

默认Factory为SimpleClientHttpRequestFactory。

在顶级父类里设置时就被赋值。

```
public abstract class HttpAccessor {

	private ClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
```

ClientHttpRequestFactory接口提供了四种实现方式：

1.SimpleClientHttpRequestFactory
2.HttpComponentsClientHttpRequestFactory
3.Netty4ClientHttpRequestFactory
4.OkHttp3ClientHttpRequestFactory

所有请Factory都实现了两个接口，用于创建同步请求和异步请求：

1. ClientHttpRequestFactory
2. AsyncClientHttpRequestFactory

#### SimpleClientHttpRequestFactory

使用J2SE提供的java.net.HttpURLConnection创建底层的Http请求连接。

支持超时配制：

```
private int connectTimeout = -1;

private int readTimeout = -1;
```

#### HttpComponentsClientHttpRequestFactory

底层使用HttpClient访问远程的Http服务，使用HttpClient可以配置连接池和证书等信息。

#### 配制

springboot默认没有创建RestTemplate的实例bean，所以需要自己创建。

RestTemplateAutoConfiguration是用来创建RestTemplateBuilder的。

RestTemplateBuilder.build()每次都可以创建新的RestTemplate对象，不会从IOC中取。

HttpComponentsClientHttpRequestFactory创建方法。

```java

import com.alibaba.fastjson.support.spring.FastJsonHttpMessageConverter;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Rest Template config
 *
 */
@Configuration
@ConditionalOnClass(value = {RestTemplate.class, HttpClient.class})
public class RestTemplateConfig {

    @Value("${remote.maxTotalConnect:0}")
    private int maxTotalConnect; //连接池的最大连接数默认为0
    @Value("${remote.maxConnectPerRoute:200}")
    private int maxConnectPerRoute; //单个主机的最大连接数
    @Value("${remote.connectTimeout:15000}")
    private int connectTimeout; //连接超时默认6s
    @Value("${remote.readTimeout:60000}")
    private int readTimeout; //读取超时默认30s

    private ClientHttpRequestFactory createFactory() {
        if (this.maxTotalConnect <= 0) {
            SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
            factory.setConnectTimeout(this.connectTimeout);
            factory.setReadTimeout(this.readTimeout);
            return factory;
        }
        HttpClient httpClient = HttpClientBuilder.create().setMaxConnTotal(this.maxTotalConnect)
                .setMaxConnPerRoute(this.maxConnectPerRoute).build();
        HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(
                httpClient);
        factory.setConnectTimeout(this.connectTimeout);
        factory.setReadTimeout(this.readTimeout);
        return factory;
    }

    @Bean
    @ConditionalOnMissingBean(RestTemplate.class)
    public RestTemplate getRestTemplate() {
        RestTemplate restTemplate = new RestTemplate(this.createFactory());
        List<HttpMessageConverter<?>> converterList = restTemplate.getMessageConverters();

        HttpMessageConverter<?> converterTarget = null;
        for (HttpMessageConverter<?> item : converterList) {
            if (StringHttpMessageConverter.class == item.getClass()) {
                converterTarget = item;
                break;
            }
        }
        if (null != converterTarget) {
            converterList.remove(converterTarget);
        }
        converterList.add(1, new StringHttpMessageConverter(StandardCharsets.UTF_8));

        converterList.add(new FastJsonHttpMessageConverter());
        return restTemplate;
    }

}
```