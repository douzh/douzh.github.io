# doc-vue-router

-   [官网vue2](https://v2.cn.vuejs.org/)
-   [官网vue3](https://cn.vuejs.org/)
-   [Vue Router](https://router.vuejs.org/zh/)

## 路由 router

``` html
<script src="https://unpkg.com/vue@3"></script>
<script src="https://unpkg.com/vue-router@4"></script>

<div id="app">
  <h1>Hello App!</h1>
  <p>
    <!--使用 router-link 组件进行导航 -->
    <!--通过传递 `to` 来指定链接 -->
    <!--`<router-link>` 将呈现一个带有正确 `href` 属性的 `<a>` 标签-->
    <router-link to="/">Go to Home</router-link>
    <router-link to="/about">Go to About</router-link>
  </p>
  <!-- 路由出口 -->
  <!-- 路由匹配到的组件将渲染在这里 -->
  <router-view></router-view>
</div>
```

``` javascript
// 1. 定义路由组件.
// 也可以从其他文件导入
const Home = { template: '<div>Home</div>' }
const About = { template: '<div>About</div>' }

// 2. 定义一些路由
// 每个路由都需要映射到一个组件。
// 我们后面再讨论嵌套路由。
const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About },
]

// 3. 创建路由实例并传递 `routes` 配置
// 你可以在这里输入更多的配置，但我们在这里
// 暂时保持简单
const router = VueRouter.createRouter({
  // 4. 内部提供了 history 模式的实现。为了简单起见，我们在这里使用 hash 模式。
  history: VueRouter.createWebHashHistory(),
  routes, // `routes: routes` 的缩写
})

// 5. 创建并挂载根实例
const app = Vue.createApp({})
//确保 _use_ 路由实例使
//整个应用支持路由。
app.use(router)

app.mount('#app')

// 现在，应用已经启动了！
```

通过调用 app.use(router)，我们会触发第一次导航且可以在任意组件中以
this.\$router 的形式访问它，并且以 this.\$route 的形式访问当前路由

要在 setup 函数中访问路由，请调用 useRouter 或 useRoute 函数。

在整个文档中，我们会经常使用 router 实例，请记住，this.\$router
与直接使用通过 createRouter 创建的 router 实例完全相同。我们使用
this.\$router 的原因是，我们不想在每个需要操作路由的组件中都导入路由。

很多时候，我们需要将给定匹配模式的路由映射到同一个组件。例如，我们可能有一个
User 组件，它应该对所有用户进行渲染，但用户 ID 不同。在 Vue Router
中，我们可以在路径中使用一个动态字段来实现，我们称之为 路径参数 ：

``` javascript
const User = {
  template: '<div>User</div>',
}

// 这些都会传递给 `createRouter`
const routes = [
  // 动态字段以冒号开始
  { path: '/users/:id', component: User },
]
```

现在像 /users/johnny 和 /users/jolyne 这样的 URL 都会映射到同一个路由。

路径参数 用冒号 : 表示。当一个路由被匹配时，它的 params
的值将在每个组件中以 this.\$route.params
的形式暴露出来。因此，我们可以通过更新 User 的模板来呈现当前的用户 ID：

``` javascript
const User = {
  template: '<div>User {{ $route.params.id }}</div>',
}
```
