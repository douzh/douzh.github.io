# dn-vue 

-   [官网vue2](https://v2.cn.vuejs.org/)
-   [官网vue3](https://cn.vuejs.org/)
    -   [深度指南](https://cn.vuejs.org/guide/introduction.html)
    -   [互动教程](https://cn.vuejs.org/tutorial/#step-1)
    -   [示例](https://cn.vuejs.org/examples/#hello-world)
-   [vue cli](https://cli.vuejs.org/zh/)
    停止维护，配置文件vue.config.js，vue3使用vite
    -   [vue cli github](https://github.com/vuejs/vue-cli)
        有源码和文档源件
-   [前端工具链 vite](https://cn.vitejs.dev/) 配置文件vite.config.js
    -   [vite github ](https://github.com/vitejs/vite)有源码和文档源件
-   [Vue Router](https://router.vuejs.org/zh/)
-   [doc-vue](id:doc-cst-js-vue)
-   [doc-nodejs](id:doc-cst-js-node)
    -   [nvm](id:doc-cst-js-node-nvm) nodejs多版本管理工具
    -   [NPM 使用介绍](id:doc-cst-js-node-npm)
        -   [NPM create](id:doc-cst-js-node-create)

## Vue的使用

vue可以直接在现在项目引入js使用期特性，新项目可以构建单页面应用，这需要使用构建工具链。

路由使用vue router，这是vue默认集成的官方路由。

## 单页面应用配置

基于node，使用npm管理项目，script定义命令

## 新建单页面应用

使用脚手架创建项目，按提示选择参数。命令含义参考"[NPM
create](id:doc-cst-js-node-create)"

    npm create vue@latest

执行命令运行项目

    npm install
    npm run dev

### 项目结构

    │  index.html
    │  jsconfig.json
    │  package-lock.json
    │  package.json
    │  README.md
    │  vite.config.js
    │
    ├─public
    │      favicon.ico
    │
    └─src
        │  App.vue
        │  main.js
        │
        ├─assets
        │      base.css
        │      logo.svg
        │      main.css
        │
        ├─components
        │  │  HelloWorld.vue
        │  │  TheWelcome.vue
        │  │  WelcomeItem.vue
        │  │
        │  └─icons
        │          IconCommunity.vue
        │          IconDocumentation.vue
        │          IconEcosystem.vue
        │          IconSupport.vue
        │          IconTooling.vue
        │
        ├─router
        │      index.js
        │
        └─views
                AboutView.vue
                HomeView.vue

1.  package.js

    使用vite构建，使用vue-router路由

    ``` javascript
    {
      "name": "vue-demo-01",
      "version": "0.0.0",
      "private": true,
      "type": "module",
      "scripts": {
        "dev": "vite",
        "build": "vite build",
        "preview": "vite preview"
      },
      "dependencies": {
        "vue": "^3.4.15",
        "vue-router": "^4.2.5"
      },
      "devDependencies": {
        "@vitejs/plugin-vue": "^5.0.3",
        "vite": "^5.0.11"
      }
    }
    ```

2.  main.js

    ``` javascript
    import './assets/main.css'

    import { createApp } from 'vue'
    import App from './App.vue'
    import router from './router'

    const app = createApp(App)

    app.use(router)

    app.mount('#app')
    ```

3.  router

    路由文件：src/router/index.js

    ``` javascript
    import { createRouter, createWebHistory } from 'vue-router'
    import HomeView from '../views/HomeView.vue'

    const router = createRouter({
      history: createWebHistory(import.meta.env.BASE_URL),
      routes: [
        {
          path: '/',
          name: 'home',
          component: HomeView
        },
        {
          path: '/about',
          name: 'about',
          // route level code-splitting
          // this generates a separate chunk (About.[hash].js) for this route
          // which is lazy-loaded when the route is visited.
          component: () => import('../views/AboutView.vue')
        }
      ]
    })

    export default router

    ```

    ``` javascript
    import { createRouter, createWebHistory } from 'vue-router'
    import HomeView from '../views/HomeView.vue'

    const router = createRouter({
      history: createWebHistory(import.meta.env.BASE_URL),
      routes: [
        {
          path: '/',
          name: 'home',
          component: HomeView
        },
        {
          path: '/about',
          name: 'about',
          // route level code-splitting
          // this generates a separate chunk (About.[hash].js) for this route
          // which is lazy-loaded when the route is visited.
          component: () => import('../views/AboutView.vue')
        }
      ]
    })

    export default router
    ```

4.  App.vue

    根路由"/"默认RouterView会加载HomeView

    ``` html
    <script setup>
    import { RouterLink, RouterView } from 'vue-router'
    import HelloWorld from './components/HelloWorld.vue'
    </script>

    <template>
      <header>
        <img alt="Vue logo" class="logo" src="@/assets/logo.svg" width="125" height="125" />

        <div class="wrapper">
          <HelloWorld msg="You did it!" />

          <nav>
            <RouterLink to="/">Home</RouterLink>
            <RouterLink to="/about">About</RouterLink>
          </nav>
        </div>
      </header>

      <RouterView />
    </template>
    ```

5.  HomeView

    ``` html
    <script setup>
    import TheWelcome from '../components/TheWelcome.vue'
    </script>

    <template>
      <main>
        <TheWelcome />
      </main>
    </template>
    ```

6.  TheWelcome

    使用WelcomeItem生成数据

    ``` html
    <template>
      <WelcomeItem>
        <template #icon>
          <DocumentationIcon />
        </template>
        <template #heading>Documentation</template>

        Vue’s
        <a href="https://vuejs.org/" target="_blank" rel="noopener">official documentation</a>
        provides you with all information you need to get started.
      </WelcomeItem>
    ......
    ```

7.  WelcomeItem

    有个默认slot和icon heading两个命名slot

    ``` html
    <template>
      <div class="item">
        <i>
          <slot name="icon"></slot>
        </i>
        <div class="details">
          <h3>
            <slot name="heading"></slot>
          </h3>
          <slot></slot>
        </div>
      </div>
    </template>
    ```

## 语法

``` html
<p>Using text interpolation: {{ rawHtml }}</p>
<p>Using v-html directive: <span v-html="rawHtml"></span></p>

<div v-bind:id="dynamicId"></div>
<div :id="dynamicId"></div>
<div :id="`list-${id}`"></div>
<p v-if="seen">Now you see me</p>

<a v-on:click="doSomething"> ... </a>
<a @click="doSomething"> ... </a>

<a v-bind:[attributeName]="url"> ... </a>
<a :[attributeName]="url"> ... </a>

<a v-on:[eventName]="doSomething"> ... </a>
<a @[eventName]="doSomething">
```
