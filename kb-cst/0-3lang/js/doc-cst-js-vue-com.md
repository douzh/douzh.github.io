# doc-vue-com 

-   [官网vue2](https://v2.cn.vuejs.org/)
-   [官网vue3](https://cn.vuejs.org/)

## 组件注册

一个 Vue 组件在使用前需要先被“注册”，这样 Vue
才能在渲染模板时找到其对应的实现。组件注册有两种方式：全局注册和局部注册。

### 全局注册

我们可以使用 Vue 应用实例的 .component() 方法，让组件在当前 Vue
应用中全局可用。

``` javascript
import { createApp } from 'vue'

const app = createApp({})

app.component(
  // 注册的名字
  'MyComponent',
  // 组件的实现
  {
    /* ... */
  }
)
```

如果使用单文件组件，你可以注册被导入的 .vue 文件：

    import MyComponent from './App.vue'

    app.component('MyComponent', MyComponent)

.component() 方法可以被链式调用：

    app
      .component('ComponentA', ComponentA)
      .component('ComponentB', ComponentB)
      .component('ComponentC', ComponentC)

全局注册的组件可以在此应用的任意组件的模板中使用：

    <!-- 这在当前应用的任意组件中都可用 -->
    <ComponentA/>
    <ComponentB/>
    <ComponentC/>

所有的子组件也可以使用全局注册的组件，这意味着这三个组件也都可以在彼此内部使用。

### 局部注册

全局注册虽然很方便，但有以下几个问题：

-   全局注册，但并没有被使用的组件无法在生产打包时被自动移除
    (也叫“tree-shaking”)。如果你全局注册了一个组件，即使它并没有被实际使用，它仍然会出现在打包后的
    JS 文件中。

-   全局注册在大型项目中使项目的依赖关系变得不那么明确。在父组件中使用子组件时，不太容易定位子组件的实现。和使用过多的全局变量一样，这可能会影响应用长期的可维护性。

相比之下，局部注册的组件需要在使用它的父组件中显式导入，并且只能在该父组件中使用。它的优点是使组件之间的依赖关系更加明确，并且对
tree-shaking 更加友好。

局部注册需要使用 components 选项：

    <script>
    import ComponentA from './ComponentA.vue'

    export default {
      components: {
        ComponentA
      }
    }
    </script>

    <template>
      <ComponentA />
    </template>

对于每个 components 对象里的属性，它们的 key
名就是注册的组件名，而值就是相应组件的实现。上面的例子中使用的是 ES2015
的缩写语法，等价于：

    export default {
      components: {
        ComponentA: ComponentA
      }
      // ...
    }

请注意：局部注册的组件在后代组件中并不可用。在这个例子中，ComponentA
注册后仅在当前组件可用，而在任何的子组件或更深层的子组件中都不可用。

### 组件名格式

在整个指引中，我们都使用 PascalCase 作为组件名的注册格式，这是因为：

-   PascalCase 是合法的 JavaScript 标识符。这使得在 JavaScript
    中导入和注册组件都很容易，同时 IDE 也能提供较好的自动补全。

-   \<PascalCase /\> 在模板中更明显地表明了这是一个 Vue 组件，而不是原生
    HTML 元素。同时也能够将 Vue 组件和自定义元素 (web components)
    区分开来。

在单文件组件和内联字符串模板中，我们都推荐这样做。但是，PascalCase
的标签名在 DOM 内模板中是不可用的，详情参见 DOM 内模板解析注意事项。

为了方便，Vue 支持将模板中使用 kebab-case 的标签解析为使用 PascalCase
注册的组件。这意味着一个以 MyComponent 为名注册的组件，在模板中可以通过
\<MyComponent\> 或 \<my-component\> 引用。这让我们能够使用同样的
JavaScript 组件注册代码来配合不同来源的模板。

## Props

### Props 声明

一个组件需要显式声明它所接受的 props，这样 Vue 才能知道外部传入的哪些是
props，哪些是透传 attribute (关于透传
attribute，我们会在专门的章节中讨论)。

props 需要使用 props 选项来定义：

    export default {
      props: ['foo'],
      created() {
        // props 会暴露到 `this` 上
        console.log(this.foo)
      }
    }

除了使用字符串数组来声明 prop 外，还可以使用对象的形式：

    export default {
      props: {
        title: String,
        likes: Number
      }
    }

对于以对象形式声明中的每个属性，key 是 prop 的名称，而值则是该 prop
预期类型的构造函数。比如，如果要求一个 prop 的值是 number 类型，则可使用
Number 构造函数作为其声明的值。

对象形式的 props
声明不仅可以一定程度上作为组件的文档，而且如果其他开发者在使用你的组件时传递了错误的类型，也会在浏览器控制台中抛出警告。我们将在本章节稍后进一步讨论有关
prop 校验的更多细节。

1.  传递 prop 的细节

2.  Prop 名字格式

    如果一个 prop 的名字很长，应使用 camelCase 形式，因为它们是合法的
    JavaScript
    标识符，可以直接在模板的表达式中使用，也可以避免在作为属性 key
    名时必须加上引号。

        export default {
          props: {
            greetingMessage: String
          }
        }

        <span>{{ greetingMessage }}</span>

    虽然理论上你也可以在向子组件传递 props 时使用 camelCase 形式 (使用
    DOM 内模板时例外)，但实际上为了和 HTML attribute
    对齐，我们通常会将其写为 kebab-case 形式：

        <MyComponent greeting-message="hello" />

    对于组件名我们推荐使用
    PascalCase，因为这提高了模板的可读性，能帮助我们区分 Vue 组件和原生
    HTML 元素。然而对于传递 props 来说，使用 camelCase
    并没有太多优势，因此我们推荐更贴近 HTML 的书写风格。

3.  静态 vs. 动态 Prop

    至此，你已经见过了很多像这样的静态值形式的 props：

        <BlogPost title="My journey with Vue" />

    相应地，还有使用 v-bind 或缩写 : 来进行动态绑定的 props：

        <!-- 根据一个变量的值动态传入 -->
        <BlogPost :title="post.title" />

        <!-- 根据一个更复杂表达式的值动态传入 -->
        <BlogPost :title="post.title + ' by ' + post.author.name" />

4.  传递不同的值类型

    在上述的两个例子中，我们只传入了字符串值，但实际上任何类型的值都可以作为
    props 的值被传递。

    1.  Number

            <!-- 虽然 `42` 是个常量，我们还是需要使用 v-bind -->
            <!-- 因为这是一个 JavaScript 表达式而不是一个字符串 -->
            <BlogPost :likes="42" />

            <!-- 根据一个变量的值动态传入 -->
            <BlogPost :likes="post.likes" />

    2.  Boolean

            <!-- 仅写上 prop 但不传值，会隐式转换为 `true` -->
            <BlogPost is-published />

            <!-- 虽然 `false` 是静态的值，我们还是需要使用 v-bind -->
            <!-- 因为这是一个 JavaScript 表达式而不是一个字符串 -->
            <BlogPost :is-published="false" />

            <!-- 根据一个变量的值动态传入 -->
            <BlogPost :is-published="post.isPublished" />

    3.  Array

            <!-- 虽然这个数组是个常量，我们还是需要使用 v-bind -->
            <!-- 因为这是一个 JavaScript 表达式而不是一个字符串 -->
            <BlogPost :comment-ids="[234, 266, 273]" />

            <!-- 根据一个变量的值动态传入 -->
            <BlogPost :comment-ids="post.commentIds" />

    4.  Object

            <!-- 虽然这个对象字面量是个常量，我们还是需要使用 v-bind -->
            <!-- 因为这是一个 JavaScript 表达式而不是一个字符串 -->
            <BlogPost
              :author="{
                name: 'Veronica',
                company: 'Veridian Dynamics'
              }"
             />

            <!-- 根据一个变量的值动态传入 -->
            <BlogPost :author="post.author" />

5.  使用一个对象绑定多个 prop

    如果你想要将一个对象的所有属性都当作 props
    传入，你可以使用没有参数的 v-bind，即只使用 v-bind 而非
    :prop-name。例如，这里有一个 post 对象：

        export default {
          data() {
            return {
              post: {
                id: 1,
                title: 'My Journey with Vue'
              }
            }
          }
        }

    以及下面的模板：

        <BlogPost v-bind="post" />

    而这实际上等价于：

        <BlogPost :id="post.id" :title="post.title" />

6.  单向数据流

    所有的 props 都遵循着单向绑定原则，props
    因父组件的更新而变化，自然地将新的状态向下流往子组件，而不会逆向传递。这避免了子组件意外修改父组件的状态的情况，不然应用的数据流将很容易变得混乱而难以理解。

    另外，每次父组件更新后，所有的子组件中的 props
    都会被更新到最新值，这意味着你不应该在子组件中去更改一个
    prop。若你这么做了，Vue 会在控制台上向你抛出警告：

        export default {
          props: ['foo'],
          created() {
            // ❌ 警告！prop 是只读的！
            this.foo = 'bar'
          }
        }

    导致你想要更改一个 prop 的需求通常来源于以下两种场景：

    prop
    被用于传入初始值；而子组件想在之后将其作为一个局部数据属性。在这种情况下，最好是新定义一个局部数据属性，从
    props 上获取初始值即可：

        export default {
          props: ['initialCounter'],
          data() {
            return {
              // 计数器只是将 this.initialCounter 作为初始值
              // 像下面这样做就使 prop 和后续更新无关了
              counter: this.initialCounter
            }
          }
        }

    需要对传入的 prop 值做进一步的转换。在这种情况中，最好是基于该 prop
    值定义一个计算属性：

        export default {
          props: ['size'],
          computed: {
            // 该 prop 变更时计算属性也会自动更新
            normalizedSize() {
              return this.size.trim().toLowerCase()
            }
          }
        }

7.  更改对象 / 数组类型的 props

    当对象或数组作为 props 被传入时，虽然子组件无法更改 props
    绑定，但仍然可以更改对象或数组内部的值。这是因为 JavaScript
    的对象和数组是按引用传递，而对 Vue
    来说，禁止这样的改动，虽然可能生效，但有很大的性能损耗，比较得不偿失。

    这种更改的主要缺陷是它允许了子组件以某种不明显的方式影响父组件的状态，可能会使数据流在将来变得更难以理解。在最佳实践中，你应该尽可能避免这样的更改，除非父子组件在设计上本来就需要紧密耦合。在大多数场景下，子组件应该抛出一个事件来通知父组件做出改变。

### Prop 校验

Vue 组件可以更细致地声明对传入的 props
的校验要求。比如我们上面已经看到过的类型声明，如果传入的值不满足类型要求，Vue
会在浏览器控制台中抛出警告来提醒使用者。这在开发给其他开发者使用的组件时非常有用。

要声明对 props 的校验，你可以向 props 选项提供一个带有 props
校验选项的对象，例如：

``` javascript
export default {
  props: {
    // 基础类型检查
    //（给出 `null` 和 `undefined` 值则会跳过任何类型检查）
    propA: Number,
    // 多种可能的类型
    propB: [String, Number],
    // 必传，且为 String 类型
    propC: {
      type: String,
      required: true
    },
    // Number 类型的默认值
    propD: {
      type: Number,
      default: 100
    },
    // 对象类型的默认值
    propE: {
      type: Object,
      // 对象或者数组应当用工厂函数返回。
      // 工厂函数会收到组件所接收的原始 props
      // 作为参数
      default(rawProps) {
        return { message: 'hello' }
      }
    },
    // 自定义类型校验函数
    propF: {
      validator(value) {
        // The value must match one of these strings
        return ['success', 'warning', 'danger'].includes(value)
      }
    },
    // 函数类型的默认值
    propG: {
      type: Function,
      // 不像对象或数组的默认，这不是一个
      // 工厂函数。这会是一个用来作为默认值的函数
      default() {
        return 'Default function'
      }
    }
  }
}
```

一些补充细节：

-   所有 prop 默认都是可选的，除非声明了 required: true。
-   除 Boolean 外的未传递的可选 prop 将会有一个默认值 undefined。
-   Boolean 类型的未传递 prop 将被转换为 false。这可以通过为它设置
    default 来更改——例如：设置为 default: undefined 将与非布尔类型的
    prop 的行为保持一致。
-   如果声明了 default 值，那么在 prop 的值被解析为 undefined 时，无论
    prop 是未被传递还是显式指明的 undefined，都会改为 default 值。

当 prop 的校验失败后，Vue 会抛出一个控制台警告 (在开发模式下)。

注意 prop 的校验是在组件实例被创建之前，所以实例的属性 (比如
data、computed 等) 将在 default 或 validator 函数中不可用。

1.  运行时类型检查

    校验选项中的 type 可以是下列这些原生构造函数：

    -   String
    -   Number
    -   Boolean
    -   Array
    -   Object
    -   Date
    -   Function
    -   Symbol

    另外，type 也可以是自定义的类或构造函数，Vue 将会通过 instanceof
    来检查类型是否匹配。例如下面这个类：

        class Person {
          constructor(firstName, lastName) {
            this.firstName = firstName
            this.lastName = lastName
          }
        }

    你可以将其作为一个 prop 的类型：

        export default {
          props: {
            author: Person
          }
        }

    Vue 会通过 instanceof Person 来校验 author prop 的值是否是 Person
    类的一个实例。

2.  Boolean 类型转换

    为了更贴近原生 boolean attributes 的行为，声明为 Boolean 类型的
    props 有特别的类型转换规则。以带有如下声明的 \<MyComponent\>
    组件为例：

        export default {
          props: {
            disabled: Boolean
          }
        }

    该组件可以被这样使用：

        <!-- 等同于传入 :disabled="true" -->
        <MyComponent disabled />

        <!-- 等同于传入 :disabled="false" -->
        <MyComponent />

    当一个 prop 被声明为允许多种类型时，Boolean
    的转换规则也将被应用。然而，当同时允许 String 和 Boolean
    时，有一种边缘情况——只有当 Boolean 出现在 String 之前时，Boolean
    转换规则才适用：

        // disabled 将被转换为 true
        export default {
          props: {
            disabled: [Boolean, Number]
          }
        }

        // disabled 将被转换为 true
        export default {
          props: {
            disabled: [Boolean, String]
          }
        }

        // disabled 将被转换为 true
        export default {
          props: {
            disabled: [Number, Boolean]
          }
        }

        // disabled 将被解析为空字符串 (disabled="")
        export default {
          props: {
            disabled: [String, Boolean]
          }
        }

## 组件事件

### 触发与监听事件

在组件的模板表达式中，可以直接使用 \$emit 方法触发自定义事件 (例如：在
v-on 的处理函数中)：

    <!-- MyComponent -->
    <button @click="$emit('someEvent')">click me</button>

\$emit() 方法在组件实例上也同样以 this.\$emit() 的形式可用：

    export default {
      methods: {
        submit() {
          this.$emit('someEvent')
        }
      }
    }

父组件可以通过 v-on (缩写为 @) 来监听事件：

    <MyComponent @some-event="callback" />

同样，组件的事件监听器也支持 .once 修饰符：

    <MyComponent @some-event.once="callback" />

像组件与 prop
一样，事件的名字也提供了自动的格式转换。注意这里我们触发了一个以
camelCase 形式命名的事件，但在父组件中可以使用 kebab-case 形式来监听。与
prop 大小写格式一样，在模板中我们也推荐使用 kebab-case
形式来编写监听器。

和原生 DOM
事件不一样，组件触发的事件没有冒泡机制。你只能监听直接子组件触发的事件。平级组件或是跨越多层嵌套的组件间通信，应使用一个外部的事件总线，或是使用一个全局状态管理方案。

### 事件参数

有时候我们会需要在触发事件时附带一个特定的值。举例来说，我们想要
\<BlogPost\> 组件来管理文本会缩放得多大。在这个场景下，我们可以给 \$emit
提供一个额外的参数：

    <button @click="$emit('increaseBy', 1)">
      Increase by 1
    </button>

然后我们在父组件中监听事件，我们可以先简单写一个内联的箭头函数作为监听器，此函数会接收到事件附带的参数：

    <MyButton @increase-by="(n) => count += n" />

或者，也可以用一个组件方法来作为事件处理函数：

    <MyButton @increase-by="increaseCount" />

该方法也会接收到事件所传递的参数：

    methods: {
      increaseCount(n) {
        this.count += n
      }
    }

所有传入 \$emit()
的额外参数都会被直接传向监听器。举例来说，\$emit('foo', 1, 2, 3)
触发后，监听器函数将会收到这三个参数值。

### 声明触发的事件

组件可以显式地通过 emits 选项来声明它要触发的事件：

    export default {
      emits: ['inFocus', 'submit']
    }

这个 emits 选项和 defineEmits() 宏还支持对象语法。通过 TypeScript
为参数指定类型，它允许我们对触发事件的参数进行验证：

    export default {
      emits: {
        submit(payload: { email: string, password: string }) {
          // 通过返回值为 `true` 还是为 `false` 来判断
          // 验证是否通过
        }
      }
    }

TypeScript 用户请参考：如何为组件所抛出的事件标注类型。

尽管事件声明是可选的，我们还是推荐你完整地声明所有要触发的事件，以此在代码中作为文档记录组件的用法。同时，事件声明能让
Vue 更好地将事件和透传 attribute
作出区分，从而避免一些由第三方代码触发的自定义 DOM
事件所导致的边界情况。

如果一个原生事件的名字 (例如 click) 被定义在 emits
选项中，则监听器只会监听组件触发的 click 事件而不会再响应原生的 click
事件。

### 事件校验

和对 props
添加类型校验的方式类似，所有触发的事件也可以使用对象形式来描述。

要为事件添加校验，那么事件可以被赋值为一个函数，接受的参数就是抛出事件时传入
this.\$emit 的内容，返回一个布尔值来表明事件是否合法。

    export default {
      emits: {
        // 没有校验
        click: null,

        // 校验 submit 事件
        submit: ({ email, password }) => {
          if (email && password) {
            return true
          } else {
            console.warn('Invalid submit event payload!')
            return false
          }
        }
      },
      methods: {
        submitForm(email, password) {
          this.$emit('submit', { email, password })
        }
      }
    }

## 组件 v-model

v-model 可以在组件上使用以实现双向绑定。

首先让我们回忆一下 v-model 在原生元素上的用法：

    <input v-model="searchText" />

在代码背后，模板编译器会对 v-model
进行更冗长的等价展开。因此上面的代码其实等价于下面这段：

    <input
      :value="searchText"
      @input="searchText = $event.target.value"
    />

而当使用在一个组件上时，v-model 会被展开为如下的形式：

    <CustomInput
      :model-value="searchText"
      @update:model-value="newValue => searchText = newValue"
    />

要让这个例子实际工作起来，\<CustomInput\> 组件内部需要做两件事：

-   将内部原生 \<input\> 元素的 value attribute 绑定到 modelValue prop
-   当原生的 input 事件触发时，触发一个携带了新值的 update:modelValue
    自定义事件

这里是相应的代码：

    <!-- CustomInput.vue -->
    <script>
    export default {
      props: ['modelValue'],
      emits: ['update:modelValue']
    }
    </script>

    <template>
      <input
        :value="modelValue"
        @input="$emit('update:modelValue', $event.target.value)"
      />
    </template>

现在 v-model 可以在这个组件上正常工作了：

    <CustomInput v-model="searchText" />

另一种在组件内实现 v-model 的方式是使用一个可写的，同时具有 getter 和
setter 的 computed 属性。get 方法需返回 modelValue prop，而 set
方法需触发相应的事件：

``` javascript
<!-- CustomInput.vue -->
<script>
export default {
  props: ['modelValue'],
  emits: ['update:modelValue'],
  computed: {
    value: {
      get() {
        return this.modelValue
      },
      set(value) {
        this.$emit('update:modelValue', value)
      }
    }
  }
}
</script>

<template>
  <input v-model="value" />
</template>
```

### v-model 的参数

默认情况下，v-model 在组件上都是使用 modelValue 作为 prop，并以
update:modelValue 作为对应的事件。我们可以通过给 v-model
指定一个参数来更改这些名字：

    <MyComponent v-model:title="bookTitle" />

在这个例子中，子组件应声明一个 title prop，并通过触发 update:title
事件更新父组件值：

``` javascript
<!-- MyComponent.vue -->
<script>
export default {
  props: ['title'],
  emits: ['update:title']
}
</script>

<template>
  <input
    type="text"
    :value="title"
    @input="$emit('update:title', $event.target.value)"
  />
</template>
```

### 多个 v-model 绑定

利用刚才在 v-model
参数小节中学到的指定参数与事件名的技巧，我们可以在单个组件实例上创建多个
v-model 双向绑定。

组件上的每一个 v-model 都会同步不同的 prop，而无需额外的选项：

    <UserName
      v-model:first-name="first"
      v-model:last-name="last"
    />

``` html
<script>
export default {
  props: {
    firstName: String,
    lastName: String
  },
  emits: ['update:firstName', 'update:lastName']
}
</script>

<template>
  <input
    type="text"
    :value="firstName"
    @input="$emit('update:firstName', $event.target.value)"
  />
  <input
    type="text"
    :value="lastName"
    @input="$emit('update:lastName', $event.target.value)"
  />
</template>
```

### 处理 v-model 修饰符

在学习输入绑定时，我们知道了 v-model 有一些内置的修饰符，例如
.trim，.number 和 .lazy。在某些场景下，你可能想要一个自定义组件的
v-model 支持自定义的修饰符。

我们来创建一个自定义的修饰符 capitalize，它会自动将 v-model
绑定输入的字符串值第一个字母转为大写：

    <MyComponent v-model.capitalize="myText" />

组件的 v-model 上所添加的修饰符，可以通过 modelModifiers prop
在组件内访问到。在下面的组件中，我们声明了 modelModifiers 这个
prop，它的默认值是一个空对象：

``` html
<script>
export default {
  props: {
    modelValue: String,
    modelModifiers: {
      default: () => ({})
    }
  },
  emits: ['update:modelValue'],
  created() {
    console.log(this.modelModifiers) // { capitalize: true }
  }
}
</script>

<template>
  <input
    type="text"
    :value="modelValue"
    @input="$emit('update:modelValue', $event.target.value)"
  />
</template>
```

注意这里组件的 modelModifiers prop 包含了 capitalize 且其值为
true，因为它在模板中的 v-model 绑定 v-model.capitalize="myText"
上被使用了。

有了这个 prop，我们就可以检查 modelModifiers
对象的键，并编写一个处理函数来改变抛出的值。在下面的代码里，我们就是在每次
\<input /\> 元素触发 input 事件时将值的首字母大写：

    <script>
    export default {
      props: {
        modelValue: String,
        modelModifiers: {
          default: () => ({})
        }
      },
      emits: ['update:modelValue'],
      methods: {
        emitValue(e) {
          let value = e.target.value
          if (this.modelModifiers.capitalize) {
            value = value.charAt(0).toUpperCase() + value.slice(1)
          }
          this.$emit('update:modelValue', value)
        }
      }
    }
    </script>

    <template>
      <input type="text" :value="modelValue" @input="emitValue" />
    </template>

1.  带参数的 v-model 修饰符

    对于又有参数又有修饰符的 v-model 绑定，生成的 prop 名将是 arg +
    "Modifiers"。举例来说：

        <MyComponent v-model:title.capitalize="myText">

    相应的声明应该是：

    ``` javascript
    export default {
      props: ['title', 'titleModifiers'],
      emits: ['update:title'],
      created() {
        console.log(this.titleModifiers) // { capitalize: true }
      }
    }
    ```

    这里是另一个例子，展示了如何在使用多个不同参数的 v-model
    时使用修饰符：

        <UserName
          v-model:first-name.capitalize="first"
          v-model:last-name.uppercase="last"
        />

    ``` javascript
    <script>
    export default {
      props: {
        firstName: String,
        lastName: String,
        firstNameModifiers: {
          default: () => ({})
        },
        lastNameModifiers: {
          default: () => ({})
        }
      },
      emits: ['update:firstName', 'update:lastName'],
      created() {
        console.log(this.firstNameModifiers) // { capitalize: true }
        console.log(this.lastNameModifiers) // { uppercase: true}
      }
    }
    </script>
    ```

## 透传 Attributes

### Attributes 继承

“透传 attribute”指的是传递给一个组件，却没有被该组件声明为 props 或
emits 的 attribute 或者 v-on 事件监听器。最常见的例子就是 class、style
和 id。

当一个组件以单个元素为根作渲染时，透传的 attribute
会自动被添加到根元素上。举例来说，假如我们有一个 \<MyButton\>
组件，它的模板长这样：

    <!-- <MyButton> 的模板 -->
    <button>click me</button>

一个父组件使用了这个组件，并且传入了 class：

    <MyButton class="large" />

最后渲染出的 DOM 结果是：

    <button class="large">click me</button>

这里，\<MyButton\> 并没有将 class 声明为一个它所接受的 prop，所以 class
被视作透传 attribute，自动透传到了 \<MyButton\> 的根元素上。

1.  对 class 和 style 的合并

    如果一个子组件的根元素已经有了 class 或 style
    attribute，它会和从父组件上继承的值合并。如果我们将之前的
    \<MyButton\> 组件的模板改成这样：

        <!-- <MyButton> 的模板 -->
        <button class="btn">click me</button>

    则最后渲染出的 DOM 结果会变成：

        <button class="btn large">click me</button>

2.  v-on 监听器继承

    同样的规则也适用于 v-on 事件监听器：

        <MyButton @click="onClick" />

    click 监听器会被添加到 \<MyButton\> 的根元素，即那个原生的
    \<button\> 元素之上。当原生的 \<button\> 被点击，会触发父组件的
    onClick 方法。同样的，如果原生 button 元素自身也通过 v-on
    绑定了一个事件监听器，则这个监听器和从父组件继承的监听器都会被触发。

3.  深层组件继承

    有些情况下一个组件会在根节点上渲染另一个组件。例如，我们重构一下
    \<MyButton\>，让它在根节点上渲染 \<BaseButton\>：

        <!-- <MyButton/> 的模板，只是渲染另一个组件 -->
        <BaseButton />

    此时 \<MyButton\> 接收的透传 attribute 会直接继续传给
    \<BaseButton\>。

    请注意：

    -   透传的 attribute 不会包含 \<MyButton\> 上声明过的 props 或是针对
        emits 声明事件的 v-on 侦听函数，换句话说，声明过的 props
        和侦听函数被 \<MyButton\>“消费”了。

    -   透传的 attribute 若符合声明，也可以作为 props 传入
        \<BaseButton\>。

### 禁用 Attributes 继承

如果你不想要一个组件自动地继承 attribute，你可以在组件选项中设置
inheritAttrs: false。

最常见的需要禁用 attribute 继承的场景就是 attribute
需要应用在根节点以外的其他元素上。通过设置 inheritAttrs 选项为
false，你可以完全控制透传进来的 attribute 被如何使用。

这些透传进来的 attribute 可以在模板的表达式中直接用 \$attrs 访问到。

    <span>Fallthrough attribute: {{ $attrs }}</span>

这个 \$attrs 对象包含了除组件所声明的 props 和 emits 之外的所有其他
attribute，例如 class，style，v-on 监听器等等。

有几点需要注意：

-   和 props 有所不同，透传 attributes 在 JavaScript
    中保留了它们原始的大小写，所以像 foo-bar 这样的一个 attribute
    需要通过 \$attrs\['foo-bar'\] 来访问。

-   像 @click 这样的一个 v-on 事件监听器将在此对象下被暴露为一个函数
    \$attrs.onClick。

现在我们要再次使用一下之前小节中的 \<MyButton\>
组件例子。有时候我们可能为了样式，需要在 \<button\> 元素外包装一层
\<div\>：

    <div class="btn-wrapper">
      <button class="btn">click me</button>
    </div>

我们想要所有像 class 和 v-on 监听器这样的透传 attribute 都应用在内部的
\<button\> 上而不是外层的 \<div\> 上。我们可以通过设定 inheritAttrs:
false 和使用 v-bind="\$attrs" 来实现：

    <div class="btn-wrapper">
      <button class="btn" v-bind="$attrs">click me</button>
    </div>

小提示：没有参数的 v-bind 会将一个对象的所有属性都作为 attribute
应用到目标元素上。

### 多根节点的 Attributes 继承

和单根节点组件有所不同，有着多个根节点的组件没有自动 attribute
透传行为。如果 \$attrs 没有被显式绑定，将会抛出一个运行时警告。

    <CustomLayout id="custom-layout" @click="changeValue" />

如果 \<CustomLayout\> 有下面这样的多根节点模板，由于 Vue 不知道要将
attribute 透传到哪里，所以会抛出一个警告。

    <header>...</header>
    <main>...</main>
    <footer>...</footer>

如果 \$attrs 被显式绑定，则不会有警告：

    <header>...</header>
    <main v-bind="$attrs">...</main>
    <footer>...</footer>

### 在 JavaScript 中访问透传 Attributes

如果需要，你可以通过 \$attrs 这个实例属性来访问组件的所有透传
attribute：

    export default {
      created() {
        console.log(this.$attrs)
      }
    }

## 插槽 Slots

## 依赖注入

## 异步组件
