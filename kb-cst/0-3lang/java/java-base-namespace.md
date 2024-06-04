先来看一段代码

```
public class Main {

	public static void main(String[] args) {
		Object o=new C();
		System.out.println(((C)o).name);
		System.out.println(((B)o).name);
		System.out.println(((A)o).name);
	}
}

class A {
	String name="this is A";
}

class B extends A{
	String name="this is B";
}

class C extends B{
	String name="this is C";
}
```

输出：

```
this is C
this is B
this is A
```

我们知道，同一个命名空间（作用域）是不能有同一个名字的。

JAVA和python、javascritpt这些语言不一样，构造器会从父类一直执行到子类。

从上面的代码看出，在继承链上的每个类都有自己的命名空间，也就是一个java对象是一串空间，python的是一个空间。

再看一下方法的命名空间：

```
package test;

public class Main {

	public static void main(String[] args) {
		Object o=new C();
		((C)o).print();
		((B)o).print();
		((A)o).print();
	}
}

class A {
	String name="this is A";
	public void print() {
		System.out.println(name);
	}
}

class B extends A{
	String name="this is B";
	public void print() {
		System.out.println(name);
	}
}

class C extends B{
	String name="this is C";
	public void print() {
		System.out.println(name);
	}
}
```

输出：

```
this is C
this is C
this is C
```

可见，通过实例访问父类被覆盖的方法是行不通的，我想到的只能是通过反射。

类是方法的命名空间，方法的查找都是从子类向上查找的，所以直接访问父类的是行不通的。

JAVA实例命名空间是个串，类的命名空间也是个串，实例命名空间是构造器执行是创建的，类命名空间是类加载时创建的。

实例可以调用类命名空间的方法是因为实例和类空间有关联，可以看看其它语言的apply方法，本质上都是过程式的调用。