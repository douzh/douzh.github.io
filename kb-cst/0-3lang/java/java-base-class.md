## JAVA创建对象过程

    public class Print {
         public Print(String s){
             System.out.print(s + " ");
         }
    }
    
     public class Test1 {
    
        public static Print obj1 = new Print("1");
        
        public Print obj2 = new Print("2");
    
        public static Print obj3 = new Print("3");
        
        static{
            new Print("4");
        }
        
        public static Print obj4 = new Print("5");
        
        public Print obj5 = new Print("6");
        
        public Test1(){
            new Print("7");
        }
        
    }
    
     public class Test2 extends Test1{
    
        static{
            new Print("a");
        }
        
        public static Print obj1 = new Print("b");
        
        public Print obj2 = new Print("c");
        
        public Test2(){
            new Print("d");
        }
        
        public static Print obj3 = new Print("e");
        
        public Print obj4 = new Print("f");
        
        public static void main(String [] args){
            Test1 obj1 = new Test2();
            Test1 obj2 = new Test2();
        }
    }

执行main方法，程序输出顺序为：

    1 3 4 5 a b e 2 6 7 c f d 2 6 7 c f d

输出结果表明，程序的执行顺序为：

**如果类还没有被加载：**

1、先执行父类的静态代码块和静态变量初始化，并且静态代码块和静态变量的执行顺序只跟代码中出现的顺序有关。

2、执行子类的静态代码块和静态变量初始化。

3、执行父类的实例变量初始化

4、执行父类的构造函数

5、执行子类的实例变量初始化

6、执行子类的构造函数

**如果类已经被加载：**

则静态代码块和静态变量就不用重复执行，再创建类对象时，只执行与实例相关的变量初始化和构造方法。