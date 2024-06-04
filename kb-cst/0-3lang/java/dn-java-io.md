# java.io

## 什么是IO

IO:Input/Output即输入&输出，也称之为流，指的是数据从一个地点到另一个地点过程.

输入输出介质：
- 文件
- 网络
- 键盘（输入）
- 显示器（输出）

### IO分类

Java针对IO操作提供了相应的API，Java中几乎所有的IO操作都需要使用java.io包；java中对于流的分类包含各种方式：

按流向分（输入输出过程通常都是站在程序角度考虑）
- 输入流（Input）
- 输出流（Output）
  
按流的处理类型分
- 字节流（byte）
- 字符流（char）
  
按流的功能来分
- 节点流（直接跟输入输出源交互）
- 处理流（对其他流包装的流：包装流）

### 顶级父类
流虽然内容繁多，但是规律性很强，几乎所有的流都从四个基本流继承而来：

字节流
- java.io.InputStream
- java.io.OutputStream

字符流
- java.io.Reader
- java.io.Writer

以上四个流是所有java流的顶层父类，都是抽象类;

流的类型识别规律性很强：一般以Stream结尾的都是字节流；一般以Reader/Writer结尾的流都是字符流

## 字节流

字节概述

​ 在计算机系统中，一切都是字节：系统中存储的各种文件（文本文档，图片，视频，音频）的存储在计算机低层都是以字节的形式存储的，因此对于任何的文件操作都是可以使用一个一个字节进行操作的（读，写）；java.io中的字节流顶层父类是:InputStream/OutputStream.

### 字节输入流
java中字节输入流都是从java.io.InputStream继承而来。由于该类为抽象类，因此无法实例化的，所以jdk对于字节输入提供了一些能够直接使用子类：

- FileInputStream
- ByteArrayInputStream
- BufferedInputStream
- ObjectInputStream

InputStream常用方法

- int available():获取流中可读字节数
- int read():从流中读取一个字节，返回当前读取的字节数据
- int read(byte[] b):将读取的字节数据存储到字节缓冲区，并返回实际的读取字节总数
- skip(int b):跳过指定个字节发生下一次读取

Java的IO只能对标准文件发生读写操作，不允许直接对一个目录创建输入或者输出流（会导致java.io.IOException(拒绝访问)）

### 字节输出流
根据数据的流向除了可以进行读取（输入）操作之外，数据的写（输出）操作也是十分常见，java中的字节输出流都是从java.io.OutputStream继承过来

由于OutputStream是一个抽象类，无法实例化，因此jdk也提供了针对该流的子类：

- FileOutputStream
- ByteArrayOutputStream
- BufferedOutputStream
- ObjectOutputStream
- PrintStream

OutputStream类的常见方法：

- write(int b):将一个字节通过输出流写出到目标输出源
- write(byte[] b)：将一个字节数组通过输出流写出到目标输出源
- write(byte[] b,int offset,int len)将一个数组的offset开始写出len个字节到目标输出源

FileOutputStream
FileOutputStream是一个针对字节输出流的实现流，主要用于对文件进行写入操作，内部的方法主要是针对父类的实现

## 字符流

字符概述

通常在文本文件中，文件内容的存在形式一般是以一个个字符（一个中文汉字，一个英文字母，一个符号）的形式存在，在GBK编码模式下通常1个字符=2个字节;字符流一般适用于对文本数据的处理，java中所有的字符流几乎都是以Reader/Writer结尾，并且都是从两个基本的抽象类中继承：

- java.io.Reader：字符输入流
- java.io.Writer：字符输出流

### 字符输入流
字符输入流一般用于对文本数据进行读取操作，常见的子类：

- InputStreamReader
- FileReader
- BufferedReader
- CharArrayReader


### 字符输出流
字符输出流一般用于对文本数据进行写出操作，常见的子类：

- OutputStreamWriter
- BufferedWriter
- CharArrayWriter
- FileWriter
- PrintWriter

对于字符输出流，内部使用到了一个字符缓冲区(字符数组),在进行数据写出时，通常是将需要写出的数据缓存存在了字符数组中，然后在关闭流时（或者缓冲区存满时），一次性将缓冲区的数据写出到目标输出源， 如果需要在流未关闭前（或者缓冲区未满时）强制的将字符缓冲区中的数据写出，可以手动调用flush()强制输出。


## 处理流
对于流的处理类型（功能）来分又分为节点流和处理流：
- 节点流:也称之为低级流，直接跟输入输出源沟通的流（例如:FileReader,FileWriter,FileInputStream,FileOutputStream）
- 处理流:处理流也称之为高级流或包装流，即可以用于对其他节点流进行包装，以实现流的类型转换或者效率的提升。处理流主要由缓冲流和转换流构成

### 缓冲流
缓冲流是一种自带缓冲区的流，主要由以下四种构成

- BufferedInputStream：字节缓冲输入流
- BufferedOutputStream：字节缓冲输出流
- BufferedReader：字符缓冲输入流
- BufferedWriter：字符缓冲输出流

缓冲流内部实现原理使用了了一个默认大小为8kb的字节缓冲区，每次将缓冲区的空间存满之后再将数据通过流对象完成读写操作

### 转换流
在一些需求中经常会需要将一个字节流的数据转换为字符流；又或者需要将一个字符流转换为字节流，此时就需要使用转换流来完成功能，转换流一般应用于：文件转码，从网络中读取的数据为字节流时，但是该流包含内容是字符时，可以使用转换流实现转换；java.io中转换流主要由以下两个类构成：

- InputStreamReader：用于将字节输入流转换为字符输入流（字节->字符）
- OutputStreamWriter：用于将字符输出流转换为字节输出流（字符->字节）

``` java
//获取标准输入流
InputStream is = System.in;

//将字节流转换为字符流
Reader isr = new InputStreamReader(is);
//将字符节点流包装为缓冲流
BufferedReader br = new BufferedReader(isr);
//读取一行
String line = br.readLine();
System.out.println("输入的内容--->"+line);

```

综合案例：文件转码工具

日常的文件拷贝中由于多个编辑器（系统）的编码模式存在差异，因此极有可能出现文件乱码问题（对于文本文件较为常见），通过转换流可以实现文件转码的功能：

``` java
public class FileCharacterConvert {

    /**
     * 将一个目标文件的编码转换为新的编码
     * @param file  原始文件
     * @param targetDir 目标目录
     * @param oldEncoding  原始编码
     * @param newEncoding   新编码
     */
    public static void convert(File file,File targetDir, String oldEncoding, String newEncoding) throws IOException {

        //使用特定的编码获取文件的输入流
        FileInputStream fis = new FileInputStream(file);
        Reader reader = new InputStreamReader(fis,oldEncoding);
        BufferedReader br = new BufferedReader(reader);

        //使用特定的编码获取文件的输出流
        FileOutputStream fow = new FileOutputStream(new File(targetDir,file.getName()));
        Writer writer = new OutputStreamWriter(fow,newEncoding);
        BufferedWriter bw = new BufferedWriter(writer);

        //开始读写
        String line = "";
        //循环读取每一行文本以换行符作为一行的结束标记（但是换行符不会被作为内容读取）
        while((line = br.readLine()) != null){
            //写入读取的一行文本（）
            bw.write(line);
            //手动加入一个换行标记到文件，否则所有内容会在同一行显示
            bw.newLine();
            //将缓冲区的数据强制输出到目标输出源
            bw.flush();
        }
        bw.close();
        br.close();
    }

    public static void main(String[] args) throws IOException {
        //准备需要转换的文件
        File f = new File("C:\\Users\\Administrator\\Desktop\\GamePanel.java");
        convert(f,new File("C:\\Users\\Administrator\\Desktop\\temp"),"gbk","utf-8");
    }
}


```

## 打印流
java.io，对于数据的输出提供了两个特殊的流:打印流，打印流只有输出没有输入，并且可以直接针对输出源创建，也可以将其他输出流包装，打印流主要包含以下两个流：

- PrintStream:字节打印流
- PrintWriter:字符打印流

PrintStream

PrintStream是继承自java.io.OutputStream，是一个用于进行字节数据输出的流，内包含了大量的print/prinln重载方法；并且System.out实际上就是一个PrintStream

PrintWriter

PrintWriter和PrintStream区别在于，PrintWriter是基于字符的打印流（包含字符缓冲区），其API与PrintStream极其相似；在使用PrintWriter打印数据的时候记得执行flush()方法


## System

System有三个系统流：
- in
- out
- err

可以对这三个流重定向

``` java
  System.setOut(new PrintStream(new FileOutputStream(file)));
  System.setErr(new PrintStream(new FileOutputStream(file)));
  System.setIn(new FileInputStream(file));
```

** File类

File文件常用操作：
- createNewFile():  创建一个新文件
- File.separator
- File.pathSeparator
- exists(): 文件是否存在
- delete()
- mkdir(): 创建一个文件夹
- list()
- listFiles()
- isDirectory()

  
``` java
public static void print(File f){
    if(f==null){
       return ;
    }
    if(f.isDirectory()){
        File[] fileArray=f.listFiles();
        if(fileArray!=null){
            for (int i = 0; i < fileArray.length; i++) {
                //递归调用
                print(fileArray[i]);
            }
        }
    }
    else{
        System.out.println(f);
    }
}
```

** java.io.FileOutputStream

向文件中写入字符串

``` java
/**
 * 字节流
 * 向文件中写入字符串
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName="D:"+File.separator+"hello.txt";
        File f=new File(fileName);
        OutputStream out =new FileOutputStream(f);
        String str="你好";
        byte[] b=str.getBytes();
        out.write(b);
        out.close();
    }
}

/**
 * 字节流
 * 向文件中一个字节一个字节的写入字符串
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName="D:"+File.separator+"hello.txt";
        File f=new File(fileName);
        OutputStream out =new FileOutputStream(f);
        String str="你好";
        byte[] b=str.getBytes();
        for (int i = 0; i < b.length; i++) {
            out.write(b[i]);
        }
        out.close();
    }
}

/**
 * 字节流
 * 向文件中追加新内容：
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName="D:"+File.separator+"hello.txt";
        File f=new File(fileName);
        OutputStream out =new FileOutputStream(f,true);
        String str="Rollen";
        //String str="\r\nRollen";  可以换行
        byte[] b=str.getBytes();
        for (int i = 0; i < b.length; i++) {
            out.write(b[i]);
        }
        out.close();
    }
}
```

## java.io.FileInputStream

读取文件内容

``` java
/**
 * 字节流
 * 读文件内容,节省空间
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName="D:"+File.separator+"hello.txt";
        File f=new File(fileName);
        InputStream in=new FileInputStream(f);
        byte[] b=new byte[(int)f.length()];
        in.read(b);
        System.out.println("文件长度为："+f.length());
        in.close();
        System.out.println(new String(b));
    }
}

/**
 * 字节流
 * 读文件
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName="D:"+File.separator+"hello.txt";
        File f=new File(fileName);
        InputStream in=new FileInputStream(f);
        byte[] b=new byte[1024];
        int count =0;
        int temp=0;
        // 当独到文件末尾的时候会返回-1.正常情况下是不会返回-1的
        while((temp=in.read())!=(-1)){
            b[count++]=(byte)temp;
        }
        in.close();
        System.out.println(new String(b));
    }
}
```

## PipedStream

管道流主要可以进行两个线程之间的通信。

PipedOutputStream 管道输出流

PipedInputStream 管道输入流

```
/**
 * 验证管道流
 * */
import java.io.*;
 
/**
 * 消息发送类
 * */
class Send implements Runnable{
    private PipedOutputStream out=null;
    public Send() {
        out=new PipedOutputStream();
    }
    public PipedOutputStream getOut(){
        return this.out;
    }
    public void run(){
        String message="hello , ITEEDU";
        try{
            out.write(message.getBytes());
        }catch (Exception e) {
            e.printStackTrace();
        }try{
            out.close();
        }catch (Exception e) {
            e.printStackTrace();
        }
    }
}
 
/**
 * 接受消息类
 * */
class Recive implements Runnable{
    private PipedInputStream input=null;
    public Recive(){
        this.input=new PipedInputStream();
    }
    public PipedInputStream getInput(){
        return this.input;
    }
    public void run(){
        byte[] b=new byte[1000];
        int len=0;
        try{
            len=this.input.read(b);
        }catch (Exception e) {
            e.printStackTrace();
        }try{
            input.close();
        }catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("接受的内容为 "+(new String(b,0,len)));
    }
}
/**
 * 测试类
 * */
class hello{
    public static void main(String[] args) throws IOException {
        Send send=new Send();
        Recive recive=new Recive();
        try{
//管道连接
            send.getOut().connect(recive.getInput());
        }catch (Exception e) {
            e.printStackTrace();
        }
        new Thread(send).start();
        new Thread(recive).start();
    }
}
```

【运行结果】：

接受的内容为 hello , ITEEDU

## PrintStream
``` java
/**
 * 使用PrintStream进行输出
 * */
import java.io.*;
 
class hello {
    public static void main(String[] args) throws IOException {
        PrintStream print = new PrintStream(new FileOutputStream(new File("d:"
                + File.separator + "hello.txt")));
        print.println(true);
        print.println("ITEEDU");
        print.close();
    }
}
``` java

当然也可以格式化输出

```
/**
 * 使用PrintStream进行输出
 * 并进行格式化
 * */
import java.io.*;
class hello {
    public static void main(String[] args) throws IOException {
        PrintStream print = new PrintStream(new FileOutputStream(new File("d:"
                + File.separator + "hello.txt")));
        String name="Rollen";
        int age=20;
        print.printf("姓名：%s. 年龄：%d.",name,age);
        print.close();
    }
}
```

## 字节转字符

将字节输出流转化为字符输出流

``` java
/**
 * 将字节输出流转化为字符输出流
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName= "d:"+File.separator+"hello.txt";
        File file=new File(fileName);
        Writer out=new OutputStreamWriter(new FileOutputStream(file));
        out.write("hello");
        out.close();
    }
}
```

将字节输入流变为字符输入流

``` java
/**
 * 将字节输入流变为字符输入流
 * */
import java.io.*;
class hello{
    public static void main(String[] args) throws IOException {
        String fileName= "d:"+File.separator+"hello.txt";
        File file=new File(fileName);
        Reader read=new InputStreamReader(new FileInputStream(file));
        char[] b=new char[100];
        int len=read.read(b);
        System.out.println(new String(b,0,len));
        read.close();
    }
}
```

## Data读写

有时没有必要存储整个对象的信息，而只是要存储一个对象的成员数据，成员数据的类型假设都是Java的基本数据类型，这样的需求不必使用到与Object输入、输出相关的流对象，可以使用DataInputStream、DataOutputStream来写入或读出数据。

DataOutputStream:

``` java
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
 
public class DataOutputStreamDemo{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "hello.txt");
        char[] ch = { 'A', 'B', 'C' };
        DataOutputStream out = null;
        out = new DataOutputStream(new FileOutputStream(file));
        for(char temp : ch){
            out.writeChar(temp);
        }
        out.close();
    }
}
```

现在我们在上面例子的基础上，使用DataInputStream读出内容

``` java
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
 
public class DataOutputStreamDemo{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "hello.txt");
        DataInputStream input = new DataInputStream(new FileInputStream(file));
        char[] ch = new char[10];
        int count = 0;
        char temp;
        while((temp = input.readChar()) != 'C'){
            ch[count++] = temp;
        }
        System.out.println(ch);
    }
}
```

** SequenceInputStream

SequenceInputStream主要用来将2个流合并在一起，比如将两个txt中的内容合并为另外一个txt。下面给出一个实例：

``` java
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.SequenceInputStream;
 
/**
 * 将两个文本文件合并为另外一个文本文件
 * */
public class SequenceInputStreamDemo{
    public static void main(String[] args) throws IOException{
        File file1 = new File("d:" + File.separator + "hello1.txt");
        File file2 = new File("d:" + File.separator + "hello2.txt");
        File file3 = new File("d:" + File.separator + "hello.txt");
        InputStream input1 = new FileInputStream(file1);
        InputStream input2 = new FileInputStream(file2);
        OutputStream output = new FileOutputStream(file3);
        // 合并流
        SequenceInputStream sis = new SequenceInputStream(input1, input2);
        int temp = 0;
        while((temp = sis.read()) != -1){
            output.write(temp);
        }
        input1.close();
        input2.close();
        output.close();
        sis.close();
    }
}
```

## Scanner
其实我们比较常用的是采用Scanner类来进行数据输入

``` java
import java.util.Scanner;
 
/**
 * Scanner的小例子，从键盘读数据
 * */
public class ScannerDemo{
    public static void main(String[] args){
        Scanner sca = new Scanner(System.in);
        // 读一个整数
        int temp = sca.nextInt();
        System.out.println(temp);
        //读取浮点数
        float flo=sca.nextFloat();
        System.out.println(flo);
        //读取字符
        //...等等的，都是一些太基础的，就不师范了。
    }
}
```

其实Scanner可以接受任何的输入流

``` java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
 
/**
 * Scanner的小例子，从文件中读内容
 * */
public class ScannerDemo{
    public static void main(String[] args){
 
        File file = new File("d:" + File.separator + "hello.txt");
        Scanner sca = null;
        try{
            sca = new Scanner(file);
        }catch(FileNotFoundException e){
            e.printStackTrace();
        }
        String str = sca.next();
        System.out.println("从文件中读取的内容是：" + str);
    }
}
```

## 文件压缩

先举一个压缩单个文件的例子吧：

``` java
public class ZipOutputStreamDemo1{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "hello.txt");
        File zipFile = new File("d:" + File.separator + "hello.zip");
        InputStream input = new FileInputStream(file);
        ZipOutputStream zipOut = new ZipOutputStream(new FileOutputStream(
                zipFile));
        zipOut.putNextEntry(new ZipEntry(file.getName()));
        // 设置注释
        zipOut.setComment("hello");
        int temp = 0;
        while((temp = input.read()) != -1){
            zipOut.write(temp);
        }
        input.close();
        zipOut.close();
    }
}
```

下面的们来看看如何压缩多个文件。

``` java
/**
 * 一次性压缩多个文件
 * */
public class ZipOutputStreamDemo2{
    public static void main(String[] args) throws IOException{
        // 要被压缩的文件夹
        File file = new File("d:" + File.separator + "temp");
        File zipFile = new File("d:" + File.separator + "zipFile.zip");
        InputStream input = null;
        ZipOutputStream zipOut = new ZipOutputStream(new FileOutputStream(
                zipFile));
        zipOut.setComment("hello");
        if(file.isDirectory()){
            File[] files = file.listFiles();
            for(int i = 0; i < files.length; ++i){
                input = new FileInputStream(files[i]);
                zipOut.putNextEntry(new ZipEntry(file.getName()
                        + File.separator + files[i].getName()));
                int temp = 0;
                while((temp = input.read()) != -1){
                    zipOut.write(temp);
                }
                input.close();
            }
        }
        zipOut.close();
    }
}
```

java中的每一个压缩文件都是可以使用ZipFile来进行表示的

``` java 
/**
 * ZipFile演示
 * */
public class ZipFileDemo{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "hello.zip");
        ZipFile zipFile = new ZipFile(file);
        System.out.println("压缩文件的名称为：" + zipFile.getName());
    }
}
```

解压单个压缩文件

``` java
/**
 * 解压缩文件（压缩文件中只有一个文件的情况）
 * */
public class ZipFileDemo2{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "hello.zip");
        File outFile = new File("d:" + File.separator + "unZipFile.txt");
        ZipFile zipFile = new ZipFile(file);
        ZipEntry entry = zipFile.getEntry("hello.txt");
        InputStream input = zipFile.getInputStream(entry);
        OutputStream output = new FileOutputStream(outFile);
        int temp = 0;
        while((temp = input.read()) != -1){
            output.write(temp);
        }
        input.close();
        output.close();
    }
}
```

当我们需要解压缩多个文件的时候，ZipEntry就无法使用了，如果想操作更加复杂的压缩文件，我们就必须使用ZipInputStream类

``` java
/**
 * 解压缩一个压缩文件中包含多个文件的情况
 * */
public class ZipFileDemo3{
    public static void main(String[] args) throws IOException{
        File file = new File("d:" + File.separator + "zipFile.zip");
        File outFile = null;
        ZipFile zipFile = new ZipFile(file);
        ZipInputStream zipInput = new ZipInputStream(new FileInputStream(file));
        ZipEntry entry = null;
        InputStream input = null;
        OutputStream output = null;
        while((entry = zipInput.getNextEntry()) != null){
            System.out.println("解压缩" + entry.getName() + "文件");
            outFile = new File("d:" + File.separator + entry.getName());
            if(!outFile.getParentFile().exists()){
                outFile.getParentFile().mkdir();
            }
            if(!outFile.exists()){
                outFile.createNewFile();
            }
            input = zipFile.getInputStream(entry);
            output = new FileOutputStream(outFile);
            int temp = 0;
            while((temp = input.read()) != -1){
                output.write(temp);
            }
            input.close();
            output.close();
        }
    }
}
```
