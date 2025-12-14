# 重积分

## 定义

定义 设$f(x,y)$是有界闭区域 D 上的有界函数,将闭区域 D 任意分成n个小闭区域

$$\Delta \sigma_1,\Delta \sigma_2,\cdots,\Delta \sigma_n$$

其中$\Delta \sigma_i$表示第$i$个小闭区域,也表示他的面积。在$\Delta \sigma_i$上任取一点$(\xi_i,\eta_i)$,作乘积$f(\xi_i,\eta_i)\Delta \sigma_i(i=1,2,\cdots,n)$,并作和$\sum_{i=1}^n f(\xi_i,\eta_i)\Delta \sigma_i$。如果当各小闭区域的直径中的最大值$\lambda$趋于零时,这和的极限总存在,则称此极限为函数$f(x,y)$在闭区域 D 上的二重积分,记作$\iint_D f(x,y)d\sigma$,即

$$\iint_D f(x,y)d\sigma=\lim_{\lambda\to 0}\sum_{i=1}^n f(\xi_i,\eta_i)\Delta \sigma_i$$

其中$f(x,y)$叫被积函数,$f(x,y)d\sigma$叫被积表达式,$d\sigma$叫面积元素，
x与y叫积分变量，D叫做积分区域，$\sum_{i=1}^n f(\xi_i,\eta_i)\Delta \sigma_i$叫做积分和。

在二重积分的定义中对闭区域 D的划分是任意的,如果在直角坐标系中用平行于坐标轴的直线网来划分 D,那么除了包含边界点的一些小闭区域外D,其余的小闭区域都是矩形闭区域.设矩形闭区域$\Delta \sigma_i$，的边长为$\Delta x_j$和$\Delta x_k$则$\Delta \sigma_i=\Delta x_j \cdot \Delta x_k$因此在直角坐标系中,有时也把面积元素$\Delta \sigma_i$记作 $dxdy$,而把重积分记作

$$\iint_D f(x,y)dxdy$$

## 二重积分的性质

## 二重积分的计算

$$\iint_D f(x,y)d\sigma=\int_a^b dx\int_{\varphi_1(x)}^{\varphi_2(x)} f(x,y)dy\\=\int_a^b dy\int_{\varphi_1(y)}^{\varphi_2(y)} f(x,y)dx$$

### 利用极坐标计算二重积分

## 三重积分

定义 设$f(x,y,x)$是空间有界闭区域 $\Omega$ 上的有界函数,将$\Omega$任意分成n个小闭区域

$$\Delta v_1,\Delta v_2,\cdots,\Delta v_n$$

其中$\Delta v_i$表示第$i$个小闭区域,也表示他的体积。在$\Delta v_i$上任取一点$(\xi_i,\eta_i,\zeta_i)$,作乘积$f(\xi_i,\eta_i,\zeta_i)\Delta v_i(i=1,2,\cdots,n)$,并作和$\sum_{i=1}^n f(\xi_i,\eta_i,\zeta_i)\Delta v_i$。如果当各小闭区域的直径中的最大值$\lambda$趋于零时,这和的极限总存在,则称此极限为函数$f(x,y,z)$在闭区域$\Omega$上的三重积分,记作$\iiint_\Omega f(x,y,z)dv$,即

$$\iiint_\Omega f(x,y,z)dv=\lim_{\lambda\to 0}\sum_{i=1}^n f(\xi_i,\eta_i,\zeta_i)\Delta v_i$$

$dv$叫体积元素。

在直角坐标系中,如果用平行于坐标面的平面来划分$\Omega$,那么除了包含$\Omega$的边界点的一些不规则小闭区域外,得到的小闭区域$\Delta v_i$为长方体。设长方体小闭区域$\Delta v_i$的边长为$\Delta x_j,\Delta y_k,\Delta z_l$则$\Delta v_i=\Delta x_j\cdot \Delta y_k\cdot \Delta z_l$.因此在直角坐标系中，有时也把体积元素 $dv$ 记作 $dxdydz$,而把三重积分记作

$$\iiint_\Omega f(x,y,z)dxdydz$$

### 三重积分的计算

$$\iiint_\Omega f(x,y,z)dxdydz=\int_a^b dx\int_{y_1(x)}^{y_2(x)}\int_{z_1(x,y)}^{z_2(x,y)} f(x,y)dy$$

### 利用柱面坐标计算三重积分

### 利用球面坐标计算三重积分

