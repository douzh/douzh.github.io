# 多元函数微分

## 定义

### n维空间


设 n 为取定的一个正整数,我们用 $R^n$表示 n 元有序实数组$(x_1,x_2,...,x_n)$的全体所构成的集合,即

$R^n=R \times R \times R...\times R = \{(x_1,x_2,...,x_n)\|x_i \in R^n,i=1,2,...,n\}$

$x=(x_1,x_2,...,x_n),y=(y_1,y_2,...,y_n)$为$R^n$中任意两点

$$x+y=(x_1+y_1,x_2+y_2,...,x_n+y_n)$$

$$\lambda x=(x_1\lambda,x_2\lambda,...,x_n\lambda)$$

x和y的距离y记做$\rho (x,y)$，$\rho(x,0)$记作$\|\|x\|\|$(在$R^1,R^2,R^3$中,$\rho(x,0)$通常记作$\|x\|$)

$$\rho(x,y)=||x-y||=\sqrt{(x_1-y_1)^2+(x_2-y_2)^2+...+(x_n-y_n)^2}$$

$$\rho(x,0)=||x||=\sqrt{x_1^2+x_2^2+...+x_n^2}$$

$x=(x_1,x_2,...,x_n),a=(a_1,a_2,...,a_n) \in R^n$，如果$\|\|x-a\|\|\to 0$，则称变元x在$R^n$中趋于固元a,记为$x \to a$

### 多元函数

定义1 设D是$R^2$的一个非空子集,称映射$f:D\to R$为定义在 D 上的二元函数,通常记为

$$z=f(x,y),(x,y)\in D$$

或

$$z=f(P),P \to D$$

其中点集 D 称为该函数的定义域，x、y称为自变量z称为因变

多元函数定义类似

### 多元函数的极限

定义2 设二元函数$f(P)=f(x,y)$的定义域为 D,$P_0(x_0,y_0)$是 D 的聚点.如果存在常数A,对于任意给定的正数$\varepsilon$,总存在正数$\delta$,使得当点$P(z,y) \in D\cap U(P_0,\delta)$时都有

$$|f(P)-A|=|f(x,y)-A|<\varepsilon$$

成立，那么就称常数A为$f(x,y)$在$P_0$的极限，记为

$$\lim\limits_{(x,y) \to (x_0,y_0)}f(x,y)=A 或 f(x,y)\to A((x,y) \to (x_0,y_0))$$

也记作

$$\lim_{P \to P_0}f(P)=A 或 f(P)\to A(P\to P_0)$$

## 多元函数的连续性

定义3 设二元函数$f(P)= f(x,y)$的定义域为 D,$P_0(x_0,y_0)$为 D 的聚点,且$P \in D$.如果

$$\lim\limits_{(x,y) \to (x_0,y_0)}f(x,y)=f(x_0,y_0)$$

那么称$f(x,y)$在$P_0(x_0,y_0)$连续.

定义4 设函数$f(x,y)$的定义域为 D,$P_0(x_0，y_0)$是 D 的聚点.如果函数$f(x,y)$在点$P_0(x_0，y_0)$不连续,则称$P_0(x_0，y_0)$为函数$f(x,y)$的间断点

## 偏导数

定义 设函数 $z=f(x,y)$在点$(z_0,y_0)$的某一邻域内有定义,当$y$固定在$y_0$而$x$在$x_0$处有增量$\Delta x$时,相应的函数有增量

$$f(x_0+\Delta x,y_0)-f(x_0,y_0)$$

如果

$$\lim\limits_{\Delta x \to 0} \frac{f(x_0+\Delta x,y_0)-f(x_0,y_0)}{\Delta x}$$

存在, 则称些极限为函数$z=f(x,y)$在点$(x_0,y_0)$处对x的偏导数,记为

$$\frac{\alpha z}{\alpha x}|_{x=x_0,y=y_0},\frac{\alpha f}{\alpha x}|_{x=x_0,y=y_0},z_x|_{x=x_0,y=y_0} 或 f_x(x_0,y_0)$$

类似的，函数$z=f(x,y)$在点$(x_0,y_0)$处对y的偏导数定义为

$$\lim\limits_{\Delta y \to 0} \frac{f(x_0,y_0+\Delta y)-f(x_0,y_0)}{\Delta y}$$

记作

$$\frac{\alpha z}{\alpha y}|_{x=x_0,y=y_0},\frac{\alpha f}{\alpha y}|_{x=x_0,y=y_0},z_y|_{x=x_0,y=y_0} 或 f_y(x_0,y_0)$$

### 高阶偏导数

$$\frac{\alpha }{\alpha x}(\frac{\alpha z}{\alpha x})=\frac{\alpha^2z}{\alpha x^2}=f_{xx}=(x,y)$$

$$\frac{\alpha }{\alpha y}(\frac{\alpha z}{\alpha y})=\frac{\alpha^2z}{\alpha y^2}=f_{yy}=(x,y)$$

$$\frac{\alpha }{\alpha x}(\frac{\alpha z}{\alpha y})=\frac{\alpha^2z}{\alpha y\alpha x}=f_{yx}=(x,y)$$

$$\frac{\alpha }{\alpha y}(\frac{\alpha z}{\alpha x})=\frac{\alpha^2z}{\alpha x\alpha y}=f_{xy}=(x,y)$$

定理 如果函数 $z=f(x，y)$的两个二阶混合偏导数$\frac{\alpha^2z}{\alpha x\alpha y}$及$\frac{\alpha^2z}{\alpha y\alpha x}$在区域D内连续，那么在该区域内这两个二阶混合偏导数必相等

对于二元以上的函数,也可以类似地定义高阶偏导数.而且高阶混合偏导数在偏导数连续的条件下也与求导的次序无关

### 拉普拉斯方程

拉普拉斯(Laplace)方程

$$\frac{\alpha^2z}{\alpha x^2}+\frac{\alpha^2z}{\alpha y^2}=0$$

$$\frac{\alpha^2z}{\alpha x^2}+\frac{\alpha^2z}{\alpha y^2}+\frac{\alpha^2z}{\alpha z^2}=0$$

## 全微分

定义 设函数 $z=f(x,y)$在点$(x，y)$的某邻域内有定义,如果函数在点$(x，y)$的全增量

$$\Delta z=f(x+\Delta x,y+\Delta y)-f(x,y)$$

可表示为

$$\Delta z=A\Delta x+B\Delta y+o(\rho)$$

其中A、B不依赖于$\Delta x、\Delta y$而且仅与$x、y$有关,$\rho=\sqrt{\Delta x^2+\Delta y^2}$,则称函数$z=f(x,y)$在点$(x，y)$可微分，而$A\Delta x+B\Delta y$称为函数$z=f(x,y)$在点$(x，y)$的全微分,记作$dz$，即

$$dz=A\Delta x+B\Delta y$$

如果函数在区域 D 内各点处都可微分,那么称这函数在 D 内可微分

定理1(必要条件) 如果函数 $z=f(x,y)$在点$(x，y)$可微分,则该函数在点$(x,y)$的偏导数$\frac{\alpha^2z}{\alpha x^2}、\frac{\alpha^2z}{\alpha y^2}$必定存在，且函数 $z=f(x,y)$在点$(x，y)$的全微分为

$$dz=\frac{\alpha^2z}{\alpha x^2}\Delta x+\frac{\alpha^2z}{\alpha y^2}\Delta y$$

定理 2(充分条件) 如果函数 $z=f(x,y)$的偏导数$\frac{\alpha^2z}{\alpha x^2}、\frac{\alpha^2z}{\alpha y^2}$点$(x，y)$连续，则该函数在该点可微分。

## 多元复合函数求导

## 隐函数求导

## 方向导数与梯度

### 方向导数

偏导数反映的是函数沿坐标轴方向的变化率.但许多物理现象告诉我们,只考虑函数沿坐标轴方向的变化率是不够的.因此我们有必要来讨论函数沿任一指定方向的变化率问题

定理 如果函数$f(x,y)$在点 $P_0(x_0,y_0)$可微分,那么函数在该点沿任一方向$l$的方向导数存在,且有

$$\frac{\alpha f}{\alpha l}|_{(x_0,y_0)}=f_x(x_0,y_0)cos\alpha+f_y(x_0,y_0)sin\beta$$

其中$cos\alpha、cos\beta$是方向$l$的方向余弦。

### 梯度

函数$f(x,y)$在点 $P_0(x_0,y_0)$的梯度

$$f_x(x_0,y_0)\vec i+f_y(x_0,y_0)\vec j$$

记作$\vec{grad}f(x_0,y_0)$或$\nabla f(x_0,y_0)$$即

$$\vec{grad}f(x_0,y_0)=\nabla f(x_0,y_0)=f_x(x_0,y_0)\vec i+f_y(x_0,y_0)\vec j$$

其中$\nabla=\frac{\alpha}{\alpha x}\vec i+\frac{\alpha}{\alpha y}\vec j$称为(二维的)向量微分算子或Nabla算子,$\nabla f=\frac{\alpha f}{\alpha x}\vec i+\frac{\alpha f}{\alpha y}\vec j$

$\vec e_i=(cos\alpha,cos\beta)$是与方向$l$同向的单位向量，则

$$\frac{\alpha f}{\alpha l}|_{(x_0,y_0)}=f_x(x_0,y_0)cos\alpha+f_y(x_0,y_0)sin\beta\\ =\vec{grad}f(x_0,y_0)\vec e_i=|\vec{grad}f(x_0,y_0)|cos\theta$$

其中$\theta=(\widehat{\vec{grad}f(x_0,y_0),\vec e_i})$

这一关系式表明了函数在一点的梯度与函数在这点的方向导数间的关系

## 多元函数的极值

## 二元函数的泰勒公式

## 最小二积法
