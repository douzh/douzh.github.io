# 定积分

## 定义

定义 设函数 $f(x)$在$[a,b]$上有界,在$[a,b]$中任意插入若千个分点

$$a=x_0<x_1<x_2...<x_{n-1}<x_n=b$$

把区间$[a,b]$分成 $n$ 个小区间

$$[x_0,x_1],[x_1,x_2],...,[x_{n-1},x_n]$$

各个小区间的长度依次为

$$\Delta x_1=x_1-x_0,\Delta x_2=x_2-x_1,...,\Delta x_n=x_n-x_{n-1}$$

在每个小区间$[x_{i-1},x_i]$上任取一点$\xi_i(x_{i-1}<\xi_i<x_i)$,作为函数值$f(\xi_i)$与小区间长度$\Delta x_i$的乘积$f(\xi_i)\Delta x_i(i= 1,2,...,n)$ 并作出和

$$S=\sum_{i=1}^{n}f(\xi_i)\Delta x_i$$

记$\lambda=max\{\Delta x_1,\Delta x_2,...,\Delta x_n\}$，如果不论对$[a,b]$怎样划分,也不论在小区间$[x_{i-1},x_i]$上点怎样选取,只要当$\lambda \to 0$时,和$S$总趋于确定的极限$I$,那么称这个极限 $I$为函数$f(x)$在区间$[a,b]$上的定积分(简称积分),记作$\int_a^b f(x)dx$，即

$$\int_a^b f(x)dx=I=\lim\limits_{\lambda \to 0}\sum_{i=1}^{n}f(\xi_i)\Delta x_i$$

其中$f(x)$叫做被积函,$f(x)dx$叫做被积表达式, $x$做积分变量，$a$做积分下限, $b$叫做积分上限,$[a,b]$叫做积分区间。

定理1 设$f(x)$在区间$[a,b]$上连续,则$f(x)$在$[a,0]$上可积。

定理2 设$f(x)$在区间$[a,b]$上有界,且只有有限个间断点,则$f(x)$在$[a,b]$上可积。

## 性质

性质1

$$\int_a^b [f(x)+g(x)]dx=\int_a^b f(x)dx+ \int_a^b g(x)dx$$

性质2

$$\int_a^b kf(x)dx=k\int_a^b f(x)dx$$

性质3 设 $a<c<b$

$$\int_a^b f(x)dx=\int_a^c f(x)dx+ \int_c^b g(x)dx$$

性质4 如果在区间$[a,b]$上 $f(x)=1$,则

$$\int_a^b 1dx=k\int_a^b dx=b-a$$

性质5 如果在区间$[a,b]$上,$f(x) \ge 0$,则

$$\int_a^b f(x)dx \ge 0$$

推论1 如果在区间$[a,b]$上，$f(x) \le g(x)$,则

$$\int_a^b f(x)dx \le \int_a^b g(x)dx$$

推论2 

$$|\int_a^b f(x)dx| \le \int_a^b |f(x)|dx$$

性质6 设 $M$ 及$m$分别是函数$f(x)$在区间$[a,b]$上的最大值及最小值

$$m(b-a) \le \int_a^b f(x)dx \le M(b-a)$$

## 微积分基本公式

定理1 如果函数$f(x)$在区间$[a,b]$上连续,则积分上限的函数

$$\Phi(x)=\int_a^x f(t)dt$$

在$[a,b]$上可导，并且它的导数

$$\Phi'(x)=\frac{d}{dx}\int_a^x f(t)dt=f(x)$$

定理2 如果函数$f(x)$在区间$[a,b]$上连续,则函数

$$\Phi(x)=\int_a^x f(t)dt$$

就是函数$f(x)$在$[a,b]$上的一个原函数。

**牛顿-莱布尼茨公式**

定理3 如果函数 $F(x)$是连续函数 $f(x)$在区间$[a,b]$上的一个原函数，则

$$\int_a^b f(x)dx=F(b)-F(a)=[F(x)]_a^b$$

**一个连续函数在区间$[a,b]$上的定积分等于它的任一个原函数在区间$[a,b]$上的增量。**

## 反常积分

定义1 设函数$f(x)$在区间$[a,+\infty)$上连续,取$t>a$,如果极限

$$\lim\limits_{t \to +\infty}\int_a^tf(x)$$

存在,则称此极限为函数$f(x)$在无穷区间$[a，+\infty)$上的反常积分,记作$\int_a^{+\infty} f(x)dx$，即

$$\int_a^{+\infty} f(x)dx=\lim\limits_{t \to +\infty}\int_a^tf(x)dx$$

这时也称反常积分$\int_a^{+\infty} f(x)dx$收敛;如果上述极限不存在,则函数在无穷区间$[a,+\infty)$上的反常积分$\int_a^{+\infty} f(x)dx$就没有意义，习惯上称为反常积分$\int_a^{+\infty} f(x)dx$发散，这时记号为$\int_a^{+\infty} f(x)dx$不再表示数值了

类似的，设函数$f(x)$在区间$[a,+\infty)$上连续,取$t<a$,如果极限

$$\lim\limits_{t \to -\infty}\int_a^tf(x)$$

存在,则称此极限为函数$f(x)$在无穷区间$[a，-\infty)$上的反常积分,记作$\int_a^{-\infty} f(x)dx$，即

$$\int_a^{-\infty} f(x)dx=\lim\limits_{t \to -\infty}\int_a^tf(x)dx$$

这时也称反常积分$\int_a^{-\infty} f(x)dx$收敛;如果上述极限不存在,则称为反常积分$\int_a^{-\infty} f(x)dx$发散。

设函数$f(x)$在区间$[-\infty,+\infty)$上连续，如果反常积分$\int_a^{-\infty} f(x)dx$和$\int_a^{+\infty} f(x)dx$都收敛，则称上述两反常积分之和为函数$f(x)$在无穷区间$[-\infty,+\infty)$上的反常积分，记作$\int_{-\infty}^{+\infty} f(x)dx$，即

$$\int_{-\infty}^{+\infty} f(x)dx=\int_{-\infty}^0 f(x)dx+\int_0^{+\infty} f(x)dx\\
=\lim\limits_{t \to -\infty}\int_t^0f(x)+\lim\limits_{t \to +\infty}\int_0^tf(x)dx$$

这时也称反常积分$\int_{-\infty}^{+\infty} f(x)dx$收敛，否则就称反常积分$\int_{-\infty}^{+\infty} f(x)dx$发散。

上述反常积分统称为无穷限的反常积分。

**无界函数的反常积分**

如果函数$f(x)$在点 的任一邻域内都无界,那么点$a$称为函数$f(x)$的瑕点(也称为无界间断点).无界函数的反常积分又称为瑕积分

定义2 设函数$f(x)$在区间$(a,b]$上连续，点a为$f(x)$的瑕点，取$t>a$,如果极限

$$\lim\limits_{t \to a^+}\int_t^bf(x)dx$$

存在,则称此极限为函数$f(x)$在区间$(a,+b]$上的反常积分,记作$\int_a^b f(x)dx$，即

$$\int_a^b f(x)dx=\lim\limits_{t \to a^+}\int_t^bf(x)dx$$

这时也称反常积分$\int_a^b f(x)dx$收敛;如果上述极限不存在,则称为反常积分$\int_a^b f(x)dx$发散。

类似地，设函数$f(x)$在区间$[a,b)$上连续，点b为$f(x)$的瑕点，取$t<b$,如果极限

$$\lim\limits_{t \to b^-}\int_a^tf(x)dx$$

存在，则定义

$$\int_a^b f(x)dx=\lim\limits_{t \to b^-}\int_a^tf(x)dx$$

否则就称反常积分$\int_a^b f(x)dx$发散。

设函数$f(x)$在区间$[a,b]$上除c点$(a<c<b)$外连续，点c为$f(x)$的瑕点，如果两个反常积分$\int_a^c f(x)dx$和$\int_c^b f(x)dx$都收敛，则定义

$$\int_a^b f(x)dx=\int_a^c f(x)dx+\int_c^b f(x)dx\\
=\lim\limits_{t \to c^-}\int_a^tf(x)dx+\lim\limits_{t \to c^+}\int_t^bf(x)dx$$

