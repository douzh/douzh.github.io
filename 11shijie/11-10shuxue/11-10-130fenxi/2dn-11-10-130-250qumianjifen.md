# 曲线曲面积分

## 弧长曲线积分计算

定义 设L为xOy面内的一条光滑曲线驱,函数 $f(x,y)$在L上有界.在L上任意插入一点列$M_1,M_2,\cdots,M_{n-1}$把L分成n个小段设第个$i$小段的长度为$\Delta s_i$. 又$(\xi_i,\eta_i)$为第个小段上任意取定的一点, 作乘积 $f(\xi_i,\eta_i)\Delta s_i(i=1,2,\cdots,n)$, 并作和$\sum_{i=1}^n f(\xi_i,\eta_i)\Delta s_i$,如果当各小弧段的长度的最大值$\lambda \to0$, 这和的极限总存在,则称此极限为函数 $f(x,y)$在曲线弧L上对弧长的曲线积分或第一类曲线积分,记作$\int_Lf(x,y)ds$,即

$$\int_Lf(x,y)ds=\lim_{\lambda\to 0}\sum_{i=1}^n f(\xi_i,\eta_i)\Delta s_i$$

其中$f(x,y)$叫做被积函数, L叫做积分弧段。

**空间曲线**

$$\int_\Gamma f(x,y,z)ds=\lim_{\lambda\to 0}\sum_{i=1}^n f(\xi_i,\eta_i,\zeta_i)\Delta s_i$$

如果$L=L_1+L_2$

如果L是闭曲线，曲线积分记作为$\oint_Lf(x,y)ds$。

**性质**

性质1 设$\alpha,\beta$为常数，则

$$\int_L [\alpha f(x,y)+\beta g(x,y)]ds=\alpha\int_Lf(x,y)ds+\beta\int_Lg(x,y)ds$$

性质2 若积分弧段L可分成两段光滑的曲线弧$L_1,L_2$，则

$$\int_{L_1+L_2}f(x,y)ds=\int_{L_1}f(x,y)ds+\int_{L_2}f(x,y)ds$$

性质3 设在L上$f(x,y)\le g(x,y)$，则

$$\int_Lf(x,y)ds\le\int_Lg(x,y)ds$$

特别的有

$$|\int_Lf(x,y)ds|\le\int_L|g(x,y)|ds\$$

**计算**

定理 设$f(x,y)$在曲线弧L上有定义且连续, L 的参数方程为
$$
 \begin{cases}
 x=\varphi(t)  (\alpha<t<\beta)\\
 y=\psi(t)
 \end{cases}
$$

其中 $\varphi(t),\psi(t)$ 在$[\alpha,\beta]$上具有一阶连续导数，且$\varphi^{'2}(t)+\psi^{'2}(t)\ne 0$，则曲线积分$\int_Lf(x,y)ds$存在且

$$\int_Lf(x,y)ds=\int_{\alpha}^{\beta}f[\varphi(t),\psi(t)]\sqrt{\varphi^{'2}(t)+\psi^{'2}(t)}dt$$

## 对坐标的曲线积分

定义 设L为xOy面内从点 A 到点B 的一条有向光滑曲线弧，函数$P(x,y)Q(z,y)$在 L 上有界. 在 L 上沿L 的方向任意插入一点列 $M(x_1,y_1),M,(x_2,y_2),\cdots,M_{n-1}(x_{n-1},y_{n-1})$,把L分成n 个有向小弧段

$$\widehat{M_{i-1}M_i}(i=1,2,\cdots,n-1;M_0=A,M_n=B)$$

设$\Delta x_i=x_i-x_{i-1}$，$\Delta y_i=y_i-y_{i-1}$，点$(\xi_i,\eta_i)$为$\widehat{M_{i-1}M_i}$上任取一点，如果当各小弧段长度的最大$\lambda \to 0$时，$sum_{i=1}^n P(\xi_i,\eta_i)\Delta x_i$的极限总存在, 则称此极限为函数 $P(x,y)$在有向曲线弧 L 上对坐标$x$的曲线积分, 记作 $\int_L P(x,y)dx$. 类似地,如果$sum_{i=1}^n P(\xi_i,\eta_i)\Delta y_i$总存在,则称此极限为函数 $Q(x,y)$在有向曲线弧 L 上对坐标$y$的曲线积分,记作$\int_L Q(x,y)dy$. 即 

$$\int_L P(x,y)dx=\lim_{\lambda\to 0}\sum_{i=1}^n P(\xi_i,\eta_i)\Delta x_i$$

$$\int_L Q(x,y)dy=\lim_{\lambda\to 0}\sum_{i=1}^n Q(\xi_i,\eta_i)\Delta y_i$$

其中$P(x,y)$和$Q(x,y)$分别称为被积函数，L称为积分弧段。

以上两个积分也称为第二类曲线积分

**空间曲线**

$$\int_\Gamma P(x,y,z)dx=\lim_{\lambda\to 0}\sum_{i=1}^n P(\xi_i,\eta_i,\zeta_i\Delta x_i$$

$$\int_\Gamma Q(x,y,z)dy=\lim_{\lambda\to 0}\sum_{i=1}^n Q(\xi_i,\eta_i,\zeta_i)\Delta y_i$$

$$\int_\Gamma R(x,y,z)dx=\lim_{\lambda\to 0}\sum_{i=1}^n R(\xi_i,\eta_i,\zeta_i)\Delta x_i$$

**简化写法**

$$\int_L P(x,y)dx+\int_L Q(x,y)dy$$

$$\int_L P(x,y)dx+ Q(x,y)dy$$

$$\int_L \vec F(x,y)d\vec r$$

其中$\vec F=P(x,y)\vec i+Q(x,y)\vec j$为向量值函数，$d\vec r=dx\vec i+dy\vec j$.

类似的

$$\int_\Gamma P(x,y,z)dx+\int_\Gamma Q(x,y,z)dy+\int_\Gamma R(x,y,z)dz$$

$$\int_\Gamma P(x,y,z)dx+ Q(x,y,z)dy+ R(x,y,z)dz$$

$$\int_\Gamma \vec A(x,y,z)d\vec r$$

其中$\vec A=P(x,y,z)\vec i+Q(x,y,z)\vec j+R(x,y,z)\vec k$为向量值函数，$d\vec r=dx\vec i+dy\vec j+dz\vec k$.

**性质**

性质1 设$\alpha,\beta$为常数，则

$$\int_L [\alpha \vec F_1(x,y)+\beta \vec F_2(x,y)]d\vec r=\alpha\int_L\vec F_1(x,y)d\vec r+\beta\int_L\vec F_2(x,y)d\vec r$$

性质2 若积分弧段L可分成两段光滑的曲线弧$L_1,L_2$，则

$$\int_L \vec F(x,y)d\vec r=\int_{L_1}\vec F(x,y)d\vec r+\int_{L_2}\vec F(x,y)d\vec r$$

性质3 设L是有几光滑曲线弧，$L^-$是$L$的反向曲线弧，则

$$\int_{L^-} \vec F(x,y)d\vec r=-\int_L\vec F(x,y)d\vec r$$

**计算**

定理 设$P(x,y)、Q(x,y)$在有向曲线弧L上有定义且连续, L 的参数方程为
$$
 \begin{cases}
 x=\varphi(t) \\
 y=\psi(t)
 \end{cases}
$$

当参数t单调的由$\alpha$变到$\beta$时，点$M(x,y)$从L的起点A沿L运动到终点B,  $\varphi(t),\psi(t)$ 在$[\alpha,\beta]$上具有一阶连续导数，且$\varphi^{'2}(t)+\psi^{'2}(t)\ne 0$，则曲线积分$\int_LP(x,y)ds+Q(x,y)dy$存在,且

$$\int_L P(x,y)dx+ Q(x,y)dy=\int_{\alpha}^{\beta} |P[\varphi(t),\psi(t)]\varphi'(t)+Q[\varphi(t),\psi(t)]\psi'(t)|dt$$

## 格林公式

定理1 设闭区域 D 由分段光滑的曲线L围成函数 $P(x,y)$及$Q(x,y)$在 D 上具有一阶连续偏导数,则有

$$\iint_D (\frac{\alpha Q}{\alpha x}-\frac {\alpha P}{\alpha y})dydy=\oint_L Pdx+Qdy$$

其中L是D 的取正向的边界曲线.

定理2 设区域 G 是一个单连通域,函数 $P(x,y),Q(x,y)$在 G 内具有阶连续偏导数,则曲线积分$\int_L Pdx+Qdy$在G 内与路径无关(或沿 G 内任意闭曲线的曲线积分为零)的充分必要条件是

$$\frac{\alpha Q}{\alpha x}=\frac {\alpha P}{\alpha y}$$

在G内恒成立

定理3 设区域 G 是一个单连通域,函数 $P(x,y)、Q(x,y)$在 G 内具有阶连续偏导数,则 $P(x,y)dx + Q(z,y)dy$在G 内为某一函数 $u(x,y)$的全微分的充分必要条件是

$$\frac{\alpha Q}{\alpha x}=\frac {\alpha P}{\alpha y}$$

在G内恒成立

定理4(曲线积分的基本定理) 设 $\vec F(x,y)= P(x,y)\vec i+ Q(x,y)\vec j$ 是平面区域G 内的一个向量场,$P(x,y)Q(x,y)$都在 G 内连续,且存在一个数量函数$f(x,y)$使得 $F=\nabla f$,则曲线积分$\int_L \vec F\cdot d\vec r$在G 内与路径无关,且

$$\int_L \vec F\cdot d\vec r=f(B)-f(A)$$

其中 L 是位于 G 内起点为 A、终点为 B 的任一分段光滑曲线

## 对面积的曲面积分

定义 设曲面$\varSigma$是光滑的,函数 $f(x,y,z)$在$\varSigma$上有界.把$\varSigma$分成n小块$\Delta S_i$($\Delta S_i$同时也代表第i小块曲面的面积), 设$(\xi_i,\eta_i,\zeta_i)$为$\Delta S_i$上任意取定的一点, 作乘积 $f(\xi_i,\eta_i,\zeta_i)\Delta S_i(i=1,2,\cdots,n)$, 并作和$\sum_{i=1}^n f(\xi_i,\eta_i,\zeta_i)\Delta S_i$,如果当各小块曲面直径的最大值$\lambda \to0$, 这和的极限总存在,则称此极限为函数 $f(x,y,z)$在曲面$\varSigma$上对面积的曲面积分或第一类曲面积分,记作$\iint_\varSigma f(x,y)dS$,即

$$\int_\varSigma f(x,y,x)dS=\lim_{\lambda\to 0}\sum_{i=1}^n f(\xi_i,\eta_i,\zeta_i)\Delta S_i$$

其中$f(x,y,z)$叫做被积函数, $\varSigma$叫做积分曲面。

**计算**

$$\int_\varSigma f(x,y,x)dS=\iint_{D_{xy}}f[x,y,z(x,y)]\sqrt{1+z_x^2(x,y)+z_y^2(x,y)}dxdy$$

## 对坐标的曲面积分

定义 设$\varSigma$  为光滑的有向曲面,函数 $R(x,y,z)$在$\varSigma$上有界. 把$\varSigma$任意分成n 块小曲面$\Delta S_i$($\Delta S_i$同时也代表第i小块曲面的面积),$\Delta S_i$在$xOy$ 面上的投影为$(\Delta S_i)_{xy}$，$(\xi_i,\eta_i,\zeta_i)$是$\Delta S_i$上任意取定的一点.如果当各小块曲面的直径的最大值$\lambda \to0$时,

$$\lim_{\lambda\to 0}\sum_{i=1}^n R(\xi_i,\eta_i,\zeta_i)(\Delta S_i)_{xy}$$

总存在,则称此极限为函数 $R(x,y,z)$在有向曲面$\varSigma$ 上对坐标x、y的曲面积分,记作$\iint_{\varSigma}R(x,y,z)dxdy$,即

$$\iint_{\varSigma}R(x,y,z)dxdy=\lim_{\lambda\to 0}\sum_{i=1}^n R(\xi_i,\eta_i,\zeta_i)(\Delta S_i)_{xy}$$

其中 $R(x,y,z)$叫做被积函数,$\varSigma$做积分曲面.

类似的

$$\iint_{\varSigma}P(x,y,z)dydz=\lim_{\lambda\to 0}\sum_{i=1}^n P(\xi_i,\eta_i,\zeta_i)(\Delta S_i)_{yz}$$

$$\iint_{\varSigma}Q(x,y,z)dzdx=\lim_{\lambda\to 0}\sum_{i=1}^n Q(\xi_i,\eta_i,\zeta_i)(\Delta S_i)_{zx}$$

以上三个曲面积分也称为第二类曲面积分

**计算**

$$\iint_{\varSigma}R(x,y,z)dxdy=\iint_{D_{xy}}R[x,y,z(x,y)]dxdy$$

## 高斯公式

定理1 设空间闭区域$\Omega$是由分片光滑的闭曲面$\Sigma$所围成，函数$P(x,y,z)、Q(x,y,z)、R(x,y,z)$在$\Omega$上具有一阶连续偏导数,则有

$$\iiint_{\Omega} (\frac{\alpha P}{\alpha x}+\frac {\alpha Q}{\alpha y}+\frac {\alpha R}{\alpha z})dv= \oiint_{\Sigma} Pdydz+Qdzdx+Rdxdy$$

或

$$\iiint_{\Omega} (\frac{\alpha P}{\alpha x}+\frac {\alpha Q}{\alpha y}+\frac {\alpha R}{\alpha z})dv= \oiint_{\Sigma} (Pcos\alpha+Qcos\beta+R\gamma)dS$$

这里$\Sigma$是$\Omega$整个边界曲面的外侧,$cos\alpha 、cos\beta、 cos\gamma$是在点$(x,y,z)$处的法向量的方向余弦.

定理2 设 G 是空间二维单连通区域,$P(x,y,z)、Q(x,y,z)、R(x,y,z)$在 G 内具有一阶连续偏导数,则曲面积分

$$\iint_{\varSigma} Pdydz+Qdzdx+Rdxdy$$

在 G 内与所取曲面$\varSigma$无关而只取决于$\varSigma$ 的边界曲线(或沿 G 内任一闭曲面的曲面积分为要)的充分必要条件是

$$\frac{\alpha P}{\alpha x}+\frac {\alpha Q}{\alpha y}+\frac {\alpha R}{\alpha z}=0$$

在G内恒成立.

## 通量与散度

设有向量场

$$\vec A(x,y,z)=P(x,y,z)\vec i+Q(x,y,z)\vec j+R(x,y,z)\vec k$$

其中函数 P、Q、R 均具有一阶连续偏导数, $\Sigma$是场内的一片有向曲面,$\vec n$ 是$\Sigma$在点$(x,y,z)$处的单位法向量,则积分

$$\iint_{\Sigma} \vec A\cdot \vec ndS$$

称为向量场$\vec A$通过曲面$\Sigma$向着指定侧的通量(或流量)。

$$\iint_{\Sigma} \vec A\cdot \vec ndS=\iint_{\Sigma} \vec A\cdot d\vec S=\iint_{\varSigma} Pdydz+Qdzdx+Rdxdy$$

## 斯托克斯公式

定理1 设$\Gamma$为分段光滑的空间有向闭曲线, $\Sigma$是以$\Gamma$为边界的分片光滑的有向曲面,$\Gamma$的正向与$\Sigma$的侧符合右手规则,函数 $P(x,y,z)、Q(x,y,z)、R(x，y,z)$在曲面$\Sigma$(连同边界$\Gamma$)上具有一阶连续偏导数，则有

$$\iint_{\Sigma} (\frac{\alpha R}{\alpha y}-\frac {\alpha Q}{\alpha z})dydz+(\frac{\alpha P}{\alpha z}-\frac {\alpha R}{\alpha x})dzdx+(\frac{\alpha Q}{\alpha x}+\frac {\alpha P}{\alpha y})dxdy=\oint_{\Gamma} Pdx+Qdy+Rdz$$

$$\iint_\Sigma \begin{vmatrix}
cos\alpha & cos\beta & cos\gamma\\
\frac{\alpha}{dx} & \frac{\alpha}{dy} &  \frac{\alpha}{dz}\\
P & Q & R
\end{vmatrix} =\oint_\Gamma Pdx+Qdy+Rdz$$

其中$\vec n=(cos\alpha , cos\beta , cos\gamma)$为有向曲面$\Sigma$在点$(x,y,z)$处的单位法向量。

## 环流量与旋度

设有向量场

$$\vec A(x,y,z)=P(x,y,z)\vec i+Q(x,y,z)\vec j+R(x,y,z)\vec k$$

其中函数 P、Q、R 均连续，$\Gamma$是$\vec A$ 的定义域内的一条分段光滑的有向闭曲线，$\tau$是$\Gamma$在点$(x,y,z)$处的单位切向量,则积分

$$\oint_{\Gamma} \vec A\cdot \vec \tau ds$$

称为向量场 $\vec A$ 沿有向闭曲线I的环流量

$$\oint_{\Gamma} \vec A\cdot \vec \tau ds=\oint_{\Gamma} \vec A\cdot  d\vec r=\oint_{\Gamma} Pdx+Qdy+Rdz$$


**旋度**

设有向量场

$$\vec A(x,y,z)=P(x,y,z)\vec i+Q(x,y,z)\vec j+R(x,y,z)\vec k$$

其中函数 P、Q、R 均具有一阶连续偏导数, 则向量


$$(\frac{\alpha R}{\alpha y}-\frac {\alpha Q}{\alpha z})\vec i+(\frac{\alpha P}{\alpha z}-\frac {\alpha R}{\alpha x})\vec j+(\frac{\alpha Q}{\alpha x}+\frac {\alpha P}{\alpha y})\vec k$$

称为向量场 $\vec A$ 的旋度,记作 $rot A$即

$$rot A=(\frac{\alpha R}{\alpha y}-\frac {\alpha Q}{\alpha z})\vec i+(\frac{\alpha P}{\alpha z}-\frac {\alpha R}{\alpha x})\vec j+(\frac{\alpha Q}{\alpha x}+\frac {\alpha P}{\alpha y})\vec k$$

利用向量微分算子$\nabla$,向量场 $\vec A$ 的旋度 $rot A$ 可表示为$\nabla \times \vec A$，即

$$rot A=\nabla \times \vec A=\begin{vmatrix}
\vec i & \vec j & \vec k\\
\frac{\alpha}{dx} & \frac{\alpha}{dy} &  \frac{\alpha}{dz}\\
P & Q & R
\end{vmatrix} $$

如果向量场 $\vec A$ 的旋度 $rot A$ 处处为零则称向量场 $A$ 为无旋场. 而一个无源、无旋的向量场称为调和场. 调和场是物理学中另一类重要的向量场,这种场与调和函数有密切的关系.