# 三角函数

## 三角函数的单位圆定义

三角函数的**单位圆定义**适用于**任意角**（正角、负角、零角、大于$2\pi$的角），是摆脱直角三角形锐角限制的通用定义方式。

**单位圆的设定**

在平面直角坐标系$xOy$中，以原点$O$为圆心，作半径$r=1$的圆，这个圆就是**单位圆**。
取任意角$\theta$，规定其**始边**与$x$轴正半轴重合，**终边**按逆时针方向旋转时$\theta$为正角，顺时针方向旋转时$\theta$为负角；角$\theta$的终边与单位圆的交点记为$P(x,y)$。

**六大三角函数的定义**

基于交点$P(x,y)$的坐标，直接定义六个三角函数，同时明确对应的定义域限制：
- 正弦函数：$\sin\theta = y$，定义域为$\theta \in \mathbb{R}$
- 余弦函数：$\cos\theta = x$，定义域为$\theta \in \mathbb{R}$
- 正切函数：$\tan\theta = \frac{y}{x}$，定义域为$\theta \neq k\pi+\frac{\pi}{2}$（$k\in\mathbb{Z}$），限制条件的本质是$x\neq0$
- 余切函数：$\cot\theta = \frac{x}{y}$，定义域为$\theta \neq k\pi$（$k\in\mathbb{Z}$），限制条件的本质是$y\neq0$
- 正割函数：$\sec\theta = \frac{1}{x}$，定义域为$\theta \neq k\pi+\frac{\pi}{2}$（$k\in\mathbb{Z}$），限制条件的本质是$x\neq0$
- 余割函数：$\csc\theta = \frac{1}{y}$，定义域为$\theta \neq k\pi$（$k\in\mathbb{Z}$），限制条件的本质是$y\neq0$

**核心性质推导（基于单位圆定义）**

1.  **平方关系**
    单位圆的方程为$x^2+y^2=1$，将三角函数定义代入方程，可得核心平方恒等式：$\sin^2\theta+\cos^2\theta=1$。
    在此基础上，两边同时除以$\cos^2\theta$（$\cos\theta\neq0$），可推导得到$1+\tan^2\theta=\sec^2\theta$；
    两边同时除以$\sin^2\theta$（$\sin\theta\neq0$），可推导得到$1+\cot^2\theta=\csc^2\theta$。

2.  **符号规律**
    三角函数的符号由角$\theta$终边所在的象限决定，本质是交点$P(x,y)$的坐标符号：
    - 第一象限（$2k\pi<\theta<2k\pi+\frac{\pi}{2}$，$k\in\mathbb{Z}$）：$x>0$且$y>0$，所有三角函数的值均为正
    - 第二象限（$2k\pi+\frac{\pi}{2}<\theta<2k\pi+\pi$，$k\in\mathbb{Z}$）：$x<0$且$y>0$，仅有$\sin\theta$和$\csc\theta$的值为正，其余函数值为负
    - 第三象限（$2k\pi+\pi<\theta<2k\pi+\frac{3\pi}{2}$，$k\in\mathbb{Z}$）：$x<0$且$y<0$，仅有$\tan\theta$和$\cot\theta$的值为正，其余函数值为负
    - 第四象限（$2k\pi+\frac{3\pi}{2}<\theta<2k\pi+2\pi$，$k\in\mathbb{Z}$）：$x>0$且$y<0$，仅有$\cos\theta$和$\sec\theta$的值为正，其余函数值为负

3.  **周期性**
    当角$\theta$增加$2k\pi$（$k\in\mathbb{Z}$）时，终边会与原终边完全重合，交点$P(x,y)$的坐标不变，因此$\sin(\theta+2k\pi)=\sin\theta$，$\cos(\theta+2k\pi)=\cos\theta$，$\sec(\theta+2k\pi)=\sec\theta$，$\csc(\theta+2k\pi)=\csc\theta$，这四个函数的最小正周期为$2\pi$。
    当角$\theta$增加$k\pi$（$k\in\mathbb{Z}$）时，终边会关于原点对称，交点$P(x,y)$变为$P(-x,-y)$，代入正切、余切的定义可得$\tan(\theta+k\pi)=\frac{-y}{-x}=\frac{y}{x}=\tan\theta$，$\cot(\theta+k\pi)=\frac{-x}{-y}=\frac{x}{y}=\cot\theta$，因此这两个函数的最小正周期为$\pi$。
