# 无穷极数

## 概念和性质

## 幂级数

## 函数展开成幂级数

泰勒级数

$$ f(x) = \sum_{n=0}^{\infty} \frac{f^{(n)}(x_0)}{n!} (x-x_0)^n $$

麦克劳林展开式

$$ f(x) = \sum_{n=0}^{\infty} \frac{f^{(n)}(0)}{n!} x^n $$

## 傅立叶级数

$$ f(x) = \frac{a_0}{2} + \sum_{n=1}^{\infty}  a_n \cos nx + b_n \sin nx$$

## 广义牛顿二项式展开

牛顿是著名的数学家和物理学家，我们知道他创立了经典力学，创造了微积分，可是他墓碑上刻着的唯一公式既不是关于牛顿力学，也不是关于微积分，而是大多数人可能并不知晓的一个公式：

$(P+PQ)^{\frac{m}{n}}=P^{\frac{m}{n}}+\frac{m}{n} AQ+\frac{m-n}{2n} BQ+\frac{m-2n}{3n} CQ+\frac{m-3n}{4n} DQ+⋯$

1665年，伦敦爆发鼠疫，牛顿回到乡下躲避瘟疫，正是在这个时期他做出了平生最重要的几个成就，二项式定理、光的分解，万有引力，微分。这个公式刻在牛顿墓碑上足见它才是牛顿最引以为傲的成就，事实上，后来的微积分正是由牛顿二项式孕育而来。我们所熟知的二项式定理是在高中学习排列组合的时候接触的，

$(1+x)^{n}=\sum_{r=0}^{n}\binom{n}{r}x^{r}=\sum_{r=0}^{n}\frac{n!}{r!(n-r)!}x^{r}=1+nx+\frac{n(n-1)}{2!}x^2+\cdot\cdot\cdot+\frac{n!}{n!}x^n$

这里的n是正整数，这个公式用数学归纳法就可以证明。牛顿二项式是这个二项式的推广：

$(1+x)^p=1+px+\frac{p(p-1)}{2!}x^2+\frac{p(p-1)(p-2)}{3!}x^3 +\cdot\cdot\cdot$

这里p是有理数。有了这个式子我们就可以展开像 $\sqrt{1+x}$ 、 $\frac{1}{1+x}$ 这样的式子。

比较一下这两个式子，可以看到上面的式子展开后是有限项，下面这个展开后是无穷项，其实上面的式子展开后也可以写成无穷项，只不过后面的项系数全为零。如第n+2项：n*(n-1)*(n-2)* ... *1*0/(n+1)!。这样看来经典的二项式定理，就是牛顿二项式，也就是广义二项式定理的特殊情况。牛顿猜测出这样的展开式之后并没有给出证明，后来欧拉完善了这个证明，现在根据欧拉的方法来证明一下。

构造一个函数： $f(m)=1+mx+\frac{m(m-1)}{2!}x^2+\cdot\cdot\cdot$

这里m是有理数，先证明f这个函数满足f(m)f(n)=f(m+n)，回忆经典二项式定理，若a，b是正整数，则

$f(a)=1+ax+\frac{a(a-1)}{2!}x^2+\cdot\cdot\cdot=(1+x)^a$

$f(b)=1+bx+\frac{b(b-1)}{2!}x^2+\cdot\cdot\cdot=(1+x)^b$

$f(a+b)=(1+x)^{a+b}=(1+x)^a\cdot(1+x)^b=f(a)f(b)$

这样f(a+b)与f(a)f(b)同类项的系数一定相等，f(a+b)的第（k+1）项为

$\frac{(a+b)(a+b-1)\cdot\cdot\cdot(a+b-k+1)}{k!}x^k$

f(a)f(b)的x^k这一项的系数为

$\sum_{i=0}^{k}{\frac{a(a-1)\cdot\cdot\cdot (a-i+1)}{i!}\cdot\frac{b(b-1)\cdot\cdot\cdot (b-k+i+1)}{(k-i)!}}$

由于f(a+b)=f(a)f(b)，于是

$\frac{(a+b)(a+b-1)\cdot\cdot\cdot(a+b-k+1)}{k!}=\sum_{i=0}^{k}{\frac{a(a-1)\cdot\cdot\cdot (a-i+1)}{i!}\cdot\frac{b(b-1)\cdot\cdot\cdot (b-k+i+1)}{(k-i)!}}$

这是一个恒等式，对于任意正整数a,b成立，牛顿二项式的推广本质来说是这个恒等式对于有理数也成立，甚至对实数、复数都成立。我们在扩充数域的时候保证了运算法则的兼容，也就是不管是整数、有理数、实数、复数，它们都满足加法和乘法的交换律、结合律，满足乘法分配率，于是既然这个恒等式在整数集成立，在有理数集必然也成立。

这样，对于有理数m,n，就有

$\frac{(m+n)(m+n-1)\cdot\cdot\cdot(m+n-k+1)}{k!}=\sum_{i=0}^{k}{\frac{m(m-1)\cdot\cdot\cdot (m-i+1)}{i!}\cdot\frac{n(n-1)\cdot\cdot\cdot (n-k+i+1)}{(k-i)!}}$

于是：

$f(m+n)=1+(m+n)x+\frac{(m+n)(m+n-1)}{2!}x^2+\cdot\cdot\cdot=f(m)f(n)$

有了它，后面的证明就简单了。先考虑正有理数的情况，设a,b是正整数：

$f(\frac{a}{b})=1+\frac{a}{b}x+\frac{\frac{a}{b}(\frac{a}{b}-1)}{2!}x^2+\cdot\cdot\cdot$

$[f(\frac{a}{b})]^b=f(a)=1+ax+\frac{a(a-1)}{2!}x^2+\cdot\cdot\cdot=(1+x)^a$

则 $(1+x)^\frac{a}{b}=f(\frac{a}{b})=1+\frac{a}{b}x+\frac{\frac{a}{b}(\frac{a}{b}-1)}{2!}x^2+\cdot\cdot\cdot$

对于负有理数，设p为负有理数，则f(p)f(-p)=f(p-p)=f(0)=1,这样f(p)=1/f(-p),前面已经证明了正有理数满足牛顿二项式，则

$(1+x)^p=\frac{1}{(1+x)^{-p}}=\frac{1}{f(-p)}=f(p)=1+px+\frac{p(p-1)}{2!}x^2+\cdot\cdot\cdot$

现在来展开$\sqrt{1+x}$，让 $\frac{a}{b}=\frac{1}{2}$ ，得到

$(1+x)^\frac{1}{2}=f(\frac{1}{2})=1+\frac{1}{2}x-\frac{1}{8}x^2+\frac{1}{16}x^3-\frac{5}{128}x^4+\cdot\cdot\cdot$

$\frac{1}{1+x}=1-x+x^2-x^3+\cdot\cdot\cdot$