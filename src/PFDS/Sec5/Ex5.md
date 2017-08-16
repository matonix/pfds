# 演習問題 5.5

## の前に

### 記号

- $\#t$
  - $t$ のサイズ $+1$
  - $t=T(a,z,b)\  \rightarrow\  \#t=\#a+\#b$ 

- $\phi(t)$
  - 個々のノードのポテンシャル
  - $\phi(t)=\log(\#t)$

- $\Phi(t)$

  - 木全体のポテンシャル
  - 木の個々のノードの全てのポテンシャルの合計

- ${\cal T}(t)$
  -  $t$ に対する `partition` 呼び出しの実コスト
  -  `partition` の再帰呼び出しの合計回数

- ${\cal A}(t)$
  - 木 $t$ に対する `partition` 呼び出しの償却コスト

  - ${\cal A}(T)={\cal T}(t)+\Phi(a)+\Phi(b)-\Phi(t)$

    - `(a, b) = partition(pivot, t)`

    ​

### 補題 5.1

$y+z\le x$ なるすべての正の $x$、$y$、$z$ について、
$$
1+\log y +\log z<2\log x
$$

### 定理 5.2

$$
{\cal A}(t) \le 1+2\phi(t)=1+2\log(\#t)
$$

## では

>  定理 5.2 における zig-zag ケースを証明する。

```
  s = x                   ||
     / \         s'= y    ||    x = t'
t = y   d   =>      / \   ||   / \
   / \             c   a  ||  b   d
  c   u                   ||
```

ただし、$a$ と $b$  は `partition (pivot, u)` の返り値とする。すると、

​	$\cal A(s)$

$=$ 		$\{\cal A$ の定義 $\}$

​	${\cal T}(s)+\Phi(s')+\Phi(t')-\Phi(s)$

$=$ 		$\{{\cal T}(s)=1+{\cal T}(u)\}$

​	$1+{\cal T}(u)+\Phi(s')+\Phi(t')-\Phi(s)$

$=$		$\{{\cal T}(u)={\cal A}(u)-\Phi(a)-\Phi(b)+\Phi(u)\}$

​	$1+{\cal A}(u)-\Phi(a)-\Phi(b)+\Phi(u)+\Phi(s')+\Phi(t')-\Phi(s)$

$=$		$\{\Phi(s'), \Phi(t'), \Phi(s)$ を展開して単純化 $\}$

​	$1+{\cal A}(u)-\phi(s')+\phi(t')-\phi(s)-\phi(t)$

$\le$ 		$\{$ 帰納法の仮定: ${\cal A}(c)\le 1+2\phi(c) \}$

​	$2+2\phi(c)-\phi(s')+\phi(t')-\phi(s)-\phi(t)$

$<$		$\{\phi(c)<\phi(s)$ と $\phi(s')\le\phi(t)\}$

​	$2+\phi(c)+\phi(t')$

$<$		$\{\#c+\#t'<\#s$ と補題5.1 $\}$

​	$1+2\phi(s)$

となる。

## 補足

- $\Phi(s')=\phi(s')+\Phi(c)+\Phi(a)$
- $\Phi(t')=\phi(t')+\Phi(b)+\Phi(d)$
- $\Phi(s)=\phi(s)+\phi(t)+\Phi(c)+\Phi(d)+\Phi(u)$

