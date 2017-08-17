# 演習問題 5.6

> `deleteMin` が $O(\log n)$ 償却時間で実行されることを示す。

`deleteMin t` の償却コストが ${\cal A}(t) \le 1+2\phi(t)=1+2\log(\#t)$ を満たすことを示す。

`s' = deleteMin s` とすると、この時の構造の変化は以下のようになる。

```
  s = y            x = s'
     / \          / \
t = x   b   =>   u'  y = t'
   / \              / \
  u   a            a   b
```

ただし、`u' = deleteMin u` とする。すると、

$\qquad \cal A(s)$

$=\qquad \{\cal A$ の定義 $\}$

$\qquad {\cal T}(s)+\Phi(s')-\Phi(s)$

$=\qquad \{{\cal T}(s)=1+{\cal T}(u)\}$

$\qquad 1+{\cal T}(u)+\Phi(s')-\Phi(s)$

$=\qquad \{{\cal T}(u)={\cal A}(u)-\Phi(u')+\Phi(u)\}$

$\qquad 1+{\cal A}(u)-\Phi(u')+\Phi(u)+\Phi(s')-\Phi(s)$

$=\qquad \{\Phi(s'), \Phi(s)$ を展開して単純化 $\}$

$\qquad 1+{\cal A}(u)+\phi(s')+\phi(t')-\phi(s)-\phi(t)$

$\le\qquad \{$ 帰納法の仮定: ${\cal A}(u)\le 1+2\phi(u) \}$

$\qquad 2+2\phi(u)+\phi(s')+\phi(t')-\phi(s)-\phi(t)$

$<\qquad \{\phi(u)<\phi(t)$ と $\phi(s')<\phi(s)\}$

$\qquad 2+\phi(u)+\phi(t')$

$<\qquad \{\#u+\#t'<\#s$ と補題5.1 $\}$

$\qquad 1+2\phi(s)$

となる。

