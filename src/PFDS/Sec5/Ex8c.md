# 演習問題 5.8 (c)

> ペアリングヒープの二分木表現における `deleteMin` と `merge` が $O(\log n)$ 償却時間で実行されることを示す。

`merge t` の償却コストが ${\cal A}(t) \le 1+2\phi(t)=1+2\log(\#t)$ を満たすことを示す。??

mergeってO(1)じゃないの？？

- mergeを重く作ることでdeleteMinが軽量化する実装方法がある…？