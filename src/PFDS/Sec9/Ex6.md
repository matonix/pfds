# 演習問題 9.6

> i 番目の要素に対する `lookup` や `update` が O(log i) 時間で動くことを示せ

どちらの関数も定数個の分岐と `size` 関数の呼び出しがある。 `size` 関数は定数時間で動作する。

`lookup` および `update` の再帰呼び出しの回数は高々 log i 回である。
なお、 `updateTree` の呼び出し以降は `lookup` および `update` の再帰呼び出しは行われない。

Digit list の k 番目で `updateTree` が呼ばれたとする。
Digit list はゼロなし二進数なので、 k 番目の木のサイズは 2^k である。
サイズ 2^k の木の深さは高々 k である。
よって、k = log i のとき、`updateTree` の呼び出し回数は高々 log i である。

以上の処理を合算しても、処理全体は O(log i) で動作する。
