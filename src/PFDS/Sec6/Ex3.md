# 演習問題 6.3

> `findMin` `deleteMin` `merge` も $O(\log n)$ 償却時間で実行されることを証明しよう

## `findMin`

### アウトライン

`findMin` は停止計算を進行させ、 `removeMinTree` と `root` を1回ずつ呼ぶ。

`root` は $O(1)$ であるため `removeMinTree` に着目する。

`removeMinTree` は再帰関数であり、ヒープを構成する停止リストを逐次的に処理する。

ヒープに含まれる木の数が $t$ のとき、`findMin` 経由で呼ばれた `removeMinTree` は $t-1$ 個の停止計算をつくる。





$t$ は $n$ の2進表記における 1 の数に等しく、償却コストは $t$ の定数倍に収まるので、

償却コストは $O(\log n )$ となる。