# 演習問題 3.4 (d)

```haskell
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) = if x <= y
  then makeT x a1 $ merge b1 h2
  else makeT y a2 $ merge h1 b2
    
makeT x' a b = if size a >= size b
  then T (size a + size b + 1) x' a b
  else T (size a + size b + 1) x' b a
```

上記コードでは、MakeT において size の比較を行う際、呼び出し元の merge の評価が行われている。

小さい方の木の高さが h のとき、高さ (h-1) の木をマージすることになるため、

sum [1..h] (O(h^2))回くらいマージしていると思う。



一方、下記コードでは一切 merge が評価されない。

show 等を実行して初めて評価が開始される。

その回数も小さい方の木の高さが h のとき、最悪でも h 回程度である。

並列実行の環境では、各ノード毎のマージ処理が独立しているので、各ノード毎のマージを並列化できそう。

```haskell
  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    | x <= y && size a1 >= size b1 + size h2 = T (size h1 + size h2) x a1 (merge b1 h2)
    | x <= y                                 = T (size h1 + size h2) x (merge b1 h2) a1
    |           size a2 >= size b2 + size h1 = T (size h1 + size h2) y a2 (merge h1 b2)
    | otherwise                              = T (size h1 + size h2) y (merge h1 b2) a2
```