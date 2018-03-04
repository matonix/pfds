# 演習問題 9.7

以前紹介して頂いた、[Constructing Red-Black Trees](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.84.8182&rep=rep1&type=pdf) の 3. A closer look at top-down を読むと…

ゼロなし二進ランダムアクセスリストの `cons` 関数

```haskell
cons :: a -> RList a -> RList a
cons x ts = consTree (Leaf x) ts

consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t1 (One t2 : ts) = Two t1 t2 : ts
consTree t1 (Two t2 t3 : ts) = One t1 : consTree (link t2 t3) ts
```

赤黒木の挿入を左端のみに制限した `insert` 関数

```haskell
insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a' y' b'
  where
    T _ a' y' b' = ins s
    ins E = T R E x E
    ins s'@(T color a y b) = balance color (ins a) y b

-- Ex10 での llbalance
balance :: Color -> Tree e -> e -> Tree e -> Tree e
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance color e w f = T color e w f
```

本質的な構造の変化（再構築？）を起こしているのは `consTree` や `balance` であるが、どちらの例でも中身を参照していない部分木

- `consTree`: `t1`, `t2`, `t3`
- `balance`: `a`, `b`, `c`, `d`

の左からの出現順は変化していない。すなわち、挿入後の再構築において、in-order（通りがけ順）での要素の並びが変化しないことが共通する不変条件となる。
