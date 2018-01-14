# 演習問題 8.6

```haskell
BankersDeque a = Q (c::Int) (|f|::Int) (f::[a]) (|r|::Int) (r::[a]) -- 擬似コード
```

## c = 2 いじめ

```haskell
*PFDS.Sec8.Ex6> eval 2 "cccccttccttcctt"
cons: Q 2 1 [0] 0 []
cons: Q 2 1 [1] 1 [0] -- rebuild
cons: Q 2 2 [2,1] 1 [0]
cons: Q 2 3 [3,2,1] 1 [0]
cons: Q 2 2 [4,3] 3 [0,1,2] -- rebuild
tail: Q 2 1 [3] 3 [0,1,2]
tail: Q 2 2 [2,1] 1 [0] -- rebuild
cons: Q 2 3 [5,2,1] 1 [0]
cons: Q 2 2 [6,5] 3 [0,1,2] -- rebuild
tail: Q 2 1 [5] 3 [0,1,2]
tail: Q 2 2 [2,1] 1 [0] -- rebuild
cons: Q 2 3 [7,2,1] 1 [0]
cons: Q 2 2 [8,7] 3 [0,1,2] -- rebuild
tail: Q 2 1 [7] 3 [0,1,2]
tail: Q 2 2 [2,1] 1 [0] -- rebuild

*PFDS.Sec8.Ex6> eval 4 "cccccttccttcctt"
cons: Q 4 1 [0] 0 []
cons: Q 4 1 [1] 1 [0] -- rebuild
cons: Q 4 2 [2,1] 1 [0]
cons: Q 4 3 [3,2,1] 1 [0]
cons: Q 4 4 [4,3,2,1] 1 [0]
tail: Q 4 3 [3,2,1] 1 [0]
tail: Q 4 2 [2,1] 1 [0]
cons: Q 4 3 [5,2,1] 1 [0]
cons: Q 4 4 [6,5,2,1] 1 [0]
tail: Q 4 3 [5,2,1] 1 [0]
tail: Q 4 2 [2,1] 1 [0]
cons: Q 4 3 [7,2,1] 1 [0]
cons: Q 4 4 [8,7,2,1] 1 [0]
tail: Q 4 3 [7,2,1] 1 [0]
tail: Q 4 2 [2,1] 1 [0]
```

## c = 4 いじめ

```haskell
*PFDS.Sec8.Ex6> eval 4 "cccccccttssiicc"
cons: Q 4 1 [0] 0 []
cons: Q 4 1 [1] 1 [0] -- rebuild
cons: Q 4 2 [2,1] 1 [0]
cons: Q 4 3 [3,2,1] 1 [0]
cons: Q 4 4 [4,3,2,1] 1 [0]
cons: Q 4 5 [5,4,3,2,1] 1 [0]
cons: Q 4 3 [6,5,4] 4 [0,1,2,3] -- rebuild
tail: Q 4 2 [5,4] 4 [0,1,2,3]
tail: Q 4 1 [4] 4 [0,1,2,3]
snoc: Q 4 1 [4] 5 [7,0,1,2,3]
snoc: Q 4 4 [4,3,2,1] 3 [8,7,0] -- rebuild
init: Q 4 4 [4,3,2,1] 2 [7,0]
init: Q 4 4 [4,3,2,1] 1 [0]
cons: Q 4 5 [9,4,3,2,1] 1 [0]
cons: Q 4 3 [10,9,4] 4 [0,1,2,3] -- rebuild

*PFDS.Sec8.Ex6> eval 2 "cccccccttssiicc"
cons: Q 2 1 [0] 0 []
cons: Q 2 1 [1] 1 [0] -- rebuild
cons: Q 2 2 [2,1] 1 [0]
cons: Q 2 3 [3,2,1] 1 [0]
cons: Q 2 2 [4,3] 3 [0,1,2] -- rebuild
cons: Q 2 3 [5,4,3] 3 [0,1,2]
cons: Q 2 4 [6,5,4,3] 3 [0,1,2]
tail: Q 2 3 [5,4,3] 3 [0,1,2]
tail: Q 2 2 [4,3] 3 [0,1,2]
snoc: Q 2 2 [4,3] 4 [7,0,1,2]
snoc: Q 2 2 [4,3] 5 [8,7,0,1,2]
init: Q 2 2 [4,3] 4 [7,0,1,2]
init: Q 2 2 [4,3] 3 [0,1,2]
cons: Q 2 3 [9,4,3] 3 [0,1,2]
cons: Q 2 4 [10,9,4,3] 3 [0,1,2]
```

