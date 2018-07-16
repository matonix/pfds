# 演習問題 10.1

> update が O(log^2 n) 時間で動作することを証明しよう。

```haskell
update 0 e (One x ps) = One e ps -- パターン1
update i e (One x ps) = cons x (update (i - 1) e (Zero ps)) -- パターン2
update i e (Zero ps) = 
	let -- パターン3
    (x, y) = lookup (i `div` 2) ps
    p = if i `mod` 2 == 0 then (e, y) else (x, e)
    in Zero (update (i - 1) p ps) -- i `div` 2が正しい？ (slackより)
```

最悪ケースは n = k^2 - 1 となる場合であり、再帰中にupdate のパターン 2 と 3 を k 回（k - 1回？）ずつ繰り返すこととなる。

パターン 1, 2 そのものは O(1) で、パターン 3 が lookup の呼び出しのために O(log n) となっており、それが k (= O(log n))回実行されるため、全体としては O(log^2 n) 時間となる。

