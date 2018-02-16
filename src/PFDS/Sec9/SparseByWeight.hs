module PFDS.Sec9.SparseByWeight where

type Nat = [Int]

carry :: Int -> Nat -> Nat
carry w [] = [w]
carry w ws@(w' : ws') =
  if w < w' then w : ws else carry (2 * w) ws'

borrow :: Int -> Nat -> Nat
borrow w [] = [w]
borrow w ws@(w' : ws') =
  if w == w' then ws' else w : borrow (2 * w) ws

inc :: Nat -> Nat
inc ws = carry 1 ws

dec :: Nat -> Nat
dec ws = borrow 1 ws

add :: Nat -> Nat -> Nat
add ws [] = ws
add [] ws = ws
add m@(w1 : ws1) n@(w2 : ws2)
  | w1 < w2 = w1 : add ws1 n
  | w2 < w1 = w2 : add m ws2
  | otherwise = carry (2 * w1) (add ws1 ws2)
