module PFDS.Sec9.SkewTrinaryNumber where

type Nat = [Int]

inc :: Nat -> Nat
inc ws@(w1 : w2 : w3 : rest) =
  if w1 == w2 && w2 == w3
    then 1 + w1 + w2 + w3 : rest
    else 1 : ws
inc ws = 1 : ws

dec :: Nat -> Nat
dec (1 : ws) = ws
dec (w : ws) = let w' = w `div` 3 in w' : w' : w' : ws
