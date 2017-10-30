{-# LANGUAGE StrictData #-}

module PFDS.Commons.LazyPairingHeap where

import PFDS.Commons.Heap (Heap (..))

data LazyPairingHeap e = E | T e (LazyPairingHeap e) (LazyPairingHeap ~e) deriving (Show)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge a E = a
  merge E b = b
  merge a@(T x _ _) b@(T y _ _) =
    if x <= y then link a b else link b a

  insert x = merge (T x E E)

  findMin E = error "empty"
  findMin (T x _ _) = x

  deleteMin E = error "empty"
  deleteMin (T _ a b) = merge a b

link :: PairingHeap e -> PairingHeap e -> PairingHeap e
link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)

{-| Doctests for PairingHeap

>>> mapM_ (print . foldl (flip insert) (empty::PairingHeap Int)) [[1..x] | x <- [1..5]]
T 1 []
T 1 [T 2 []]
T 1 [T 3 [],T 2 []]
T 1 [T 4 [],T 3 [],T 2 []]
T 1 [T 5 [],T 4 [],T 3 [],T 2 []]

>>> (print . foldl (flip insert) (empty::PairingHeap Int)) [5,2,4,3,1]
T 1 [T 2 [T 3 [],T 4 [],T 5 []]]

>>> (print . foldl (flip insert) (empty::PairingHeap Int)) [5,4,3,2,1]
T 1 [T 2 [T 3 [T 4 [T 5 []]]]]

-}
