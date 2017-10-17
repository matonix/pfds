module PFDS.Commons.PairingHeap where

import PFDS.Commons.Heap (Heap (..))

data PairingHeap e = E | T e [PairingHeap e] deriving (Show)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2) =
    if x <= y then T x (h2:hs1) else T y (h1:hs2)

  insert x = merge (T x [])

  findMin E = error "empty"
  findMin (T x _) = x

  deleteMin E = error "empty"
  deleteMin (T _ hs) = mergePairs hs

mergePairs :: Ord e => [PairingHeap e] -> PairingHeap e
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

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
