{-# LANGUAGE MultiWayIf #-}

module PFDS.Sec9.Ex16 where

-- SML のファンクタをそのまま模倣することは難しいので
-- 具体的なヒープに delete を付加させることを考える
-- （backpackならできるのかもしれない？）
-- 3.7 を参考に

import qualified PFDS.Commons.HeapFamily as H
import qualified PFDS.Commons.SkewBinomialHeap as SBH

--             Positive    Negative
type Heap a = (SBH.Heap a, SBH.Heap a)

empty :: Ord a => Heap a
empty = (H.empty, H.empty)

isEmpty :: Ord a => Heap a -> Bool
isEmpty (_, n) = H.isEmpty n

insert :: Ord a => a -> Heap a -> Heap a
insert x (p, n) = invariant (H.insert x p, n)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge (p1, n1) (p2, n2) = invariant (H.merge p1 p2, H.merge n1 n2)

findMin :: Ord a => Heap a -> a
findMin (p, n) = H.findMin p

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (p, n) = invariant (H.deleteMin p, H.deleteMin n)

delete :: Ord a => a -> Heap a -> Heap a
delete x (p, n) = invariant (p, H.insert x n)

invariant :: Ord a => Heap a -> Heap a
invariant h@(p, n) = let
  pMin = H.findMin p
  nMin = H.findMin n
  in if
    | pMin < nMin  -> h
    | pMin == nMin -> invariant (deleteMin h)
    | otherwise    -> error "Invariant"
