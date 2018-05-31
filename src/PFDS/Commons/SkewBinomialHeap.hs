{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Commons.SkewBinomialHeap
  ( module H
  , module PFDS.Commons.SkewBinomialHeap
  ) where

import qualified PFDS.Commons.HeapFamily as H

data Tree a = Node Int a [a] [Tree a]
type Heap a = [Tree a]

type instance H.Elem (Heap a) = a

instance Ord a => H.Heap (Heap a) where
  empty :: Heap a
  empty = []

  isEmpty :: Heap a -> Bool
  isEmpty = null

  insert :: a -> Heap a -> Heap a
  insert x ts@(t1 : t2 : rest) = if rank t1 == rank t2
    then skewLink x t1 t2 : rest
    else Node 0 x [] [] : ts
  insert x ts = Node 0 x [] [] : ts

  merge :: Heap a -> Heap a -> Heap a
  merge ts1 ts2 = mergeTrees (normalize ts1) (normalize ts2)

  findMin :: Heap a -> a
  findMin ts = root t where
    (t, _) = removeMinTree ts

  deleteMin :: Heap a -> Heap a
  deleteMin ts = insertAll xs (H.merge (reverse ts1) ts2) where
    (Node _ x xs ts1, ts2) = removeMinTree ts
    insertAll xs ts = foldl (flip H.insert) ts xs


-- helper

rank :: Tree a -> Int
rank (Node r _ _ _) = r

root :: Tree a -> a
root (Node _ x _ _) = x

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = let Node r y ys c = link t1 t2
  in if x <= y
    then Node r x (y : ys) c
    else Node r y (x : ys) c

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node _ x2 xs2 c2) = if x1 <= x2
  then Node (r + 1) x1 xs1 (t2 : c1)
  else Node (r + 1) x2 xs2 (t1 : c2)

normalize :: Ord a => Heap a -> Heap a
normalize [] = []
normalize (t : ts) = insTree t ts

mergeTrees :: Ord a => Heap a -> Heap a -> Heap a
mergeTrees ts1 [] = ts1
mergeTrees [] ts2 = ts2
mergeTrees ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : mergeTrees ts1' ts2
  | rank t2 < rank t1 = t2 : mergeTrees ts1 ts2'
  | otherwise = insTree (link t1 t2) (mergeTrees ts1' ts2')

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 (t2 : ts) = if rank t1 < rank t2
  then t1 : t2 : ts
  else insTree (link t1 t2) ts

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Empty"
removeMinTree [t] = (t, [])
removeMinTree (t : ts) = let (t', ts') = removeMinTree ts
  in if root t < root t'
    then (t, ts)
    else (t', t : ts')
