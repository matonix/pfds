{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex11 where

import PFDS.Commons.Heap (Heap(..))
data Tree a = Node a [Tree a] deriving Show
data Digit a = Zero | Ones [Tree a] | Two (Tree a) (Tree a) deriving Show
newtype BHeap a = BH [Digit a] deriving Show

instance Heap BHeap where
  empty :: Ord a => BHeap a
  empty = BH []

  isEmpty :: Ord a => BHeap a -> Bool
  isEmpty (BH ts) = null ts

  insert :: Ord a => a -> BHeap a -> BHeap a
  insert x (BH ts) = BH $ fixup $ simpleInsTree (Node x []) ts

  merge :: Ord a => BHeap a -> BHeap a -> BHeap a
  merge (BH ts1) (BH ts2) = BH $ merge' ts1 ts2

  findMin :: Ord a => BHeap a -> a
  findMin = undefined

  deleteMin :: Ord a => BHeap a -> BHeap a
  deleteMin = undefined

-- helper functions

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) =
  if x1 <= x2
    then Node x1 (t2:c1)
    else Node x2 (t1:c2)

merge' :: Ord a => [Digit a] -> [Digit a] -> [Digit a]
merge' ds1 [] = ds1
merge' [] ds2 = ds2
merge' (Zero : ds1) (Zero : ds2) = Zero : merge' (fixup ds1) (fixup ds2)
merge' (Ones (t1 : ts1) : ds1) (Zero : ds2) = ones [t1] (merge' (f ts1 ds1) (fixup ds2))
merge' (Zero : ds1) (Ones (t2 : ts2) : ds2) = ones [t2] (merge' (fixup ds1) (f ts2 ds2))
merge' (Ones (t1 : ts1) : ds1) (Ones (t2 : ts2) : ds2) = Zero : simpleInsTree (link t1 t2) (merge' (f ts1 ds1) (f ts2 ds2))

f :: Ord a => [Tree a] -> [Digit a] -> [Digit a]
f [] = fixup
f ts = ones ts

simpleInsTree :: Tree a -> [Digit a] -> [Digit a]
simpleInsTree t [] = [Ones [t]]
simpleInsTree t (Zero : ds) = ones [t] ds
simpleInsTree t1 (Ones (t2 : ts) : ds) = Two t1 t2 : ones ts ds

ones :: [Tree a]-> [Digit a] -> [Digit a]
ones [] ds = ds
ones ts1 (Ones ts2 : ds) = Ones (ts1 ++ ts2) : ds -- 多分大丈夫
ones ts ds = Ones ts : ds

fixup :: Ord a => [Digit a] -> [Digit a]
fixup (Two t1 t2 : ds) = Zero : simpleInsTree (link t1 t2) ds
fixup (Ones ts : Two t1 t2 : ds) = Ones ts : Zero : simpleInsTree (link t1 t2) ds
fixup ds = ds
