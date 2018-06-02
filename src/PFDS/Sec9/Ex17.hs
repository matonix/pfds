{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex17 where

import qualified PFDS.Commons.HeapFamily as H

data Tree a = Node a [(Tree a, Tree a)]
data Digit a = Zero | One (Tree a) | Two (Tree a) (Tree a)
type Heap a = [Digit a]

type instance H.Elem (Heap a) = a

instance Ord a => H.Heap (Heap a) where
  empty :: Heap a
  empty = []

  isEmpty :: Heap a -> Bool
  isEmpty = null

  insert :: a -> Heap a -> Heap a
  insert x ts = insTree (Node x []) ts

  merge :: Heap a -> Heap a -> Heap a
  merge ts1 ts2 = mergeTrees ts1 ts2

  findMin :: Heap a -> a
  findMin ts = root t where
    (t, _) = removeMinTree ts

  deleteMin :: Heap a -> Heap a
  deleteMin ts = let
    (Node _ ts1, ts2) = removeMinTree ts
    in mergeTrees (reverse $ map (uncurry Two) ts1) ts2

root :: Tree a -> a
root (Node x _) = x

link :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) t3@(Node x3 c3)
  | x1 <= x2  = Node x1 ((t2, t3) : c1)
  | x1 <= x3  = Node x2 ((t1, t3) : c2)
  | otherwise = Node x3 ((t2, t1) : c3)

two :: Ord a => Tree a -> Tree a -> Digit a
two t1 t2 = if root t1 <= root t2
  then Two t1 t2
  else Two t2 t1

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t1 (Zero : ds) = One t1 : ds
insTree t1 (One t2 : ds) = two t1 t2 : ds
insTree t1 (Two t2 t3 : ds) = Zero : One (link t1 t2 t3) : ds

mergeTrees :: Ord a => Heap a -> Heap a -> Heap a
mergeTrees (Zero : ds1) (d2 : ds2) = d2 : mergeTrees ds1 ds2
mergeTrees (d1 : ds1) (Zero : ds2) = d1 : mergeTrees ds1 ds2
mergeTrees (One t1 : ds1) (One t2 : ds2) = two t1 t2 : mergeTrees ds1 ds2
mergeTrees (One t1 : ds1) (Two t2 t3 : ds2) = Zero : One (link t1 t2 t3) : mergeTrees ds1 ds2
mergeTrees (Two t1 t2 : ds1) (One t3 : ds2) = Zero : One (link t3 t1 t2) : mergeTrees ds1 ds2
mergeTrees (Two t1 t2 : ds1) (Two t3 t4 : ds2) = let
  Two t1' t2' = two t1 t3
  Two t3' t4' = two t2 t4
  in One t1' : One (link t2' t3' t4') : mergeTrees ds1 ds2

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Empty"
removeMinTree (One t1 : ts) = (t1, ts)
removeMinTree (Two t1 t2 : ts) = (t1, One t2 : ts)
removeMinTree (Zero : ts) = let
  (Node x ((t1, t2) : ts1), ts2) = removeMinTree ts
  in (Node x ts1, Two t1 t2 : ts2)
