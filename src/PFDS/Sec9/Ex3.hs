{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex3 where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
type RList a = [Tree a]

type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty [] = True
  isEmpty _ = False

  cons :: a -> RList a -> RList a
  cons x ts = consTree (Leaf x) ts

  head :: RList a -> a
  head ts = let (Leaf x, _) = unconsTree ts in x

  tail :: RList a -> RList a
  tail ts = let (_, ts') = unconsTree ts in ts'

  lookup :: Int -> RList a -> a
  lookup i [] = error "Subscript"
  lookup i (t : ts) = if i < size t
    then lookupTree i t
    else lookup (i - size t) ts

  update :: Int -> a -> RList a -> RList a
  update i y [] = error "Subscript"
  update i y (t : ts) = if i < size t
    then updateTree i y t : ts
    else t : update (i - size t) y ts

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t [] = [t]
consTree t1 (t2 : ts) = if size t1 == size t2
  then consTree (link t1 t2) ts
  else t1 : t2 : ts

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree (Leaf e : ts) = (Leaf e, ts)
unconsTree (Node _ t1 t2 : ts) = unconsTree (t1 : t2 : ts)

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree i (Leaf _) = error "Subscript"
lookupTree i (Node w t1 t2) = if i < w `div` 2
  then lookupTree i t1
  else lookupTree (i - w `div` 2) t2

updateTree :: Int -> a -> Tree a -> Tree a
updateTree 0 y (Leaf x) = Leaf y
updateTree i _ (Leaf _) = error "Subscript"
updateTree i y (Node w t1 t2) = if i < w `div` 2
  then Node w (updateTree i y t1) t2
  else Node w t1 (updateTree (i - w `div` 2) y t2)
