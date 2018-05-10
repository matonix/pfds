{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Commons.SkewBinaryRandomAccessList where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)
type RList a = [(Int, Tree a)]

type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty = null

  cons :: a -> RList a -> RList a
  cons x ts@((w1, t1) : (w2, t2) : rest) =
    if w1 == w2
      then (1 + w1 + w2, Node x t1 t2) : rest
      else (1, Leaf x) : ts
  cons x ts = (1, Leaf x) : ts

  head :: RList a -> a
  head [] = error "Empty"
  head ((_, Leaf x) : ts) = x
  head ((_, Node x _ _) : ts) = x

  tail :: RList a -> RList a
  tail [] = error "Empty"
  tail ((_, Leaf _) : ts) = ts
  tail ((w, Node _ t1 t2) : ts) = let w' = w `div` 2 in
    (w', t1) : (w', t2) : ts

  lookup :: Int -> RList a -> a
  lookup i [] = error "Subscript"
  lookup i ((w, t) : ts) =
    if i < w
      then lookupTree w i t
      else lookup (i - w) ts

  update :: Int -> a -> RList a -> RList a
  update i y [] = error "Subscript"
  update i y ((w, t) : ts) =
    if i < w
      then let t' = updateTree w i y t in (w, t') : ts
      else update (i - w) y ts

-- helper

lookupTree :: Int -> Int -> Tree a -> a
lookupTree 1 0 (Leaf x) = x
lookupTree 1 _ (Leaf x) = error "Subscript"
lookupTree _ 0 (Node x _ _) = x
lookupTree w i (Node x t1 t2) = let w' = w `div` 2 in
  if i <= w' -- P.127 に誤字
    then lookupTree w' (i - 1) t1
    else lookupTree w' (i - 1 - w') t2

updateTree :: Int -> Int -> a -> Tree a -> Tree a
updateTree 1 0 y (Leaf _) = Leaf y
updateTree 1 _ _ (Leaf _) = error "Subscript"
updateTree _ 0 y (Node x t1 t2) = Node y t1 t2
updateTree w i y (Node x t1 t2) = let w' = w `div` 2 in
  if i <= w'
    then let t1' = updateTree w' (i - 1) y t1 in Node x t1' t2
    else let t2' = updateTree w' (i - 1 - w') y t2 in Node x t1 t2'
