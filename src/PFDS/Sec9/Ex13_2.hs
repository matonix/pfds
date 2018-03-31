{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex13_2 where

-- リストを使ってTreeの数に関する明示的な制約を犠牲に実装を簡素にしたバージョン

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
data Digit a =
    Zero  -- Red
  | Ones [[Tree a]] -- Yellow
  | Two [Tree a] -- Green
  | Threes [[Tree a]] -- Yellow
  | Four [Tree a] -- Red
  deriving (Show, Eq)
type RList a = [Digit a]

type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty [] = True
  isEmpty _ = False

  cons :: a -> RList a -> RList a
  cons x ds = fixup (consTree (Leaf x) ds)

  head :: RList a -> a
  head ds = let (Leaf x, _) = unconsTree ds in x

  tail :: RList a -> RList a
  tail ds = let (_, ds') = unconsTree ds in fixup ds'

  lookup :: Int -> RList a -> a
  lookup i [] = error "Subscript"
  lookup i (d : ds) = lookupTrees i (unwrapTrees d) ds

  update :: Int -> a -> RList a -> RList a
  update = undefined

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t [] = [Ones [[t]]]
consTree t (Ones (ts : tss) : ds) = Two (t : ts) : ones tss ds
consTree t (Two ts : ds) = threes [t : ts] ds
consTree t (Threes (ts : tss) : ds) = Four (t : ts) : threes tss ds

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [Ones [[t]]] = (t, [])
unconsTree (Ones ([t] : tss) : ds) = (t, Zero : ones tss ds)
unconsTree (Two (t : ts) : ds) = (t, ones [ts] ds)
unconsTree (Threes ((t : ts) : tss) : ds) = (t, Two ts : threes tss ds)

unwrapTrees :: Digit a -> [Tree a]
unwrapTrees Zero = []
unwrapTrees (Ones tss) = concat tss
unwrapTrees (Two ts) = ts
unwrapTrees (Threes tss) = concat tss
unwrapTrees (Four ts) = ts

lookupTrees :: Int -> [Tree a] -> RList a -> a
lookupTrees i [] ds = lookup i ds
lookupTrees i (t : ts) ds = if i < size t
  then lookupTree i t
  else lookupTrees (i - size t) ts ds

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree i (Leaf _) = error "Subscript"
lookupTree i (Node w t1 t2) = if i < w `div` 2
  then lookupTree i t1
  else lookupTree (i - w `div` 2) t2

-- digit helpers

ones :: [[Tree a]] -> RList a -> RList a
ones [] ds = ds
ones tss1 (Ones tss2 : ds) = Ones (tss1 ++ tss2) : ds
ones tss1 ds = Ones tss1 : ds

threes :: [[Tree a]] -> RList a -> RList a
threes [] ds = ds
threes tss1 (Threes tss2 : ds) = Threes (tss1 ++ tss2) : ds
threes tss1 ds = Threes tss1 : ds

fixup :: RList a -> RList a
fixup (Zero : ds) = let (Node _ t1 t2, ds') = unconsTree ds in Two [t1, t2] : ds'
fixup (Ones ts : Zero : ds) = Ones ts : fixup (Zero : ds)
fixup (Threes ts : Four t : ds) = Threes ts : fixup (Four t : ds)
fixup (Four [t1, t2, t3, t4] : ds) = Two [t1, t2] : consTree (link t3 t4) ds
fixup ds = ds
