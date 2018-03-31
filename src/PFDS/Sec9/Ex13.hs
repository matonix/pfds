{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex12 where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
data Digit a =
    Zero  -- Red
  | Ones [Tree a] -- Yellow
  | Two (Tree a, Tree a) -- Green
  | Threes [(Tree a, Tree a, Tree a)] -- Yellow
  | Four (Tree a, Tree a, Tree a, Tree a) -- Red
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
  lookup = undefined
  -- lookup i [] = error "Subscript"
  -- lookup i (Zero : ts) = lookup i ts
  -- lookup i (One t : ts) = if i < size t
  --   then lookupTree i t
  --   else lookup (i - size t) ts

  update :: Int -> a -> RList a -> RList a
  update = undefined

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t1 [] = [Ones [t1]]
consTree t1 (Ones (t2 : ts) : ds) = Two (t1, t2) : ones ts ds
consTree t1 (Two (t2, t3) : ds) = threes [(t1, t2, t3)] ds
consTree t1 (Threes ((t2, t3, t4) : ts) : ds) = Four (t1, t2, t3, t4) : threes ts ds

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [Ones [t1]] = (t1, [])
unconsTree (Ones (t1 : ts) : ds) = (t1, Zero : ones ts ds)
unconsTree (Two (t1, t2) : ds) = (t1, ones [t2] ds)
unconsTree (Threes ((t1, t2, t3) : ts) : ds) = (t1, Two (t2, t3) : threes ts ds)

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree i (Leaf _) = error "Subscript"
lookupTree i (Node w t1 t2) = if i < w `div` 2
  then lookupTree i t1
  else lookupTree (i - w `div` 2) t2

-- digit helpers

ones :: [Tree a] -> RList a -> RList a
ones [] ds = ds
ones ts1 (Ones ts2 : ds) = Ones (ts1 ++ ts2) : ds
ones ts1 ds = Ones ts1 : ds

threes :: [(Tree a, Tree a, Tree a)] -> RList a -> RList a
threes [] ds = ds
threes ts1 (Threes ts2 : ds) = Threes (ts1 ++ ts2) : ds
threes ts1 ds = Threes ts1 : ds

fixup :: RList a -> RList a
fixup (Zero : ds) = let (Node _ t1 t2, ds') = unconsTree ds in Two (t1, t2) : ds'
fixup (Ones ts : Zero : ds) = Ones ts : fixup (Zero : ds)
fixup (Threes ts : Four t : ds) = Threes ts : fixup (Four t : ds)
fixup (Four (t1, t2, t3, t4) : ds) = Two (t1, t2) : consTree (link t3 t4) ds
fixup ds = ds
