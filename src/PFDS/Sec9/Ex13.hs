{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex12 where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
data Digit a =
    Zero (Tree a) (Tree a) -- Red
  | Ones [Tree a] -- Yellow
  | Two -- Green
  | Threes [Tree a] -- Yellow
  | Four (Tree a) (Tree a) -- Red
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
  cons x ts = consTree (Leaf x) ts

  head :: RList a -> a
  head ts = let (Leaf x, _) = unconsTree ts in x

  tail :: RList a -> RList a
  tail ts = let (_, ts') = unconsTree ts in ts'

  lookup :: Int -> RList a -> a
  lookup i [] = error "Subscript"
  lookup i (Zero : ts) = lookup i ts
  lookup i (One t : ts) = if i < size t
    then lookupTree i t
    else lookup (i - size t) ts

  update :: Int -> a -> RList a -> RList a
  update = undefined

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t (Zero : ts) = One t : ts
consTree t1 (One t2 : ts) = Zero : consTree (link t1 t2) ts

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [One t] = (t, [])
unconsTree (One t : ts) = (t, Zero : ts)
unconsTree (Zero : ts) = let
  (Node _ t1 t2, ts') = unconsTree ts
  in (t1, One t2 : ts')

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree i (Leaf _) = error "Subscript"
lookupTree i (Node w t1 t2) = if i < w `div` 2
  then lookupTree i t1
  else lookupTree (i - w `div` 2) t2

-- digit helpers

ones :: Int -> [Digit a] -> [Digit a]
ones 0 ds = ds
ones i (Ones j : ds) = Ones (i + j) : ds
ones i ds = Ones i : ds

threes :: Int -> [Digit a] -> [Digit a]
threes 0 ds = ds
threes i (Threes j : ds) = Threes (i + j) : ds
threes i ds = Threes i : ds

simpleInc :: [Digit a] -> [Digit a]
simpleInc [] = [Threes 1]
simpleInc (Zero : ds) = ones 1 ds
simpleInc (Ones i : ds) = Two : ones (i - 1) ds
simpleInc (Two : ds) = threes 1 ds
simpleInc (Threes i : ds) = Four : threes (i - 1) ds

simpleDec :: [Digit a] -> [Digit a]
simpleDec [] = [Ones 1]
simpleDec (Ones i : ds) = Zero : ones (i - 1) ds
simpleDec (Two : ds) = ones 1 ds
simpleDec (Threes i : ds) = Two : threes (i - 1) ds
simpleDec (Four : ds) = threes 1 ds

fixup :: [Digit a] -> [Digit a]
fixup (Zero : ds) = Two : simpleDec ds
fixup (Ones i : Zero : ds) = Ones i : Two : simpleDec ds
fixup (Threes i : Four : ds) = Threes i : Two : simpleInc ds
fixup (Four : ds) = Two : simpleInc ds
fixup ds = ds

inc :: [Digit a] -> [Digit a]
inc = fixup . simpleInc

dec :: [Digit a] -> [Digit a]
dec = fixup . simpleDec
