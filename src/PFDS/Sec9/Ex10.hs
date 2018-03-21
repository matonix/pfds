{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module PFDS.Sec9.Ex10 where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a =
    Leaf a
  | Node Int (Tree a) (Tree a)
  deriving (Show, Eq)
data Digit a =
    One (Tree a)
  | Two (Tree a) (Tree a) -- non-recursive
  | Two' (Tree a) (Tree a) -- recursive
  | Three (Tree a) (Tree a) (Tree a)
  deriving (Show, Eq)
type RList a = [Digit a]
type SRList a = (RList a, Schedule a)
type Schedule a = [RList a]

type instance Elem (SRList a) = a

instance RandomAccessList (SRList a) where
  empty :: SRList a
  empty = ([], [])

  isEmpty :: SRList a -> Bool
  isEmpty ([], _) = True
  isEmpty _ = False

  cons :: a -> SRList a -> SRList a
  cons x (ts, sched) = let
    ts' = consTree (Leaf x) ts
    !sched' = exec (exec (ts' : sched))
    in (ts', sched')

  head :: SRList a -> a
  head (One (Leaf x) : _, _) = x
  head (Two (Leaf x) _ : _, _) = x
  head (Three (Leaf x) _ _ : _, _) = x

  tail :: SRList a -> SRList a
  tail (ts, sched) = let
    (_, ts') = unconsTree ts
    !sched' = exec (exec (ts' : sched)) -- how many times?
    in (ts', sched')

  lookup = undefined
  update = undefined

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t1 (One t2 : ts) = Two t1 t2 : ts
consTree t1 (Two t2 t3 : ts) = Three t1 t2 t3 : ts
consTree t1 (Three t2 t3 t4 : ts) = Two' t1 t2 : consTree (link t3 t4) ts

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [One t] = (t, [])
unconsTree (One t : ts) = (t, Two' t1 t2 : ts')
  where
    (Node _ t1 t2, ts') = unconsTree ts
unconsTree (Two t1 t2 : ts) = (t1, One t2 : ts)
unconsTree (Three t1 t2 t3 : ts) = (t1, Two t2 t3 : ts)

exec :: Schedule a -> Schedule a
exec [] = []
exec ((Two' _ _ : job) : sched) = job : sched
exec (_ : sched) = sched
