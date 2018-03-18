{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex9 where

import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a =
    Leaf a
  | Node Int (Tree a) (Tree a)
  deriving (Show, Eq)
data Digit a =
    One (Tree a)
  | Two (Tree a) (Tree a)
  | Three (Tree a) (Tree a) (Tree a)
  deriving (Show, Eq)
type RList a = [Digit a]

type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty [] = True
  isEmpty _ = False

  -- 詳細は consTree に
  cons :: a -> RList a -> RList a
  cons x ts = consTree (Leaf x) ts

  -- どのパターンも定数回のパターンマッチ
  head :: RList a -> a
  head (One (Leaf x) : _) = x
  head (Two (Leaf x) _ : _) = x
  head (Three (Leaf x) _ _ : _) = x

  -- 詳細は unconsTree に
  tail :: RList a -> RList a
  tail ts = let (_, ts') = unconsTree ts in ts'

  lookup = undefined
  update = undefined

-- helper functions

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

-- Zero, One, Two は自明
-- 補助関数 link も定数
-- Three の場合、 p.121 の O(1 + 1/2 + 1/4 + ...) = O(1) の償却コスト
consTree :: Tree a -> RList a -> RList a
consTree t [] = [One t]
consTree t1 (One t2 : ts) = Two t1 t2 : ts
consTree t1 (Two t2 t3 : ts) = Three t1 t2 t3 : ts
consTree t1 (Three t2 t3 t4 : ts) = Two t1 t2 : consTree (link t3 t4) ts

-- Zero, Two, Three は自明
-- One の場合、 p.121 の O(1 + 1/2 + 1/4 + ...) = O(1) の償却コスト
unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [One t] = (t, [])
unconsTree (One t : ts) = (t, Two t1 t2 : ts')
  where
    (Node _ t1 t2, ts') = unconsTree ts
unconsTree (Two t1 t2 : ts) = (t1, One t2 : ts)
unconsTree (Three t1 t2 t3 : ts) = (t1, Two t2 t3 : ts)
