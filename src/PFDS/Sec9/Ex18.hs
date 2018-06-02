{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- NonZero4aryRandomAccessList
module PFDS.Sec9.Ex18 where

import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V
import PFDS.Commons.RandomAccessList
import Prelude hiding (head, tail, lookup)

data Tree a = Leaf a | Node (Vector (Tree a))
--                           ^^^^^^ 4 trees
type RList a = [Vector (Tree a)]
--              ^^^^^^ 1 ~ 4 trees

type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty = null

  cons :: a -> RList a -> RList a
  cons x vs = consTree (Leaf x) vs

  head :: RList a -> a -- raise Empty
  head vs = let (Leaf x, _) = unconsTree vs in x

  tail :: RList a -> RList a -- raise Empty
  tail vs = let (_, vs') = unconsTree vs in vs'

  lookup :: Int -> RList a -> a -- raise Subscript
  lookup i vs = lookupList i 1 vs

  update :: Int -> a -> RList a -> RList a -- raise Subscript
  update i y vs = updateList i 1 y vs

-- helper

consTree :: Tree a -> RList a -> RList a
consTree t [] = [V.singleton t]
consTree t (v : vs) = if V.length v < 4
  then V.cons t v : vs
  else V.singleton t : consTree (Node v) vs

unconsTree :: RList a -> (Tree a, RList a)
unconsTree [] = error "Empty"
unconsTree [v] | V.length v == 1 = (V.head v, [])
unconsTree (v : vs) = if V.length v > 1
  then (V.head v, V.tail v : vs)
  else let (Node v', vs') = unconsTree vs in (V.head v, V.tail v : vs')

lookupList :: Int -> Int -> RList a -> a
lookupList i w [] = error "Subscript"
lookupList i w (v : vs) = if i <= w * V.length v
  then lookupVector i w v
  else lookupList (i - w * V.length v) (w * 4) vs

lookupVector :: Int -> Int -> Vector (Tree a) -> a
lookupVector i w v = let j = i `div` w in case v !? j of
  Nothing -> error "Subscript"
  Just (Leaf x) -> x
  Just (Node v') -> lookupVector (i - j) (w `div` 4) v'

updateList :: Int -> Int -> a -> RList a -> RList a
updateList i w y [] = error "Subscript"
updateList i w y (v : vs) = if i <= w * V.length v
  then updateVector i w y v : vs
  else v : updateList (i - w * V.length v) (w * 4) y vs

updateVector :: Int -> Int -> a -> Vector (Tree a) -> Vector (Tree a)
updateVector i w y v = let j = i `div` w in case v !? j of
  Nothing -> error "Subscript"
  Just (Leaf _) -> v // [(j, Leaf y)]
  Just (Node v') -> updateVector (i - j) (w `div` 4) y v'
