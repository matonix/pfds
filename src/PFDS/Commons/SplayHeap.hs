module PFDS.Commons.SplayHeap where

import PFDS.Commons.Heap (Heap (..))

data SplayHeap e = E | T (SplayHeap e) e (SplayHeap e) deriving (Show)

instance Heap SplayHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x t = T a x b where
    (a, b) = partition x t

  findMin E = error "Empty"
  findMin (T E x _) = x
  findMin (T a _ _) = findMin a

  deleteMin E = error "Empty"
  deleteMin (T E _ b)         = b
  deleteMin (T (T E _ b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)

  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b) where
    (ta, tb) = partition x t

partition :: Ord e => e -> SplayHeap e -> (SplayHeap e, SplayHeap e)
partition _ E = (E, E)
partition pivot t@(T a x b) =
  if x <= pivot
    then case b of
      E -> (t, E)
      T b1 y b2 -> if y <= pivot
        then
          let (small, big) = partition pivot b2
          in (T (T a x b1) y small, big)
        else
          let (small, big) = partition pivot b1
          in (T a x small, T big y b2)
    else case a of
      E -> (E, t)
      T a1 y a2 -> if y <= pivot
        then
          let (small, big) = partition pivot a2
          in (T a1 y small, T big x b)
        else
          let (small, big) = partition pivot a1
          in (small, T big y (T a2 x b))
