module PFDS.Sec5.Ex4 where

import PFDS.Sec3.Heap (Heap (..))

data SplayHeap e = E | T (SplayHeap e) e (SplayHeap e)

instance Heap SplayHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x t = T (smaller x t) x (bigger x t)

  findMin (T E x _) = x
  findMin (T a _ _) = findMin a

  deleteMin (T E _ b)         = b
  deleteMin (T (T E x b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)

  merge E t = t
  merge (T a x b) t =
    T (merge (smaller x t) a) x (merge (bigger x t) b)

bigger :: Ord e => e -> SplayHeap e -> SplayHeap e
bigger pivot E = E
bigger pivot (T a x b) =
  if x <= pivot
    then bigger pivot b
    else case a of
      E -> T E x b
      T a1 y a2 -> if y <= pivot
        then T (bigger pivot a2) x b
        else T (bigger pivot a1) y (T a2 x b)

smaller :: Ord e => e -> SplayHeap e -> SplayHeap e
smaller pivot E = E
smaller pivot (T a x b) =
  if x > pivot
    then smaller pivot a
    else case b of
      E -> T a x E
      T b1 y b2 -> if y > pivot
        then T a x (smaller pivot b1)
        else T (T a x b1) y (smaller pivot b2)
