{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec3.Heap where

class Ord e => Heap h e where
  empty :: h e
  isEmpty :: h e -> Bool
  insert :: e -> h e -> h e
  merge :: h e -> h e -> h e
  findMin :: h e -> e
  deleteMin :: h e -> h e

-- impl leftist heap
data LHeap e = E | T Int e (LHeap e) (LHeap e) deriving (Show)

instance Ord e => Heap LHeap e where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) = if x <= y
    then makeT x a1 $ merge b1 h2
    else makeT y a2 $ merge h1 b2

  insert x h = merge (T 1 x E E) h

  findMin E = error "Empty"
  findMin (T _ x _ _) = x

  deleteMin E = error "Empty"
  deleteMin (T _ _ a b) = merge a b

-- 補助関数

rank :: LHeap e -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: e -> LHeap e -> LHeap e -> LHeap e
makeT x' a b = if rank a >= rank b
  then T (rank b + 1) x' a b
  else T (rank a + 1) x' b a
