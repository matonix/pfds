{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Commons.Heap where

class Heap h where
  empty :: Ord e => h e
  isEmpty :: Ord e => h e -> Bool
  insert :: Ord e => e -> h e -> h e
  merge :: Ord e => h e -> h e -> h e
  findMin :: Ord e => h e -> e
  deleteMin :: Ord e => h e -> h e

-- impl leftist heap
data LHeap e = E | T Int e (LHeap e) (LHeap e) deriving (Show)

instance Heap LHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) = if x <= y
    then makeT x a1 $ merge b1 h2
    else makeT y a2 $ merge h1 b2

  insert x = merge (T 1 x E E)

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
