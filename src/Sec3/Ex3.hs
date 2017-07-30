{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec3.Ex3 where

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

  insert x = merge (T 1 x E E)

  findMin E = error "Empty"
  findMin (T _ x _ _) = x

  deleteMin E = error "Empty"
  deleteMin (T _ _ a b) = merge a b

--

rank :: LHeap e -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: e -> LHeap e -> LHeap e -> LHeap e
makeT x' a b = if rank a >= rank b
  then T (rank b + 1) x' a b
  else T (rank a + 1) x' b a

-- 演習問題 3.3
fromList :: Ord e => [e] -> LHeap e
fromList = loop . map (\x -> T 1 x E E)

loop :: Ord e => [LHeap e] -> LHeap e
loop [] = empty
loop [h] = h
loop hs@(_:_:_) = loop $ pairs hs

pairs :: Ord e => [LHeap e] -> [LHeap e]
pairs [] = []
pairs [h] = [h]
pairs (h1:h2:hs) = merge h1 h2 : pairs hs

-- 証明
-- n 個の左偏ヒープをマージすることを考える。
-- マージは 2 の左偏ヒープを 1 つにする操作なので、
-- 1 回のマージで左偏ヒープの個数は 1 つ減ることになる。
-- これを 1 個の左偏ヒープになるまで繰り返すので、
-- マージ回数は n-1 回、すなわち O(n) となる。

-- ちなみに、マージされずに余った左偏ヒープは、
-- loop において偶数個の左偏ヒープになるまで
-- merge されずに pairs によって引き上げられるが、
-- 引き上げる回数は高々 log n 回なので
-- オーダーに影響しない。
