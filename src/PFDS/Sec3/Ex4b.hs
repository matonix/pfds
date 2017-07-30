{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module PFDS.Sec3.Ex4b where

class Ord e => Heap h e where
  empty :: h e
  isEmpty :: h e -> Bool
  insert :: e -> h e -> h e
  merge :: h e -> h e -> h e
  findMin :: h e -> e
  deleteMin :: h e -> h e

-- impl weight-biased leftist heap
data WLHeap e = E | T Int e (WLHeap e) (WLHeap e) deriving (Show)

instance Ord e => Heap WLHeap e where
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

size :: WLHeap e -> Int
size E = 0
size (T s _ _ _) = s

-- 演習課題3.4b
makeT :: e -> WLHeap e -> WLHeap e -> WLHeap e
makeT x' a b = if size a >= size b
  then T (size a + size b + 1) x' a b
  else T (size a + size b + 1) x' b a

-- 演習問題 3.3
fromList :: Ord e => [e] -> WLHeap e
fromList = loop . map (\x -> T 1 x E E)

loop :: Ord e => [WLHeap e] -> WLHeap e
loop [] = empty
loop [h] = h
loop hs@(_:_:_) = loop $ pairs hs

pairs :: Ord e => [WLHeap e] -> [WLHeap e]
pairs [] = []
pairs [h] = [h]
pairs (h1:h2:hs) = merge h1 h2 : pairs hs

t1 :: WLHeap Int
t1 = fromList $ map (*2) [1..100000]
t2 :: WLHeap Int
t2 = fromList [1..100000]

nyaos :: IO ()
nyaos = do
  let !_ = merge t1 t2
  putStrLn "nyaos"
