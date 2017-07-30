module Sec3.Ex9 where

data Color = R | B deriving (Show)
data Tree e = E | T Color (Tree e) e (Tree e) deriving (Show)

empty :: Tree e
empty = E

member :: Ord e => e -> Tree e -> Bool
member _ E = False
member x (T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

-- https://stackoverflow.com/questions/24700762/or-patterns-in-haskell
balance :: Color -> Tree e -> e -> Tree e -> Tree e
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color e w f = T color e w f

insert :: Ord e => e -> Tree e -> Tree e
insert x s = T B a' y' b' where
  T _ a' y' b' = ins s
  ins E = T R E x E
  ins s'@(T color a y b)
    | x < y = balance color (ins a) y b
    | x > y = balance color a y (ins b)
    | otherwise = s'

fromOrdList :: Ord e => [e] -> Tree e
fromOrdList xs' = (\(x,_,_) -> x) $ go d' r' xs' where
  n = length xs'
  d' = until (\x -> 2^(x+1)-1 >= n) succ 0
  r' = n - 2^d' + 1
  go :: Ord e => Int -> Int -> [e] -> (Tree e, [e], Int)
  go _ r [] = (E, [], r)
  go d r xs0@(x:xs)
    | d == 1, r == 0 = (T B E x E, xs, r)
    | d == 0, r > 0 = (T R E x E, xs, r-1)
    | d == 0 = (E, xs0, r)
    | otherwise = (T B t1 x1 t2, xs2, r2) where
      (t1, x1:xs1, r1) = go (d-1) r xs0
      (t2, xs2, r2) = go (d-1) r1 xs1

main :: IO ()
main = mapM_ (print . fromOrdList) [[(1::Int)..x] | x <- [1..10]]

-- 最短と最長で深さの差が1以下の平衡二分木を作ることを考える。
-- 最短パスの長さ d を設定する。d はノード数 n の平衡二分木における深さ最短のパスの長さである。
-- 深さ d の完全二分木を作ることを考え、余ったノードが r 個存在するとする。
-- 深さ d の完全二分木は全て黒ノードであり、余ったノードは最左の子から順に赤のノードとして付加する。

-- T R E 1 E
-- T B (T R E 1 E) 2 E
-- T B (T R E 1 E) 2 (T R E 3 E)
-- T B (T B (T R E 1 E) 2 E) 3 (T B E 4 E)
-- T B (T B (T R E 1 E) 2 (T R E 3 E)) 4 (T B E 5 E)
-- T B (T B (T R E 1 E) 2 (T R E 3 E)) 4 (T B (T R E 5 E) 6 E)
-- T B (T B (T R E 1 E) 2 (T R E 3 E)) 4 (T B (T R E 5 E) 6 (T R E 7 E))
-- T B (T B (T B (T R E 1 E) 2 E) 3 (T B E 4 E)) 5 (T B (T B E 6 E) 7 (T B E 8 E))
-- T B (T B (T B (T R E 1 E) 2 (T R E 3 E)) 4 (T B E 5 E)) 6 (T B (T B E 7 E) 8 (T B E 9 E))
-- T B (T B (T B (T R E 1 E) 2 (T R E 3 E)) 4 (T B (T R E 5 E) 6 E)) 7 (T B (T B E 8 E) 9 (T B E 10 E))
