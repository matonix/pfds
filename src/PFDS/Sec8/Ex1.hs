module PFDS.Sec8.Ex1 where

-- RedBlackTree with delete (batched rebuilding)

data Color = R | B deriving (Show, Eq)
data Vital = L | D deriving (Show, Eq)
data Tree e = E | T Color (Tree e) e (Tree e) Vital deriving (Show, Eq)
data RBTree e = RBTree (Tree e) Livings Deads deriving (Show, Eq)
type Livings = Int
type Deads = Int

empty :: RBTree e
empty = RBTree E 0 0

member :: Ord e => e -> RBTree e -> Bool
member x (RBTree t _ _) = member' x t
  where
    member' :: Ord e => e -> Tree e -> Bool
    member' _ E = False
    member' x (T _ a y b _)
      | x < y = member' x a
      | x > y = member' x b
      | otherwise = True

balance :: Color -> Tree e -> e -> Tree e -> Vital -> Tree e
balance B (T R (T R a x b u) y c v) z d w = T R (T B a x b u) y (T B c z d v) w
balance B (T R a x (T R b y c u) v) z d w = T R (T B a x b u) y (T B c z d v) w
balance B a x (T R (T R b y c u) z d v) w = T R (T B a x b u) y (T B c z d v) w
balance B a x (T R b y (T R c z d u) v) w = T R (T B a x b u) y (T B c z d v) w
balance color e w f v = T color e w f v

insert :: Ord e => e -> RBTree e -> RBTree e
insert x (RBTree s l d) = RBTree (T B a' y' b' v') (l + 1) d
  where
    T _ a' y' b' v' = ins s
    ins E = T R E x E L
    ins s'@(T color a y b v)
      | x < y = balance color (ins a) y b v
      | x > y = balance color a y (ins b) v
      | otherwise = s'

delete :: Ord e => e -> RBTree e -> RBTree e
delete x (RBTree s l d) = if l < (d + 1) * 2 then rebuild t else t
  where
    t = RBTree (del s) l (d + 1)
    del E = E
    del s'@(T color a y b v)
      | x < y = T color (del a) y b v
      | x > y = T color a y (del b) v
      | otherwise = T color (del a) x (del b) D

rebuild :: Ord e => RBTree e -> RBTree e
rebuild (RBTree s l d) = RBTree (fromOrdList s') (length s') 0
  where
    s' = toOrdList s

toOrdList :: Ord e => Tree e -> [e]
toOrdList t' = go t' []
  where
    go E xs = xs
    go (T _ a x b L) xs = go a (x:go b xs)
    go (T _ a _ b D) xs = go a (go b xs)

-- from Sec3.Ex9
fromOrdList :: Ord e => [e] -> Tree e
fromOrdList xs' = (\(x,_,_) -> x) $ go d' r' xs'
  where
    n = length xs'
    d' = until (\x -> 2^(x+1)-1 >= n) succ 0
    r' = n - 2^d' + 1
    go :: Ord e => Int -> Int -> [e] -> (Tree e, [e], Int)
    go _ r [] = (E, [], r)
    go d r xs0@(x:xs)
      | d == 1, r == 0 = (T B E x E L, xs, r)
      | d == 0, r > 0 = (T R E x E L, xs, r-1)
      | d == 0 = (E, xs0, r)
      | otherwise = (T B t1 x1 t2 L, xs2, r2)
      where
        (t1, x1:xs1, r1) = go (d-1) r xs0
        (t2, xs2, r2) = go (d-1) r1 xs1

-- 最短と最長で深さの差が1以下の平衡二分木を作ることを考える。
-- 最短パスの長さ d を設定する。d はノード数 n の平衡二分木における深さ最短のパスの長さである。
-- 深さ d の完全二分木を作ることを考え、余ったノードが r 個存在するとする。
-- 深さ d の完全二分木は全て黒ノードであり、余ったノードは最左の子から順に赤のノードとして付加する。
