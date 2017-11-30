module PFDS.Sec8.Ex1 where

-- RedBlackTree with delete (batched rebuilding)

data Color = R | B deriving (Show)
data Vital = L | D deriving (Show)
data Tree e = E | T Color (Tree e) e (Tree e) Vital deriving (Show)
data BRTree e = BRTree (Tree e) Livings Deads
type Livings = Int
type Deads = Int

empty :: BRTree e
empty = BRTree E 0 0

member :: Ord e => e -> BRTree e -> Bool
member x (BRTree t _ _) = member' x t
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

insert :: Ord e => e -> BRTree e -> BRTree e
insert x (BRTree s l d) = BRTree (T B a' y' b' v') (l + 1) d
  where
    T _ a' y' b' v' = ins s
    ins E = T R E x E L
    ins s'@(T color a y b v)
      | x < y = balance color (ins a) y b v
      | x > y = balance color a y (ins b) v
      | otherwise = s'

delete :: (Ord e, Show e, Read e) => e -> BRTree e -> BRTree e
delete x (BRTree s l d) = if l > (d + 1) * 2 then rebuild t else t
  where
    t = BRTree (del s) l (d + 1)
    del E = error "not found"
    del s'@(T color a y b v)
      | x < y = del a
      | x > y = del b
      | otherwise = T color a x b D

rebuild :: (Ord e, Show e, Read e) => BRTree e -> BRTree e
rebuild (BRTree s l d) = BRTree (fromOrdList (toOrdList s)) (l-d) 0

-- 禁じ手
toOrdList :: (Ord e, Show e, Read e) => Tree e -> [e]
toOrdList = map read . filter ((`notElem` "TELD()") . head) . filter ((==1) . length) . words . show

-- from Sec3.Ex9
fromOrdList :: Ord e => [e] -> Tree e
fromOrdList xs' = (\(x,_,_) -> x) $ go d' r' xs' where
  n = length xs'
  d' = until (\x -> 2^(x+1)-1 >= n) succ 0
  r' = n - 2^d' + 1
  go :: Ord e => Int -> Int -> [e] -> (Tree e, [e], Int)
  go _ r [] = (E, [], r)
  go d r xs0@(x:xs)
    | d == 1, r == 0 = (T B E x E L, xs, r)
    | d == 0, r > 0 = (T R E x E L, xs, r-1)
    | d == 0 = (E, xs0, r)
    | otherwise = (T B t1 x1 t2 L, xs2, r2) where
      (t1, x1:xs1, r1) = go (d-1) r xs0
      (t2, xs2, r2) = go (d-1) r1 xs1


-- main :: IO ()
-- main = mapM_ (print . foldr insert empty) [[(1::Int)..x] | x <- [1..10]]

-- T B E 1 E
-- T B (T R E 1 E) 2 E
-- T B (T B E 1 E) 2 (T B E 3 E)
-- T B (T B (T R E 1 E) 2 E) 3 (T B E 4 E)
-- T B (T R (T B E 1 E) 2 (T B E 3 E)) 4 (T B E 5 E)
-- T B (T R (T B (T R E 1 E) 2 E) 3 (T B E 4 E)) 5 (T B E 6 E)
-- T B (T B (T B E 1 E) 2 (T B E 3 E)) 4 (T B (T B E 5 E) 6 (T B E 7 E))
-- T B (T B (T B (T R E 1 E) 2 E) 3 (T B E 4 E)) 5 (T B (T B E 6 E) 7 (T B E 8 E))
-- T B (T B (T R (T B E 1 E) 2 (T B E 3 E)) 4 (T B E 5 E)) 6 (T B (T B E 7 E) 8 (T B E 9 E))
-- T B (T B (T R (T B (T R E 1 E) 2 E) 3 (T B E 4 E)) 5 (T B E 6 E)) 7 (T B (T B E 8 E) 9 (T B E 10 E))
