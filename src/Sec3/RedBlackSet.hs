module Sec3.RedBlackSet where

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

main :: IO ()
main = mapM_ (print . foldr insert empty) [[(1::Int)..x] | x <- [1..10]]

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
