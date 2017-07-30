{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PFDS.Sec2.Ex2 where

data Tree a = E | T (Tree a) a (Tree a) deriving (Show)

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T a y b)
  | x < y     = member x a
  | x > y     = member x b
  | otherwise = True

-- practice 2.2
member2 :: Ord a => a -> Tree a -> Bool
member2 _ E = False
member2 x (T a y b) = member2' x (T a y b) y
  where
    member2' _ E z = x == z
    member2' x (T a y b) z
      | x <= y    = member2' x a y
      | otherwise = member2' x b z
