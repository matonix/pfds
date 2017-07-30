{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sec2.Ex5 where

data Tree a = E | T (Tree a) a (Tree a) deriving (Show)

-- practice 2.5a
complete :: Ord a => a -> Int -> Tree a
complete _ 0 = E
complete x d = T t x t where t = complete x (d-1)

-- practice 2.5b
balanced :: Ord a => a -> Int -> Tree a
balanced _ 0 = E
balanced x 1 = T E x E
balanced x m = T (balanced x m'') x (balanced x m')
  where
    m' = (m - 1) `div` 2
    m'' = m - 1 - m'
