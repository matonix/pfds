{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PFDS.Sec2.Ex3 where

import Data.Maybe (fromMaybe)
import Control.Monad ((<$!>))

data Tree a = E | T (Tree a) a (Tree a) deriving (Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x E = T E x E
insert x s@(T a y b)
  | x < y     = T (insert x a) y b
  | x > y     = T a y (insert x b)
  | otherwise = s

-- practice 2.3
insert2 :: Ord a => a -> Tree a -> Tree a
insert2 x E = T E x E
insert2 x s = fromMaybe s $ insert2' x s
  where
    insert2' :: Ord a => a -> Tree a -> Maybe (Tree a)
    insert2' x E = Just (T E x E)
    insert2' x (T a y b)
      | x < y     = (\s -> T s y b) <$!> insert2' x a
      | x > y     = T a y <$!> insert2' x b
      | otherwise = Nothing
