{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PFDS.Sec2.Ex4 where

import Data.Maybe (fromMaybe)
import Control.Monad ((<$!>))

data Tree a = E | T (Tree a) a (Tree a) deriving (Show)

-- practice 2.4
insert3 :: Ord a => a -> Tree a -> Tree a
insert3 x E = T E x E
insert3 x s@(T _ y _) = fromMaybe s $ insert3' x s y
  where
    insert3' :: Ord a => a -> Tree a -> a -> Maybe (Tree a)
    insert3' x E z = if x == z then Nothing else Just $ T E x E
    insert3' x (T a y b) z
      | x <= y    = (\s -> T s y b) <$!> insert3' x a y
      | otherwise = T a y <$!> insert3' x b z
