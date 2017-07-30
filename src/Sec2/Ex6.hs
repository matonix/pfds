{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sec2.Ex6 where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Control.Monad ((<$!>))

-- practice 2.6

data Map k v = Empty | Map (Map k v) (k, v) (Map k v) deriving (Show)

empty :: Map k v
empty = Empty

bind :: Ord k => k -> v -> Map k v -> Map k v
bind k v Empty = Map Empty (k, v) Empty
bind k v s@(Map _ y _) = fromMaybe s $ bind' k v s y
  where
    bind' :: Ord k => k -> v -> Map k v -> (k, v) -> Maybe (Map k v)
    bind' k v Empty (l,_) = if k == l then Nothing else Just $ Map Empty (k,v) Empty
    bind' k v (Map a (j,w) b) z
      | k <= j    = (\s -> Map s (j,w) b) <$!> bind' k v a (j,w)
      | otherwise = Map a (j,w) <$!> bind' k v b z

lookup :: Ord k => k -> Map k v -> v
lookup _ Empty = undefined
lookup x s@(Map _ y _) = lookup' x s y
  where
    lookup' x Empty (j,w) = if x == j then w else undefined
    lookup' x (Map a (j,w) b) z
      | x <= j    = lookup' x a (j,w)
      | otherwise = lookup' x b z
