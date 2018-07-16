{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module PFDS.Commons.IsList where

import PFDS.Commons.RandomAccessListNoFamily
import Data.List (unfoldr)
import Prelude hiding (head, tail, lookup)

class RandomAccessList l => IsList l where
  toList :: l a -> [a]
  fromList :: [a] -> l a

instance RandomAccessList l => IsList l where
  toList = unfoldr $ \ps -> if isEmpty ps
    then Nothing
    else Just (head ps, tail ps)

  fromList = foldr cons empty
