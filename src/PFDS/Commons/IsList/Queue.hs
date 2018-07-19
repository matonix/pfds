{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module PFDS.Commons.IsList.Queue where

import PFDS.Commons.Queue
import Data.List (unfoldr)
import Prelude hiding (head, tail)

class Queue l => IsList l where
  toList :: l a -> [a]
  fromList :: [a] -> l a

instance Queue l => IsList l where
  toList = unfoldr $ \ps -> if isEmpty ps
    then Nothing
    else Just (head ps, tail ps)
  fromList = foldl snoc empty
