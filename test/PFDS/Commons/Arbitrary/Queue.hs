{-# LANGUAGE FlexibleInstances #-}

module PFDS.Commons.Arbitrary.Queue where

import Test.QuickCheck
import PFDS.Commons.Queue
import Prelude hiding (tail)

instance {-# OVERLAPS #-} (Arbitrary a, Queue q) => Arbitrary (q a) where
  arbitrary = do
    ops <- genArbitraryOprs
    return $ foldr ($) empty ops

genArbitraryOprs :: (Arbitrary a, Queue q) => Gen [q a -> q a]
genArbitraryOprs = do
  a <- arbitrary
  listOf $ elements [flip snoc a, tail]
