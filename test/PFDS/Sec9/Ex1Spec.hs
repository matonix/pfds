{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec9.Ex1Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Exception
import Test.QuickCheck.Property
import Test.Hspec.QuickCheck (prop)
import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup, drop)

import PFDS.Sec9.Ex1

spec :: Spec
spec = do
  describe "dropNaive" $ do
    it "drops one" $
      dropNaive 1 [One (Leaf 1)] `shouldBe` empty
    it "drops two" $
      dropNaive 2 [One (Leaf 'a'), One (Node 2 (Leaf 'b') (Leaf 'c'))]
        `shouldBe` [One (Leaf 'c')]
  describe "drop" $
    prop "forall n xs. drop n xs == dropNaive n xs" prop_drop
  describe "drop'" $
    prop "forall n xs. drop' n xs == dropNaive n xs" prop_drop'

prop_drop :: Int -> RList Int -> Property
prop_drop n xs = ioProperty $
  (===) <$> tryDef (drop n xs) <*> tryDef (dropNaive n xs)

prop_drop' :: Int -> RList Int -> Property
prop_drop' n xs = ioProperty $
  (===) <$> tryDef (drop' n xs) <*> tryDef (dropNaive n xs)

-- https://qiita.com/waddlaw/items/fad80832cfc60a56d7a2
tryDef :: [t] -> IO [t]
tryDef a = either (const []) id <$> tryEvaluate a

instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (RList a) where
  arbitrary = do
    ops <- genArbitraryOprs
    return $ foldr ($) empty ops

genArbitraryOprs :: Arbitrary a => Gen [RList a -> RList a]
genArbitraryOprs = do
  a <- arbitrary
  listOf $ elements [cons a, tail]
