{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec9.Ex2Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Exception
import Test.QuickCheck.Property
import Test.Hspec.QuickCheck (prop)
import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup, drop)
import PFDS.Sec9.Ex1Spec

import PFDS.Sec9.Ex2

spec :: Spec
spec = do
  describe "createNaive" $ do
    it "create one" $
      createNaive 1 'a'
        `shouldBe` [One (Leaf 'a')]
    it "create two" $
      createNaive 2 'b'
        `shouldBe` [Zero, One (Node 2 (Leaf 'b') (Leaf 'b'))]
    it "create three" $
      createNaive 3 'c'
        `shouldBe` [One (Leaf 'c'), One (Node 2 (Leaf 'c') (Leaf 'c'))]
    it "create zero" $
      createNaive 0 'a'
        `shouldBe` []
  describe "create" $
    prop "forall n x. create n x == createNaive n x" prop_create
  describe "create2" $
    prop "forall n x. create2 n x == createNaive n x" prop_create

prop_create :: Char -> Property
prop_create x = forAll (choose (1, 100)) $ \n ->
  create n x === createNaive n x

prop_create2 :: Char -> Property
prop_create2 x = forAll (choose (1, 100)) $ \n ->
  create2 n x === createNaive n x
