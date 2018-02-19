module PFDS.Commons.BinaryRandomAccessListSpec where

import Test.Hspec
import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup)

spec :: Spec
spec = do
  describe "cons" $ do
    it "cons 1 into empty" $
      cons 1 (empty :: RList Int) `shouldBe` [One (Leaf 1)]
    it "cons 2 into [1]" $
      cons 2 [One (Leaf 1)] `shouldBe` [Zero, One (Node 2 (Leaf 2) (Leaf 1))]
  describe "tail" $
    it "tail of \"abc\"" $
      tail [One (Leaf 'a'), One (Node 2 (Leaf 'b') (Leaf 'c'))]
        `shouldBe` [Zero, One (Node 2 (Leaf 'b') (Leaf 'c'))]
  describe "update" $
    it "update 'a' in \"abc\" !! 2" $
      update 2 'a' [One (Leaf 'a'), One (Node 2 (Leaf 'b') (Leaf 'c'))]
        `shouldBe` [One (Leaf 'a'), One (Node 2 (Leaf 'b') (Leaf 'a'))]
