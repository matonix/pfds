module PFDS.Sec9.Ex3Spec where

import Test.Hspec
import PFDS.Commons.RandomAccessList
import PFDS.Sec9.Ex3
import Prelude hiding (head, tail, lookup)

spec :: Spec
spec = do
  describe "cons" $ do
    it "cons 1 into empty" $
      cons 1 (empty :: RList Int)
        `shouldBe` [Leaf 1]
    it "cons 2 into [1]" $
      cons 2 [Leaf 1]
        `shouldBe` [Node 2 (Leaf 2) (Leaf 1)]
  describe "tail" $
    it "tail of \"abc\"" $
      tail [Leaf 'a', Node 2 (Leaf 'b') (Leaf 'c')]
        `shouldBe` [Node 2 (Leaf 'b') (Leaf 'c')]
  describe "lookup" $
    it "lookup 2 in \"abc\"" $
      lookup 2 [Leaf 'a', Node 2 (Leaf 'b') (Leaf 'c')]
        `shouldBe` 'c'
  describe "update" $
    it "update 'a' in \"abc\" !! 2" $
      update 2 'a' [Leaf 'a', Node 2 (Leaf 'b') (Leaf 'c')]
        `shouldBe` [Leaf 'a', Node 2 (Leaf 'b') (Leaf 'a')]
