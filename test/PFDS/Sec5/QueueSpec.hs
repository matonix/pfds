module PFDS.Sec5.QueueSpec
    (
    spec
    ) where

import Test.Hspec
import PFDS.Sec5.Queue

spec :: Spec
spec =
  describe "snoc" $ do
    it "snoc 1 into empty" $
      show (snoc (empty :: BatchedQueue Int) 1) `shouldBe` "Q [1] []"
    it "snoc [1..10] into empty" $
      show (foldl snoc (empty :: BatchedQueue Int) [1..10]) `shouldBe` "Q [1] [10,9,8,7,6,5,4,3,2]"
