module PFDS.Sec8.Ex1Spec
    (
    spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import PFDS.Sec8.Ex1

spec :: Spec
spec = do
  let tree1to5 = RBTree (T B (T R (T B E 1 E L) 2 (T B E 3 E L) L) 4 (T B E 5 E L) L) 5 0
  describe "insert" $ do
    it "insert 1 into empty" $
      insert 1 (empty :: RBTree Int)
        `shouldBe` RBTree (T B E 1 E L) 1 0
    it "insert [1..5] into empty" $
      foldr insert (empty :: RBTree Int) [1..5]
        `shouldBe` tree1to5
  describe "delete" $ do
    it "delete 1 from [1..5]" $
      delete 1 tree1to5
        `shouldBe` RBTree (T B (T R (T B E 1 E D) 2 (T B E 3 E L) L) 4 (T B E 5 E L) L) 5 1
    it "delete [1..2] from [1..5] (should not be rebuild)" $
      foldr delete tree1to5 [1..2]
        `shouldBe` RBTree (T B (T R (T B E 1 E D) 2 (T B E 3 E L) D) 4 (T B E 5 E L) L) 5 2
    it "delete [1..3] from [1..5] (should be rebuild)" $
      foldr delete tree1to5 [1..3]
        `shouldBe` RBTree (T B (T R E 4 E L) 5 E L) 2 0
    it "delete [1..5] from [1..5] (should be empty)" $
      foldr delete tree1to5 [1..5]
        `shouldBe` empty
    prop "forall xs. deletes xs (inserts xs empty) == empty" prop_delete

prop_delete :: [Int] -> Bool
prop_delete xs = foldr delete (foldr insert empty xs) xs == empty
