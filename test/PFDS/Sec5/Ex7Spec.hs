module PFDS.Sec5.Ex7Spec
    (
    spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import PFDS.Sec5.Ex7
import qualified Data.List as L (sort)

spec :: Spec
spec =
  describe "sort" $ do
    it "sort a list" $
      show (sort [1,3,7,5,8,2,4,9,6,0]) `shouldBe` "[0,1,2,3,4,5,6,7,8,9]"
    prop "should have same bahavior as Data.List.sort"
      prop_sort

prop_sort :: [Int] -> Bool
prop_sort xs = sort xs == L.sort xs
