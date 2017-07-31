module PFDS.Sec5.Ex1Spec
    (
    spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import PFDS.Sec5.Ex1

spec :: Spec
spec =
  describe "check" $
    prop "if q has >=2 elems, f & r shouldn't be empty" prop_check

prop_check :: ([Int], [Int]) -> Bool
prop_check (f, r) =
  if length f + length r >= 2
    then not (null f') && not (null r')
    else True
  where
    Q f' r' = check f r
