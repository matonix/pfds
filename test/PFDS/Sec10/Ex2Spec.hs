module PFDS.Sec10.Ex2Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import PFDS.Commons.RandomAccessListNoFamily
import PFDS.Sec10.Ex2
import Prelude hiding (head, tail, lookup)

spec :: Spec
spec =
  describe "insert" $
    prop "forall x, xs. toList (cons x (fromList xs)) == x : xs" prop_insert
    -- prop "forall xs. deletes xs (inserts xs empty) == empty" prop_delete

-- prop_delete :: [Int] -> Bool
-- prop_delete xs = foldr delete (foldr insert empty xs) xs == empty

prop_insert :: Int -> [Int] -> Bool
prop_insert x xs = toList (cons x (fromList xs)) == x : xs
