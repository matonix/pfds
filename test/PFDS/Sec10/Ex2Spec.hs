module PFDS.Sec10.Ex2Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

import PFDS.Commons.IsList.RandomAccessListNoFamily
import qualified PFDS.Commons.RandomAccessListNoFamily as RList
import PFDS.Sec10.Ex2

spec :: Spec
spec = do
  describe "to/fromList" $
    prop "toList . fromList == id" prop_ToList_fromList
  describe "isEmpty" $
    prop "isEmpty is equivalent to null" prop_isEmpty
  describe "cons" $
    prop "forall x. cons x is equivalent to (x :)" prop_cons
  describe "head" $
    prop "head is equivalent to Prelude.head" prop_head
  describe "tail" $
    prop "tail is equivalent to Prelude.tail" prop_tail
  describe "lookup" $
    prop "forall i. lookup i is equivalent to (!! i)" prop_lookup
  describe "update" $
    prop "forall i, x, xs. update i x xs is equivalent to take i xs ++ x : drop (i + 1) xs" prop_update

prop_ToList_fromList :: [Integer] -> Property
prop_ToList_fromList xs =
  toList (fromList xs :: RList Integer) === xs

prop_isEmpty :: [Integer] -> Property
prop_isEmpty xs =
  RList.isEmpty (fromList xs :: RList Integer) === null xs

prop_cons :: Integer -> [Integer] -> Property
prop_cons x xs =
  toList (RList.cons x (fromList xs :: RList Integer)) === x : xs

prop_head :: NonEmptyList Integer -> Property
prop_head (NonEmpty xs) =
  RList.head (fromList xs :: RList Integer) === head xs

prop_tail :: NonEmptyList Integer -> Property
prop_tail (NonEmpty xs) =
  toList (RList.tail (fromList xs :: RList Integer)) === tail xs

prop_lookup :: NonNegative Int -> NonEmptyList Integer -> Property
prop_lookup (NonNegative i) (NonEmpty xs) = i < length xs ==>
  RList.lookup i (fromList xs :: RList Integer) === xs !! i

prop_update :: NonNegative Int -> Integer -> NonEmptyList Integer -> Property
prop_update (NonNegative i) x (NonEmpty xs) = i < length xs ==>
  toList (RList.update i x (fromList xs :: RList Integer)) ===
    take i xs ++ x : drop (i + 1) xs
