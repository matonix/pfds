module PFDS.Sec10.Ex4Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

import PFDS.Commons.IsList.Queue
import qualified PFDS.Commons.Queue as Q
import PFDS.Sec10.Ex4
import qualified PFDS.Sec10.BootstrappedQueue as RI
import PFDS.Commons.Arbitrary.Queue

spec :: Spec
spec = do
  describe "to/fromList" $ do
    prop "toList . fromList == id" prop_toList_fromList
    prop "BQueue is equivalent to RI.BQueue" prop_toList_fromList_RI
  describe "isEmpty" $
    prop "isEmpty is equivalent to null" prop_isEmpty
  describe "snoc" $
    prop "forall x. snoc x is equivalent to (++ [x])" prop_snoc
  describe "head" $
    prop "head is equivalent to Prelude.head" prop_head
  describe "tail" $
    prop "tail is equivalent to Prelude.tail" prop_tail

prop_toList_fromList :: [Integer] -> Property
prop_toList_fromList xs =
  toList (fromList xs :: BQueue Integer) === xs

prop_toList_fromList_RI :: [Integer] -> Property
prop_toList_fromList_RI xs =
  toList (fromList xs :: BQueue Integer) === toList (fromList xs :: RI.BQueue Integer)

prop_isEmpty :: [Integer] -> Property
prop_isEmpty xs =
  Q.isEmpty (fromList xs :: BQueue Integer) === null xs

prop_snoc :: Integer -> [Integer] -> Property
prop_snoc x xs =
  toList (Q.snoc (fromList xs :: BQueue Integer) x) === xs ++ [x]

prop_head :: NonEmptyList Integer -> Property
prop_head (NonEmpty xs) =
  Q.head (fromList xs :: BQueue Integer) === head xs

prop_tail :: NonEmptyList Integer -> Property
prop_tail (NonEmpty xs) =
  toList (Q.tail (fromList xs :: BQueue Integer)) === tail xs
