module PFDS.Sec9.Ex1Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)
import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (drop)
import PFDS.Sec9.Ex1

spec :: Spec
spec = do
  describe "dropNaive" $ do
    it "drops one" $
      dropNaive 1 [One (Leaf 1)] `shouldBe` empty
    it "drops two" $
      dropNaive 2 [One (Leaf 'a'), One (Node 2 (Leaf 'b') (Leaf 'c'))]
        `shouldBe` [One (Leaf 'c')]
  describe "drop" $
    prop "equivalent to dropNaive" prop_drop

prop_drop :: RList a -> Bool
prop_drop xs = drop xs == dropNaive xs

-- TODO: make RList an instance of Arbitary
-- 任意のデータではなく特定の操作列から生成されうる
-- データ集合を作る時、操作列を生成して適用するか、
-- データのwell formed性をチェックする関数を書いて
-- ふるいに掛けるか。って書いてて後者が良さそうな気がしてきた
