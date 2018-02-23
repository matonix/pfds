module PFDS.Sec9.Ex1 where

import qualified PFDS.Commons.RandomAccessList as R
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup, drop)
import Data.List (unfoldr)

-- naive impl
dropNaive :: Int -> RList a -> RList a
dropNaive 0 xs = xs
dropNaive i xs = dropNaive (i - 1) (R.tail xs)

drop :: Int -> RList a -> RList a
drop x = dropTree (toBin x)

toBin :: Int -> [Int]
toBin = unfoldr (\x -> if x == 0 then Nothing else Just (mod x 2, div x 2))


dropTree :: [Int] -> RList a -> RList a
dropTree [] [] = []
dropTree _ [] = error "Empty"
dropTree [] ts = ts
dropTree (0 : xs) (_ : ts) = dropTree xs ts
dropTree (1 : xs) (One _ : ts) = dropTree xs ts
dropTree (1 : xs) (Zero : ts) = let
  (Node _ _ t, ts') = unconsTree ts
  in dropTree xs (One t : ts')
