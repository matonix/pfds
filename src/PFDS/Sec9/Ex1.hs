module PFDS.Sec9.Ex1 where

import qualified PFDS.Commons.RandomAccessList as R
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup, drop)
import Data.List (unfoldr)

-- naive impl
dropNaive :: Int -> RList a -> RList a
dropNaive _ [] = []
dropNaive x xs =
  if x <= 0
  then xs
  else dropNaive (x - 1) (R.tail xs)

drop :: Int -> RList a -> RList a
drop x ds
  | x <= 0 = ds
  | x >= sizeR ds = []
  | otherwise = chopZero (dropTree (toBin x) ds)
    where
      sizeR [] = 0
      sizeR (Zero : ds') = sizeR ds'
      sizeR (One t : ds') = size t + sizeR ds'

toBin :: Int -> [Int]
toBin = unfoldr (\x ->
  if x == 0
  then Nothing
  else Just (x `mod` 2, x `div` 2))

dropTree :: [Int] -> RList a -> RList a
dropTree [] ds = ds
dropTree _ [] = []
dropTree (0 : xs) (d : ds) = d : dropTree xs ds
dropTree (1 : xs) (One _ : ds) = Zero : dropTree xs ds
dropTree (1 : xs) (Zero : ds) = One t : dropTree xs ds'
  where
    (Node _ _ t, ds') = unconsTree ds

chopZero :: RList a -> RList a
chopZero = reverse . dropWhile isZero . reverse
  where
    isZero Zero = True
    isZero _ = False

drop' :: Int -> RList a -> RList a
drop' 0 ts = ts
drop' _ [] = []
drop' x (t:ts) =
  if x `mod` 2 == 0
  then drop' x' ts
  else case t of
    One _ -> drop' x' ts
    Zero -> drop' x' (One t' : ts')
  where
    x' = x `div` 2
    (Node _ _ t', ts') = unconsTree ts
