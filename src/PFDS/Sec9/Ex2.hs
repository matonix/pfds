module PFDS.Sec9.Ex2 where

import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList
import Data.List (unfoldr)

createNaive :: Int -> a -> RList a
createNaive n x = if n <= 0 then empty else cons x $ createNaive (n - 1) x

create :: Int -> a -> RList a
create n x = create' n (Leaf x)
  where
    create' 0 _ = []
    create' n t =
      if n `mod` 2 == 0
      then Zero : create' (n `div` 2) (double t)
      else One t : create' (n `div` 2) (double t)
      where
        double t@(Leaf _) = Node 2 t t
        double t@(Node w _ _) = Node (w * 2) t t

create2 :: Int -> a -> RList a
create2 n x = unfoldr f (n, Leaf x)
  where
    f (0, _) = Nothing
    f (n, t) = let
      (next, m) = n `divMod` 2
      in if m == 0
        then Just (Zero, (next, double t))
        else Just (One t, (next, double t))
        where
          double t@(Leaf _) = Node 2 t t
          double t@(Node w _ _) = Node (w * 2) t t
