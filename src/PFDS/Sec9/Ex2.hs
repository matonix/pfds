module PFDS.Sec9.Ex2 where

import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList

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
