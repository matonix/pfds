module PFDS.Sec9.Ex2 where

import PFDS.Commons.RandomAccessList
import PFDS.Commons.BinaryRandomAccessList

createNaive :: Int -> a -> RList a
createNaive n x = if n <= 0 then empty else cons x $ createNaive (n - 1) x

create :: Int -> a -> RList a
create n x = create' n 1
  where
    create' 0 _ = []
    create' n p =
      if n `mod` 2 == 0
      then Zero : create' (n `div` 2) (p * 2)
      else One (createTree p) : create' (n `div` 2) (p * 2)
      where
        createTree 1 = Leaf x
        createTree m = Node m child child
          where
            child = createTree (m `div` 2)
