module PFDS.Sec9.Ex1 where

import qualified PFDS.Commons.RandomAccessList as R
import PFDS.Commons.BinaryRandomAccessList
import Prelude hiding (head, tail, lookup, drop)

-- naive impl
dropNaive :: Int -> RList a -> RList a
dropNaive 0 xs = xs
dropNaive i xs = dropNaive (i - 1) (R.tail xs)

drop :: Int -> RList a -> RList a
drop = undefined
