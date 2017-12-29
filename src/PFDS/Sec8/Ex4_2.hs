module PFDS.Sec8.Ex4_2 where

import Prelude hiding (head, tail, last, init)
import qualified PFDS.Commons.Queue as Q

type Queue q a = ([a], q a)

class Q.Queue q => Deque q where
  empty :: Queue q a
  isEmpty :: Queue q a -> Bool
  cons :: a -> Queue q a -> Queue q a
  head :: Queue q a -> a
  tail :: Queue q a -> Queue q a
  snoc :: Queue q a -> a -> Queue q a
  last :: Queue q a -> a
  init :: Queue q a -> Queue q a
