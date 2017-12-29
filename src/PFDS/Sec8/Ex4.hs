module PFDS.Sec8.Ex4 where

import Prelude hiding (head, tail, last, init)
import PFDS.Commons.Queue

class Queue q => Deque q where
  empty :: ([a], q a)
  isEmpty :: ([a], q a) -> Bool
  cons :: a -> ([a], q a) -> ([a], q a)
  head :: ([a], q a) -> a
  tail :: ([a], q a) -> ([a], q a)
  snoc :: ([a], q a) -> a -> ([a], q a)
  last :: ([a], q a) -> a
  init :: ([a], q a) -> ([a], q a)
