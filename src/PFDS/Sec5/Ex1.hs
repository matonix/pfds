module PFDS.Sec5.Ex1 where

import Prelude hiding (head, last, tail, init)

class Deque q where
  empty :: q a
  isEmpty :: q a -> Bool
  cons :: q a -> a -> q a
  snoc :: q a -> a -> q a
  head :: q a -> a
  last :: q a -> a
  tail :: q a -> q a
  init :: q a -> q a

data BatchedDeque a = Q [a] [a] deriving (Show)

instance Deque BatchedDeque where
  empty = Q [] []

  isEmpty (Q [] []) = True
  isEmpty _ = False

  cons (Q f r) x = check (x:f) r

  snoc (Q f r) x = check f (x:r)

  head (Q [] _) = error "empty"
  head (Q (x:_) _) = x

  last (Q _ []) = error "empty"
  last (Q _ (x:_)) = x

  tail (Q [] _) = error "empty"
  tail (Q (_:f) r) = check f r

  init (Q _ []) = error "empty"
  init (Q f (_:r)) = check f r

check :: [a] -> [a] -> BatchedDeque a
check [] r = Q (reverse r) []
check f r = Q f r

-- foldl snoc (empty :: BatchedQueue Int) [1..10]
-- Q [1] [10,9,8,7,6,5,4,3,2]
