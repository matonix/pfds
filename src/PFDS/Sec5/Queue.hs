module PFDS.Sec5.Queue where

import Prelude hiding (head, tail)

-- | Doctests for Queue
--
-- >>> foldl snoc (empty :: BatchedQueue Int) [1..10]
-- Q [1] [10,9,8,7,6,5,4,3,2]

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  head :: q a -> a
  tail :: q a -> q a

data BatchedQueue a = Q [a] [a] deriving (Show)

instance Queue BatchedQueue where
  empty = Q [] []
  isEmpty (Q f _) = null f
  snoc (Q f r) x = checkf f (x:r)
  head (Q [] _) = error "empty"
  head (Q (x:_) _) = x
  tail (Q [] _) = error "empty"
  tail (Q (_:f) r) = checkf f r

checkf :: [a] -> [a] -> BatchedQueue a
checkf [] r = Q (reverse r) []
checkf f r = Q f r
