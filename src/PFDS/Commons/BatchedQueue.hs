module PFDS.Commons.BatchedQueue (BatchedQueue) where

import PFDS.Commons.Queue

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

-- | Doctests for Queue
--
-- >>> foldl snoc (empty :: BatchedQueue Int) [1..10]
-- Q [1] [10,9,8,7,6,5,4,3,2]
