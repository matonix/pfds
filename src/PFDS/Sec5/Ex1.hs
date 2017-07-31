module PFDS.Sec5.Ex1 where

import Prelude hiding (head, last, tail, init)

{-| Doctests for Deque

>>> foldl snoc (empty :: BatchedDeque Int) [1..10]
Q [1] [10,9,8,7,6,5,4,3,2]

>>> cons (empty :: BatchedDeque Int) 1
Q [1] []

>>> cons (cons (empty :: BatchedDeque Int) 1) 2
Q [2] [1]

>>> head $ Q [1] []
1

>>> head $ Q [] [1]
1

>>> head $ Q [1] [2]
1

>>> foldl cons (empty :: BatchedDeque Int) [1..10]
Q [10,9,8,7,6,5,4,3,2] [1]

>>> tail $ Q [1,2,3,4,5,6,7,8,9] [10]
Q [2,3,4,5,6,7,8,9] [10]

>>> tail $ Q [1] [2]
Q [] [2]

>>> tail $ Q [1] [10,9,8,7,6,5,4,3,2]
Q [2,3,4,5,6] [10,9,8,7]

>>> init $ Q [1] [10,9,8,7,6,5,4,3,2]
Q [1] [9,8,7,6,5,4,3,2]

>>> init $ Q [1] [2]
Q [1] []

>>> init $ Q [1,2,3,4,5,6,7,8,9] [10]
Q [1,2,3,4] [9,8,7,6,5]
-}

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

  head (Q [] []) = error "empty"
  head (Q [] [x]) = x
  head (Q (x:_) _) = x

  last (Q [] []) = error "empty"
  last (Q [x] []) = x
  last (Q _ (x:_)) = x

  tail (Q [] []) = error "empty"
  tail (Q [] _) = empty
  tail (Q (_:f) r) = check f r

  init (Q [] []) = error "empty"
  init (Q _ []) = empty
  init (Q f (_:r)) = check f r

check :: [a] -> [a] -> BatchedDeque a
check [f] [] = Q [f] []
check [] [r] = Q [] [r]
check f [] = Q ff (reverse fr) where (ff, fr) = halve f
check [] r = Q (reverse rr) rf where (rf, rr) = halve r
check f r = Q f r

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs
