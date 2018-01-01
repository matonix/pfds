{-# LANGUAGE InstanceSigs #-}

module PFDS.Commons.BankersDeque where

import PFDS.Commons.Deque

data BankersDeque a = Q Int Int [a] Int [a]

instance Deque BankersDeque where
  empty :: BankersDeque a
  empty = Q 4 0 [] 0 []

  isEmpty :: BankersDeque a -> Bool
  isEmpty (Q _ _ [] _ []) = True
  isEmpty _ = False

  cons :: a -> BankersDeque a -> BankersDeque a
  cons x (Q c lenf f lenr r) = check (Q c (lenf + 1) (x : f) lenr r)

  head :: BankersDeque a -> a
  head (Q _ _ [] _ []) = error "empty"
  head (Q _ _ [] _ (x:_)) = x
  head (Q _ _ (x:_) _ _) = x

  tail :: BankersDeque a -> BankersDeque a
  tail (Q _ _ [] _ []) = error "empty"
  tail (Q c _ [] _ (_:_)) = emptyc c
  tail (Q c lenf (_:f') lenr r) = check (Q c (lenf - 1) f' lenr r)

  snoc :: BankersDeque a -> a -> BankersDeque a
  snoc (Q c lenf f lenr r) x = check (Q c lenf f (lenr + 1) (x : r))

  last :: BankersDeque a -> a
  last (Q _ _ [] _ []) = error "empty"
  last (Q _ _ (x:_) _ []) = x
  last (Q _ _ _ _ (x:_)) = x

  init :: BankersDeque a -> BankersDeque a
  init (Q _ _ [] _ []) = error "empty"
  init (Q c _ (_:_) _ []) = emptyc c
  init (Q c lenf f lenr (_:r')) = check (Q c lenf f (lenr - 1) r')

check :: BankersDeque a -> BankersDeque a
check q@(Q c lenf f lenr r)
  | lenf > c * lenr + 1 = let
    i = (lenf + lenr) `div` 2
    j = lenf + lenr - i
    f' = take i f
    r' = r ++ reverse (drop i f)
    in Q c i f' j r'
  | lenr > c * lenf + 1 = let
    j = (lenf + lenr) `div` 2
    i = lenf + lenr - j
    r' = take j r
    f' = f ++ reverse (drop j r)
    in Q c i f' j r'
  | otherwise = q

emptyc :: Int -> BankersDeque a
emptyc c = Q c 0 [] 0 []
