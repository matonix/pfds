{-# LANGUAGE InstanceSigs #-}

-- c の取り扱いが sml と異なっている（データ構造に c を含めている）点に注意

module PFDS.Commons.RealTimeDeque where

import PFDS.Commons.Deque

--                        c  |f|  f  sf  |r|  r  sr
data RealTimeDeque a = Q Int Int [a] [a] Int [a] [a] deriving (Show)

instance Deque RealTimeDeque where
  empty :: RealTimeDeque a
  empty = Q 3 0 [] [] 0 [] []

  isEmpty :: RealTimeDeque a -> Bool
  isEmpty (Q _ _ [] _ _ [] _) = True
  isEmpty _ = False

  cons :: a -> RealTimeDeque a -> RealTimeDeque a
  cons x (Q c lenf f sf lenr r sr) =
    check (Q c (lenf + 1) (x : f) (exec1 sf) lenr r (exec1 sr))

  head :: RealTimeDeque a -> a
  head (Q _ _ [] _ _ [] _) = error "empty"
  head (Q _ _ [] _ _ (x:_) _) = x
  head (Q _ _ (x:_) _ _ _ _) = x

  tail :: RealTimeDeque a -> RealTimeDeque a
  tail (Q _ _ [] _ _ [] _) = error "empty"
  tail (Q c _ [] _ _ (_:_) _) = emptyc c
  tail (Q c lenf (_:f') sf lenr r sr) =
    check (Q c (lenf - 1) f' (exec2 sf) lenr r (exec2 sr))

  snoc :: RealTimeDeque a -> a -> RealTimeDeque a
  snoc (Q c lenf f sf lenr r sr) x =
    check (Q c lenf f (exec1 sf) (lenr + 1) (x : r) (exec1 sr))

  last :: RealTimeDeque a -> a
  last (Q _ _ [] _ _ [] _) = error "empty"
  last (Q _ _ (x:_) _ _ [] _) = x
  last (Q _ _ _ _ _ (x:_) _) = x

  init :: RealTimeDeque a -> RealTimeDeque a
  init (Q _ _ [] _ _ [] _) = error "empty"
  init (Q c _ (_:_) _ _ [] _) = emptyc c
  init (Q c lenf f sf lenr (_:r') sr) = check (Q c lenf f (exec2 sf) (lenr - 1) r' (exec2 sr))

check :: RealTimeDeque a -> RealTimeDeque a
check q@(Q c lenf f sf lenr r sr)
  | lenf > c * lenr + 1 = let
    i = (lenf + lenr) `div` 2
    j = lenf + lenr - i
    f' = take i f
    r' = rotateDrop c r i f
    in Q c i f' f' j r' r'
  | lenr > c * lenf + 1 = let
    j = (lenf + lenr) `div` 2
    i = lenf + lenr - j
    r' = take j r
    f' = rotateDrop c f j r
    in Q c i f' f' j r' r'
  | otherwise = q

rotateDrop :: Int -> [a] -> Int -> [a] -> [a]
rotateDrop c f j r = if j < c
  then rotateRev c f (drop j r) []
  else let (x:f') = f
    in x : rotateDrop c f' (j - c) (drop c r)

rotateRev :: Int -> [a] -> [a] -> [a] -> [a]
rotateRev c [] r a = reverse r ++ a
rotateRev c (x:f) r a = x : rotateRev c f (drop c r) (reverse (take c r) ++ a)

exec2 :: [a] -> [a]
exec2 s = exec1 (exec1 s)

exec1 :: [a] -> [a]
exec1 (x:s) = s
exec1 s = s

-- use 2 or 3 for c
emptyc :: Int -> RealTimeDeque a
emptyc c = Q c 0 [] [] 0 [] []
