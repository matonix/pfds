module PFDS.Strict.RealTimeQueueNaive where

import Prelude hiding (head, tail)

data Stream a = Nil | Cons a (Stream a) deriving (Show)

data RTQ a = Q [a] [a] [a] deriving (Show)

empty :: RTQ a
empty = Q [] [] []

isEmpty :: RTQ a -> Bool
isEmpty (Q [] _ _) = True
isEmpty _ = False

snoc :: RTQ a -> a -> RTQ a
snoc (Q f r s) x = exec f (x:r) s

head :: RTQ a -> a
head (Q [] _ _) = error "empty"
head (Q (x:_) _ _) = x

tail :: RTQ a -> RTQ a
tail (Q [] _ _) = error "empty"
tail (Q (_:f) r s) = exec f r s

exec :: [a] -> [a] -> [a] -> RTQ a
exec f r (x:s) = Q f r s
exec f r [] = Q f' [] f' where
  f' = rotate f r []

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:_) a = y:a
rotate (x:xs) (y:ys) a = x:rotate xs ys (y:a)
