module PFDS.Strict.RealTimeQueueFixed where

import Prelude hiding (head, tail)

data List a = Nil | Cons !a !(List a)
data RTQ a = Q [a] !(List a) [a]

empty :: RTQ a
empty = Q [] Nil []

isEmpty :: RTQ a -> Bool
isEmpty (Q [] _ _) = True
isEmpty _ = False

snoc :: RTQ a -> a -> RTQ a
snoc (Q f r s) x = exec f (Cons x r) s

head :: RTQ a -> a
head (Q [] _ _) = error "empty"
head (Q (x:_) _ _) = x

tail :: RTQ a -> RTQ a
tail (Q [] _ _) = error "empty"
tail (Q (_:f) r s) = exec f r s

exec :: [a] -> List a -> [a] -> RTQ a
exec f r (x:s) = Q f r s
exec f r [] = Q f' Nil f' where
  f' = rotate f r []

rotate :: [a] -> List a -> [a] -> [a]
rotate [] (Cons y _) a = y:a
rotate (x:xs) (Cons y ys) a = x:rotate xs ys (y:a)
