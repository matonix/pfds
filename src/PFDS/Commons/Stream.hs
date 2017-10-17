module PFDS.Commons.Stream where

import Prelude hiding ((++), take, drop, reverse)

data Stream a = Nil | Cons a (Stream a) deriving (Show)

(++) :: Stream a -> Stream a -> Stream a
Nil ++ t = t
(Cons x xs) ++ t = Cons x (xs ++ t)

take :: Int -> Stream a -> Stream a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons x xs) = Cons x (take (n-1) xs)

drop :: Int -> Stream a -> Stream a
drop 0 xs = xs
drop _ Nil = Nil
drop n (Cons _ xs) = drop (n-1) xs

reverse :: Stream a -> Stream a
reverse = rev Nil where
  rev ys Nil = ys
  rev ys (Cons x xs) = rev (Cons x ys) xs
