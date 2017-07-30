module Sec4.Ex2 where

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

sort :: Ord a => Stream a -> Stream a
sort = isort Nil where
  isort ys Nil = ys
  isort ys (Cons x xs) = isort (insert x ys) xs
  insert x Nil = Cons x Nil
  insert x (Cons y ys) = if x <= y
    then Cons x (Cons y ys)
    else Cons y (insert x ys)

-- *Sec4.Ex2> sort (Cons 5 (Cons 1 (Cons 3 (Cons 4 (Cons 2 Nil)))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
