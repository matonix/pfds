{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module PFDS.Strict.Sandbox where

data List a = Nil | Cons !a !(List a)

tail' :: List a -> List a
tail' Nil = error "empty"
tail' (Cons _ xs) = xs

run :: Int
run =
  let xs = 1 `Cons` (2 `Cons` (undefined `Cons` Nil))
  in 15

head' :: List a -> a
head' Nil = error "empty"
head' (Cons x _) = x

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

run2 :: Int
run2 =
  let xs = 1 `Cons` (2 `Cons` (undefined `Cons` Nil))
  in head' (map' (*2) xs)
