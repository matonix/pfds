module PFDS.Sec9.Ex4 where

data Digit = One | Two deriving (Show)
type Nat = [Digit]

inc :: Nat -> Nat
inc [] = [One]
inc (One : ds) = Two : ds
inc (Two : ds) = One : inc ds

dec :: Nat -> Nat
dec [] = error "zero"
dec [One] = []
dec (One : ds) = Two : dec ds
dec (Two : ds) = One : ds

add :: Nat -> Nat -> Nat
add ds [] = ds
add [] ds = ds
add (One : ds1) (One : ds2) = Two : add ds1 ds2
add (One : ds1) (Two : ds2) = One : inc (add ds1 ds2)
add (Two : ds1) (One : ds2) = One : inc (add ds1 ds2)
add (Two : ds1) (Two : ds2) = Two : inc (add ds1 ds2)


-- helper

toInt :: Nat -> Int
toInt = go 0
  where
    go n [] = n
    go n ds = go (succ n) (dec ds)

fromInt :: Int -> Nat
fromInt = go []
  where
    go ds 0 = ds
    go ds n = go (inc ds) (pred n)
