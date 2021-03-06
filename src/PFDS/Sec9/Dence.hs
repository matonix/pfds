module PFDS.Sec9.Dence where

data Digit = Zero | One deriving (Show)
type Nat = [Digit]

inc :: Nat -> Nat
inc [] = [One]
inc (Zero : ds) = One : ds
inc (One : ds) = Zero : inc ds

dec :: Nat -> Nat
dec [One] = []
dec (One : ds) = Zero : ds
dec (Zero : ds) = One : dec ds

add :: Nat -> Nat -> Nat
add ds [] = ds
add [] ds = ds
add (d : ds1) (Zero : ds2) = d : add ds1 ds2
add (Zero : ds1) (d : ds2) = d : add ds1 ds2
add (One : ds1) (One : ds2) = Zero : inc (add ds1 ds2)
