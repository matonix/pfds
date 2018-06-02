module PFDS.Sec9.NonZero4aryNumber where

data Digit = One | Two | Three | Four
type Nat = [Digit]

inc :: Nat -> Nat
inc [] = [One]
inc (One : ds) = Two : ds
inc (Two : ds) = Three : ds
inc (Three : ds) = Four : ds
inc (Four : ds) = One : inc ds

dec :: Nat -> Nat
dec [] = undefined
dec [One] = []
dec (Two : ds) = One : ds
dec (Three : ds) = Two : ds
dec (Four : ds) = Three : ds
dec (One : ds) = let ds' = dec ds in Four : ds'
