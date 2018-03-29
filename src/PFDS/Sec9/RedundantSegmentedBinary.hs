module PFDS.Sec9.RedundantSegmentedBinary where

data Digits = Zero | Ones Int | Two deriving (Show)
type Nat = [Digits]

-- zeros :: Int -> Nat -> Nat
-- zeros i [] = []
-- zeros 0 ds = ds
-- zeros i (Zeros j : ds) = Zeros (i + j) : ds
-- zeros i ds = Zeros i : ds

ones :: Int -> Nat -> Nat
ones 0 ds = ds
ones i (Ones j : ds) = Ones (i + j) : ds
ones i ds = Ones i : ds

simpleInc :: Nat -> Nat
simpleInc [] = [Ones 1]
simpleInc (Zero : ds) = ones 1 ds
simpleInc (Ones i : ds) = Two : ones (i - 1) ds

fixup :: Nat -> Nat
fixup (Two : ds) = Zero : simpleInc ds
fixup (Ones i : Two : ds) = Ones i : Zero : simpleInc ds
fixup ds = ds

inc :: Nat -> Nat
inc = fixup . simpleInc

-- for ex 9.11
plus :: Nat -> Nat -> Nat
plus ds1 [] = ds1
plus [] ds2 = ds2
plus (Zero : ds1) (Zero : ds2) = Zero : plus (fixup ds1) (fixup ds2)
plus (Ones i : ds1) (Zero : ds2) = ones 1 (plus (f i ds1) (fixup ds2))
plus (Zero : ds1) (Ones j : ds2) = ones 1 (plus (fixup ds1) (f j ds2))
plus (Ones i : ds1) (Ones j : ds2) = Zero : simpleInc (plus (f i ds1) (f j ds2))

f :: Int -> Nat -> Nat
f 1 = fixup
f i = ones (i - 1)

-- dec :: Nat -> Nat
-- dec (Ones i : ds) = zeros 1 (ones (i - 1) ds)
-- dec (Zeros i : ds) = Ones i : dec ds
