module PFDS.Sec9.SegmentedBinary where

data DigitBlock = Zeros Int | Ones Int deriving (Show)
type Nat = [DigitBlock]

zeros :: Int -> Nat -> Nat
zeros i [] = []
zeros 0 blks = blks
zeros i (Zeros j : blks) = Zeros (i + j) : blks
zeros i blks = Zeros i : blks

ones :: Int -> Nat -> Nat
ones 0 blks = blks
ones i (Ones j : blks) = Ones (i + j) : blks
ones i blks = Ones i : blks

inc :: Nat -> Nat
inc [] = [Ones 1]
inc (Zeros i : blks) = ones 1 (zeros (i - 1) blks)
inc (Ones i : blks) = Zeros i : inc blks

dec :: Nat -> Nat
dec (Ones i : blks) = zeros 1 (ones (i - 1) blks)
dec (Zeros i : blks) = Ones i : dec blks
