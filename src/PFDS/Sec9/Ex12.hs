module PFDS.Sec9.Ex12 where

data Digits =
    Zero -- Red
  | Ones Int -- Yellow
  | Two -- Green
  | Threes Int -- Yellow
  | Four -- Red
  deriving Show
type Nat = [Digits]

ones :: Int -> Nat -> Nat
ones 0 ds = ds
ones i (Ones j : ds) = Ones (i + j) : ds
ones i ds = Ones i : ds

threes :: Int -> Nat -> Nat
threes 0 ds = ds
threes i (Threes j : ds) = Threes (i + j) : ds
threes i ds = Threes i : ds

simpleInc :: Nat -> Nat
simpleInc [] = [Ones 1]
simpleInc (Ones i : ds) = Two : ones (i - 1) ds
simpleInc (Two : ds) = threes 1 ds
simpleInc (Threes i : ds) = Four : threes (i - 1) ds

simpleDec :: Nat -> Nat
simpleDec [Ones 1] = []
simpleDec (Ones i : ds) = Zero : ones (i - 1) ds
simpleDec (Two : ds) = ones 1 ds
simpleDec (Threes i : ds) = Two : threes (i - 1) ds

fixup :: Nat -> Nat
fixup (Zero : ds) = Two : simpleDec ds
fixup (Ones i : Zero : ds) = Ones i : Two : simpleDec ds
fixup (Threes i : Four : ds) = Threes i : Two : simpleInc ds
fixup (Four : ds) = Two : simpleInc ds
fixup ds = ds

inc :: Nat -> Nat
inc = fixup . simpleInc

dec :: Nat -> Nat
dec = fixup . simpleDec
