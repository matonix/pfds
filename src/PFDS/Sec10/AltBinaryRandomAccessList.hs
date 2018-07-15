module PFDS.Sec10.AltBinaryRandomAccessList where

import PFDS.Commons.RandomAccessListNoFamily
import Prelude hiding (head, tail, lookup)

data RList a = Nil
           | Zero (RList (a, a))
           | One a (RList (a, a))
           deriving (Show)

instance RandomAccessList RList where

  empty = Nil

  isEmpty Nil = True
  isEmpty _ = False

  cons x Nil = One x Nil
  cons x (Zero ps) = One x ps
  cons x (One y ps) = Zero (cons (x, y) ps)

  head xs = let (x, _) = uncons xs in x

  tail xs = let (_, xs') = uncons xs in xs'

  lookup _ Nil = error "Subscript"
  lookup 0 (One x _) = x
  lookup i (One _ ps) = lookup (i - 1) (Zero ps)
  lookup i (Zero ps) =
    let (x, y) = lookup (i `div` 2) ps
    in if i `mod` 2 == 0 then x else y

  update i y xs = fupdate (const y) i xs

uncons :: RList a -> (a, RList a)
uncons Nil = error "Empty"
uncons (One x Nil) = (x, Nil)
uncons (One x ps) = (x, Zero ps)
uncons (Zero ps) =
  let ((x, y), ps') = uncons ps
  in (x, One y ps')

fupdate :: (a -> a) -> Int -> RList a -> RList a
fupdate _ _ Nil = error "Subscript"
fupdate f 0 (One x ps) = One (f x) ps
fupdate f i (One x ps) = cons x (fupdate f (i - 1) (Zero ps))
fupdate f i (Zero ps) =
  let f' (x, y) = if i `mod` 2 == 0 then (f x, y) else (x, f y)
  in Zero (fupdate f' (i `div` 2) ps)
