module PFDS.Sec10.Ex2 (RList(..)) where

import PFDS.Commons.RandomAccessListNoFamily
import Prelude hiding (head, tail, lookup)

-- NonZeroRedundantBinary (cf. Ex9.9)
data RList a = Nil
             | One a (RList (a, a))
             | Two a a (RList (a, a))
             | Three a a a (RList (a, a))
             deriving (Show)

instance RandomAccessList RList where

  empty = Nil

  isEmpty Nil = True
  isEmpty _ = False

  cons x Nil = One x Nil
  cons x (One y ps) = Two x y ps
  cons x (Two y z ps) = Three x y z ps
  cons x (Three y z w ps) = Two x y (cons (z, w) ps)

  head xs = let (x, _) = uncons xs in x

  tail xs = let (_, xs') = uncons xs in xs'

  lookup _ Nil = error "Subscript"
  lookup 0 (One x ps) = x
  lookup i (One _ ps) = lookupHalf (i - 1) ps
  lookup 0 (Two x _ ps) = x
  lookup 1 (Two _ y ps) = y
  lookup i (Two _ _ ps) = lookupHalf (i - 2) ps
  lookup 0 (Three x _ _ ps) = x
  lookup 1 (Three _ y _ ps) = y
  lookup 2 (Three _ _ z ps) = z
  lookup i (Three _ _ _ ps) = lookupHalf (i - 3) ps

  update i y xs = fupdate (const y) i xs

uncons :: RList a -> (a, RList a)
uncons Nil = error "Empty"
uncons (One x Nil) = (x, Nil)
uncons (Two x y ps) = (x, One y ps)
uncons (Three x y z ps) = (x, Two y z ps)
uncons (One x ps) =
  let ((y, z), ps') = uncons ps
  in (x, Two y z ps')

lookupHalf :: Int -> RList (a, a) -> a
lookupHalf i ps = let
 (x, y) = lookup (i `div` 2) ps
 in if i `mod` 2 == 0 then x else y

fupdate :: (a -> a) -> Int -> RList a -> RList a
fupdate _ _ Nil = error "Subscript"
fupdate f 0 (One x ps) = One (f x) ps
fupdate f i (One x ps) = One x (fupdateHalf f (i - 1) ps)
fupdate f 0 (Two x y ps) = Two (f x) y ps
fupdate f 1 (Two x y ps) = Two x (f y) ps
fupdate f i (Two x y ps) = Two x y (fupdateHalf f (i - 2) ps)
fupdate f 0 (Three x y z ps) = Three (f x) y z ps
fupdate f 1 (Three x y z ps) = Three x (f y) z ps
fupdate f 2 (Three x y z ps) = Three x y (f z) ps
fupdate f i (Three x y z ps) = Three x y z (fupdateHalf f (i - 3) ps)

fupdateHalf :: (a -> a) -> Int -> RList (a, a) -> RList (a, a)
fupdateHalf f i ps = let
  f' (x, y) = if i `mod` 2 == 0 then (f x, y) else (x, f y)
  in fupdate f' (i `div` 2) ps
