module PFDS.Sec8.Ex3 where

import Prelude hiding (head, tail)

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving (Show)

data Queue a = Q Int [a] (RotationState a) [a]
  deriving (Show)

exec :: RotationState a -> RotationState a
exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok+1) f (x:f') r (y:r')
exec (Reversing ok [] f' [y] r') = Appending ok f' (y:r')
exec (Appending 0 f' r') = Done r'
exec (Appending ok (x:f') r') = Appending (ok-1) f' (x:r')
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok-1) f f' r r'
invalidate (Appending 0 f' (x:r')) = Done r'
invalidate (Appending ok f' r') = Appending (ok-1) f' r'
invalidate state = state

exec1 :: Int -> Queue a -> (Int, Queue a)
exec1 l (Q d f state r) = case exec state of
  Done newf -> (l', Q l' newf Idle r)
  newstate -> (l', Q l' f newstate r)
  where
    l' = calcd l state

calcd :: Int -> RotationState a -> Int
calcd l (Reversing _ (_:_) _ _ _) = l+2
calcd l (Reversing _ [] _ _ _) = l+1
calcd l _ = l

check :: Queue a -> Queue a
check q@(Q d f state r) = if d >= 0
  then snd $ exec1 d q
  else snd . uncurry exec1 $ exec1 0 (Q 1 f newstate [])
  where
    newstate = Reversing 0 f [] r []

empty :: Queue a
empty = Q 0 [] Idle []

isEmpty :: Queue a -> Bool
isEmpty (Q _ [] _ []) = True
isEmpty _ = False

snoc :: Queue a -> a -> Queue a
snoc (Q d f state r) x = check (Q (d-1) f state (x:r))

head :: Queue a -> a
head (Q _ [] _ _) = error "empty"
head (Q _ (x:_) _ _) = x

tail :: Queue a -> Queue a
tail (Q _ [] _ _) = error "empty"
tail (Q d (x:f) state r) = check (Q (d-1) f (invalidate state) r)
