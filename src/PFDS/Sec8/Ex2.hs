module PFDS.Sec8.Ex2 where

data RotationState a =
    Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving (Show)

data Queue a = Q Int [a] (RotationState a) Int [a]
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

exec1 :: Queue a -> Queue a
exec1 (Q lenf f state lenr r) = case exec state of
  Done newf -> Q lenf newf Idle lenr r
  newstate -> Q lenf f newstate lenr r

exec2 :: Queue a -> Queue a
exec2 (Q lenf f state lenr r) = case exec (exec state) of
  Done newf -> Q lenf newf Idle lenr r
  newstate -> Q lenf f newstate lenr r

check :: Queue a -> Queue a
check q@(Q lenf f state lenr r) = if lenr <= lenf
  then exec1 q
  else exec2 (Q (lenf+lenr) f newstate 0 [])
  where newstate = Reversing 0 f [] r []

empty :: Queue a
empty = Q 0 [] Idle 0 []

isEmpty :: Queue a -> Bool
isEmpty (Q lenf _ _ _ _) = lenf == 0

snoc :: Queue a -> a -> Queue a
snoc (Q lenf f state lenr r) x = check (Q lenf f state (lenr+1) (x:r))

head :: Queue a -> a
head (Q _ [] _ _ _) = error "empty"
head (Q _ (x:_) _ _ _) = x

tail :: Queue a -> Queue a
tail (Q _ [] _ _ _) = error "empty"
tail (Q lenf (x:f) state lenr r) = check (Q (lenf-1) f (invalidate state) lenr r)
