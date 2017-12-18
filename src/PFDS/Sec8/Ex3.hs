module PFDS.Sec8.Ex3 where

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

exec1 :: Queue a -> Queue a
exec1 (Q d f state r) = case exec state of
  Done newf -> Q d newf Idle r
  newstate@(Reversing ok _ _ _ _) -> Q (max d (ok*2+1)) f newstate r
  newstate@(Appending ok _ _) -> Q (max d (ok*2+1)) f newstate r
  newstate -> Q d f newstate r

-- exec2 :: Queue a -> Queue a
-- exec2 (Q d f state r) = case exec (exec state) of
--   Done newf -> Q d newf Idle r
--   newstate@(Reversing ok _ _ _ _) -> Q (max d (ok*2+1)) f newstate r
--   newstate@(Appending ok _ _) -> Q (max d (ok*2+1)) f newstate r
--   newstate -> Q d f newstate r

check :: Queue a -> Queue a
check q@(Q d f state r) = if d >= 0
  then exec1 q
  else exec1 $ exec1 (Q d f newstate [])
  where newstate = Reversing 0 f [] r []

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

-- *PFDS.Sec8.Ex3> mapM_ (print . foldl snoc empty) [[1..x] | x <- [1..30]]
-- Q 1 [1] Idle []
-- Q 0 [1] Idle [2]
-- Q 3 [1] (Appending 1 [1] [2,3]) []
-- Q 2 [1] (Appending 0 [] [1,2,3]) [4]
-- Q 1 [1,2,3] Idle [5,4]
-- Q 0 [1,2,3] Idle [6,5,4]
-- Q 5 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) []
-- Q 7 [1,2,3] (Reversing 3 [] [3,2,1] [4] [5,6,7]) [8]
-- Q 7 [1,2,3] (Appending 3 [3,2,1] [4,5,6,7]) [9,8]
-- Q 6 [1,2,3] (Appending 2 [2,1] [3,4,5,6,7]) [10,9,8]
-- Q 5 [1,2,3] (Appending 1 [1] [2,3,4,5,6,7]) [11,10,9,8]
-- Q 4 [1,2,3] (Appending 0 [] [1,2,3,4,5,6,7]) [12,11,10,9,8]
-- Q 3 [1,2,3,4,5,6,7] Idle [13,12,11,10,9,8]
-- Q 2 [1,2,3,4,5,6,7] Idle [14,13,12,11,10,9,8]
-- Q 1 [1,2,3,4,5,6,7] Idle [15,14,13,12,11,10,9,8]
-- Q 0 [1,2,3,4,5,6,7] Idle [16,15,14,13,12,11,10,9,8]
-- Q 5 [1,2,3,4,5,6,7] (Reversing 2 [3,4,5,6,7] [2,1] [15,14,13,12,11,10,9,8] [16,17]) []
-- Q 7 [1,2,3,4,5,6,7] (Reversing 3 [4,5,6,7] [3,2,1] [14,13,12,11,10,9,8] [15,16,17]) [18]
-- Q 9 [1,2,3,4,5,6,7] (Reversing 4 [5,6,7] [4,3,2,1] [13,12,11,10,9,8] [14,15,16,17]) [19,18]
-- Q 11 [1,2,3,4,5,6,7] (Reversing 5 [6,7] [5,4,3,2,1] [12,11,10,9,8] [13,14,15,16,17]) [20,19,18]
-- Q 13 [1,2,3,4,5,6,7] (Reversing 6 [7] [6,5,4,3,2,1] [11,10,9,8] [12,13,14,15,16,17]) [21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [25,24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [26,25,24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [27,26,25,24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [28,27,26,25,24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [29,28,27,26,25,24,23,22,21,20,19,18]
-- Q 15 [1,2,3,4,5,6,7] (Reversing 7 [] [7,6,5,4,3,2,1] [10,9,8] [11,12,13,14,15,16,17]) [30,29,28,27,26,25,24,23,22,21,20,19,18]
