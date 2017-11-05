module PFDS.Commons.BankersQueue where

import Prelude hiding ((++), reverse)
import PFDS.Commons.Stream
import PFDS.Commons.Queue (Queue(..))

-- MEMO
-- data Stream a = Nil | Cons a (Stream a) deriving (Show)
-- class Queue q where
--   empty :: q a
--   isEmpty :: q a -> Bool
--   snoc :: q a -> a -> q a
--   head :: q a -> a
--   tail :: q a -> q a

data BankersQueue a = Q Int (Stream a) Int (Stream a) deriving (Show)

instance Queue BankersQueue where
  empty = Q 0 Nil 0 Nil
  isEmpty (Q lf _ _ _) = lf == 0
  snoc (Q lf f lr r) x = check lf f (lr + 1) (Cons x r)
  head (Q _ Nil _ _) = error "empty"
  head (Q _ (Cons x _) _ _) = x
  tail (Q _ Nil _ _) = error "empty"
  tail (Q lf (Cons _ f') lr r) = check (lf - 1) f' lr r

check :: Int -> Stream a -> Int -> Stream a -> BankersQueue a
check lf f lr r =
  if lf >= lr then Q lf f lr r else Q (lf + lr) (f ++ reverse r) 0 Nil
