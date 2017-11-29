module PFDS.Commons.RealTimeQueue where

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

data RealTimeQueue a = Q (Stream a) [a] (Stream a) deriving (Show)

instance Queue RealTimeQueue where
  empty = Q Nil [] Nil
  isEmpty (Q Nil _ _) = True
  isEmpty _ = False

  snoc (Q f r s) x = exec f (x:r) s

  head (Q Nil _ _) = error "empty"
  head (Q (Cons x _) _ _) = x

  tail (Q Nil _ _) = error "empty"
  tail (Q (Cons _ f) r s) = exec f r s

exec :: Stream a -> [a] -> Stream a -> RealTimeQueue a
exec f r (Cons x s) = Q f r s
exec f r Nil = Q f' [] f' where
  f' = rotate f r Nil

rotate :: Stream a -> [a] -> Stream a -> Stream a
rotate Nil (y:_) a = Cons y a
rotate (Cons x xs) (y:ys) a = Cons x (rotate xs ys (Cons y a))
