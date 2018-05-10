{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PFDS.Sec9.Ex14 where

import qualified PFDS.Commons.QueueFamily as Q
import qualified PFDS.Commons.RandomAccessList as L
import PFDS.Commons.SkewBinaryRandomAccessList
import Prelude hiding (head, tail, lookup)

type instance Q.Elem (Queue a) = a

data RotationState a =
    Idle
  | Reversing Int (RList a) (RList a) (RList a) (RList a)
  | Appending Int (RList a) (RList a)
  | Done (RList a)

data Queue a = Q Int (RList a) (RotationState a) Int (RList a)

exec :: RotationState a -> RotationState a
exec (Reversing ok f f' r r') = if L.isEmpty f
  then Appending ok f' (L.head r `L.cons` r')
  else Reversing (ok + 1) (L.tail f) (L.head f `L.cons` f') r (L.head r `L.cons` r')
exec (Appending 0 f' r') = Done r'
exec (Appending ok f' r') = if L.isEmpty f'
  then Appending (ok - 1) (L.tail f') (L.head f' `L.cons` r')
  else Appending ok f' r'
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 f' r') = Done (L.tail r')
invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
invalidate state = state

exec2 :: Queue a -> Queue a
exec2 (Q lenf f state lenr r) = case exec (exec state) of
  Done newf -> Q lenf newf Idle lenr r
  newstate -> Q lenf f newstate lenr r

check :: Queue a -> Queue a
check q@(Q lenf f state lenr r) = if lenr <= lenf
  then exec2 q
  else exec2 (Q (lenf+lenr) f newstate 0 L.empty)
  where newstate = Reversing 0 f L.empty r L.empty

instance Q.Queue (Queue a) where
  empty :: Queue a
  empty = Q 0 L.empty Idle 0 L.empty

  isEmpty :: Queue a -> Bool
  isEmpty (Q lenf _ _ _ _) = lenf == 0

  snoc :: Queue a -> a -> Queue a
  snoc (Q lenf f state lenr r) x = check (Q lenf f state (lenr + 1) (x `L.cons` r))

  head :: Queue a -> a
  head (Q _ [] _ _ _) = error "empty"
  head (Q _ xs _ _ _) = L.head xs

  tail :: Queue a -> Queue a
  tail (Q _ [] _ _ _) = error "empty"
  tail (Q lenf f state lenr r) = check (Q (lenf - 1) (L.tail f) (invalidate state) lenr r)

lookup :: Int -> Queue a -> a
lookup i (Q lenf f _ lenr r) = if i < lenf
  then L.lookup i f
  else L.lookup (lenr - (i - lenf) - 1) r

update :: Int -> a -> Queue a -> Queue a
update i x (Q lenf f state lenr r) = if i < lenf
  then let f' = L.update i x f in Q lenf f' state lenr r
  else let r' = L.update (lenr - (i - lenf) - 1) x r in Q lenf f state lenr r'

-- state の中身は…？
