{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PFDS.Sec8.Ex4_2 where

import Prelude hiding (head, tail, last, init)
import qualified PFDS.Commons.Queue as Q
import qualified PFDS.Commons.HoodMelvilleQueue as HMQ
type HMQueue = HMQ.Queue

type Queue q a = ([a], q a)

class Q.Queue q => Deque q where
  empty :: Queue q a
  isEmpty :: Queue q a -> Bool
  cons :: a -> Queue q a -> Queue q a
  head :: Queue q a -> a
  tail :: Queue q a -> Queue q a
  snoc :: Queue q a -> a -> Queue q a
  last :: Queue q a -> a
  init :: Queue q a -> Queue q a

instance Deque HMQueue where
  empty :: Queue HMQueue a
  empty = ([], Q.empty)

  isEmpty :: Queue HMQueue a -> Bool
  isEmpty (_, q) = Q.isEmpty q

  cons :: a -> Queue HMQueue a -> Queue HMQueue a
  cons e (xs, q) = (e:xs, q)

  head :: Queue HMQueue a -> a
  head (x:_, _) = x
  head ([], q) = Q.head q

  tail :: Queue HMQueue a -> Queue HMQueue a
  tail (_:xs, q) = (xs, Q.tail q) -- ?
  tail ([], q) = ([], Q.tail q)

  snoc :: Queue HMQueue a -> a -> Queue HMQueue a
  snoc (xs, q) e = (xs, Q.snoc q e)

  -- last :: Queue HMQueue a -> a
  -- init :: Queue HMQueue a -> Queue q a
