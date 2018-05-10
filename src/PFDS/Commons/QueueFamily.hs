{-# LANGUAGE TypeFamilies #-}

module PFDS.Commons.QueueFamily where

type family Elem q

class Queue q where
  empty :: q
  isEmpty :: q -> Bool
  snoc :: q -> Elem q -> q
  head :: q -> Elem q
  tail :: q -> q
