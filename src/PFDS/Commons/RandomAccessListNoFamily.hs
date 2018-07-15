module PFDS.Commons.RandomAccessListNoFamily where

class RandomAccessList l where
  empty :: l a
  isEmpty :: l a -> Bool
  cons :: a -> l a-> l a
  head :: l a -> a -- raise Empty
  tail :: l a -> l a -- raise Empty
  lookup :: Int -> l a -> a -- raise Subscript
  update :: Int -> a -> l a -> l a -- raise Subscript
