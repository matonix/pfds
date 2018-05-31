{-# LANGUAGE TypeFamilies #-}

module PFDS.Commons.HeapFamily where

type family Elem h

class Heap h where
  empty :: h
  isEmpty :: h -> Bool
  insert :: Elem h -> h -> h
  merge :: h -> h -> h
  findMin :: h -> Elem h
  deleteMin :: h -> h
