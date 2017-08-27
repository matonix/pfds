{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PFDS.Sec5.Ex8b where

import PFDS.Sec3.Heap (Heap (..))

data BinTree e = E' | T' e (BinTree e) (BinTree e) deriving (Show)

instance Heap BinTree where
  empty = E'

  isEmpty E' = True
  isEmpty _ = False

  merge h E' = h
  merge E' h = h
  merge (T' x h1 E') (T' y h2 E') =
    if x <= y then T' x (T' y h2 h1) E' else T' y (T' x h1 h2) E'

  insert x = merge (T' x E' E')

  findMin E' = error "empty"
  findMin (T' x _ _) = x

  deleteMin E' = error "empty"
  deleteMin (T' _ h E') = mergePairs h

mergePairs :: Ord e => BinTree e -> BinTree e
mergePairs E' = E'
mergePairs (T' x h E') = T' x h E'
mergePairs (T' x h1 (T' y h1' h2')) = merge (T' x h1 E') $ merge (T' y h1' E') $ mergePairs h2'

{-| Doctests for Ex8b

>>> mapM_ (print . foldl (flip insert) (empty::BinTree Int)) [[1..x] | x <- [1..5]]
T' 1 E' E'
T' 1 (T' 2 E' E') E'
T' 1 (T' 3 E' (T' 2 E' E')) E'
T' 1 (T' 4 E' (T' 3 E' (T' 2 E' E'))) E'
T' 1 (T' 5 E' (T' 4 E' (T' 3 E' (T' 2 E' E')))) E'

>>> print . foldl (flip insert) (empty::BinTree Int) $ [2,4,5,3,1]
T' 1 (T' 2 (T' 3 E' (T' 5 E' (T' 4 E' E'))) E') E'

-}
