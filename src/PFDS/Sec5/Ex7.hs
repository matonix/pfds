module PFDS.Sec5.Ex7 where

import PFDS.Commons.Heap (Heap (..))
import PFDS.Commons.SplayHeap (SplayHeap (..))

sort :: Ord a => [a] -> [a]
sort = inOrder [] . construct

construct :: Ord a => [a] -> SplayHeap a
construct = foldl (flip insert) empty

inOrder :: [a] -> SplayHeap a -> [a]
inOrder xs E = xs
inOrder xs (T a x b) = inOrder (x:bs) a where
  bs = inOrder xs b

{-| Doctests for Ex7

>>> mapM_ (print . construct) [[1..x] | x <- [1..5]]
T E 1 E
T (T E 1 E) 2 E
T (T (T E 1 E) 2 E) 3 E
T (T (T (T E 1 E) 2 E) 3 E) 4 E
T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E

-}
