{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module PFDS.Sec6.Ex6 (PhyQueue) where

import           PFDS.Commons.Queue
import           Prelude            hiding (head, tail)
import qualified Prelude            (tail)

data PhyQueue a = PQ [a] Int [a] Int [a] Int

check :: PhyQueue a -> PhyQueue a
check pq@(PQ w lenf f lenr r phi) =
  if lenr <= lenf
    then pq
    else checkw (PQ f (lenf + lenr) (f ++ reverse r) 0 [] phi)

checkw :: PhyQueue a -> PhyQueue a
checkw (PQ [] lenf f lenr r phi) = PQ f lenf f lenr r phi
checkw pq = pq

instance Queue PhyQueue where
  empty = PQ [] 0 [] 0 [] 0
  isEmpty (PQ _ lenf _ _ _ _) = lenf == 0
  snoc (PQ w lenf f lenr r phi) x = check $ PQ w lenf f (lenr+1) (x:r) phi
  head (PQ [] _ _ _ _ _) = error "empty queue"
  head (PQ (x:_) _ _ _ _ _) = x
  tail (PQ [] _ _ _ _ _) = error "empty queue"
  tail (PQ (_:w) lenf f lenr r phi) = check $ PQ w (lenf - 1) (Prelude.tail f) lenr r phi
