module PFDS.Sec10.BootstrappedQueue
  ( BQueue(..)
  ) where

import PFDS.Commons.Queue
import Prelude hiding (head, tail)

data BQueue a = E | Q Int [a] (BQueue [a]) Int [a] deriving (Show)

instance Queue BQueue where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ (Q lenfm f m (lenr + 1) (x : r))

  head E = error "Empty"
  head (Q lenfm (x : f') m lenr r) = x

  tail E = error "Empty"
  tail (Q lenfm (x : f') m lenr r) = checkQ (Q (lenfm - 1) f' m lenr r)

checkQ :: BQueue a -> BQueue a
checkQ q@(Q lenfm f m lenr r) = if lenr <= lenfm
  then checkF q
  else checkF (Q (lenfm + lenr) f (snoc m (reverse r)) 0 [])

checkF :: BQueue a -> BQueue a
checkF (Q lenfm [] E lenr r) = E
checkF (Q lenfm [] m lenr r) = Q lenfm (head m) (tail m) lenr r
checkF q = q
