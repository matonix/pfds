module PFDS.Sec10.BootstrappedQueue where

import qualified PFDS.Commons.Queue as Commons
import Prelude hiding (head, tail)

data Queue a = E | Q Int [a] (Queue [a]) Int [a]

instance Commons.Queue Queue where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ (Q lenfm f m (lenr + 1) (x : r))

  head E = error "Empty"
  head (Q lenfm (x : f') m lenr r) = x

  tail E = error "Empty"
  tail (Q lenfm (x : f') m lenr r) = checkQ (Q (lenfm - 1) f' m lenr r)

checkQ :: Queue a -> Queue a
checkQ q@(Q lenfm f m lenr r) = if lenr <= lenfm
  then checkF q
  else checkF (Q (lenfm + lenr) f (Commons.snoc m (reverse r)) 0 [])

checkF :: Queue a -> Queue a
checkF (Q lenfm [] E lenr r) = E
checkF (Q lenfm [] m lenr r) = Q lenfm (Commons.head m) (Commons.tail m) lenr r
checkF q = q
