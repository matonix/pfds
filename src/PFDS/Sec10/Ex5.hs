{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module PFDS.Sec10.Ex5 where

import PFDS.Commons.Queue
import Prelude hiding (head, tail)

data BQueue q a = E | Q Int [a] (q [a]) Int [a]
deriving instance (Show a, Show (q [a])) => Show (BQueue q a)

instance Queue q => Queue (BQueue q) where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [x] empty 0 []
  snoc (Q lenfm f m lenr r) x = checkQ (Q lenfm f m (lenr + 1) (x : r))

  head E = error "Empty"
  head (Q lenfm (x : f') m lenr r) = x

  tail E = error "Empty"
  tail (Q lenfm (x : f') m lenr r) = checkQ (Q (lenfm - 1) f' m lenr r)

checkQ :: Queue q => BQueue q a -> BQueue q a
checkQ q@(Q lenfm f m lenr r) = if lenr <= lenfm
  then checkF q
  else checkF (Q (lenfm + lenr) f (snoc m (reverse r)) 0 [])

checkF :: Queue q => BQueue q a -> BQueue q a
checkF (Q lenfm [] (isEmpty -> True) lenr r) = E
checkF (Q lenfm [] m lenr r) = Q lenfm (head m) (tail m) lenr r
checkF q = q

-- (b) PrimQ に実時間キューを指定した場合
-- snoc も tail も内部で checkQ, checkF を呼ぶが、
-- 内部で呼ばれる snoc, head, tail は実時間キューのものなので O(1) である。
-- ゆえにすべての操作が O(1) 償却時間となる。
