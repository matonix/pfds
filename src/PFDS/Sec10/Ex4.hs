module PFDS.Sec10.Ex4
  ( BQueue(..)
  , EL(..)
  ) where

import PFDS.Commons.Queue
import Prelude hiding (head, tail)

data EL a = Elem a | List [EL a] deriving (Show)
data BQueue a = E | Q Int [EL a] (BQueue a) Int [EL a] deriving (Show)

instance Queue BQueue where

  empty = E

  isEmpty E = True
  isEmpty _ = False

  snoc E x = Q 1 [Elem x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (Elem x : r)

  head E = error "Empty"
  head (Q _ (Elem x : _) _ _ _) = x

  tail E = error "Empty"
  tail (Q lenfm (_ : f') m lenr r) = checkQ (lenfm - 1) f' m lenr r

checkQ :: Int -> [EL a] -> BQueue a -> Int -> [EL a] -> BQueue a
checkQ lenfm f m lenr r = if lenr <= lenfm
  then checkF lenfm f m lenr r
  else checkF (lenfm + lenr) f (snocEL m (reverse r)) 0 []

checkF :: Int -> [EL a] -> BQueue a -> Int -> [EL a] -> BQueue a
checkF lenfm [] E lenr r = E
checkF lenfm [] m lenr r = Q lenfm (headEL m) (tail m) lenr r
checkF lenfm f m lenr r = Q lenfm f m lenr r

snocEL :: BQueue a -> [EL a] -> BQueue a
snocEL E x = Q 1 [List x] E 0 []
snocEL (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (List x : r)

headEL :: BQueue a -> [EL a]
headEL E = error "Empty"
headEL (Q _ (List x : _) _ _ _) = x
