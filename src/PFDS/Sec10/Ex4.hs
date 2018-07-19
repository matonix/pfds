-- 自信がない
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

  snoc q x = snocEL 1 q (Elem x)

  head E = error "Empty"
  head (Q lenfm f m lenr r) = let (x, _) = unconsEL f
    in x

  tail E = error "Empty"
  tail (Q lenfm f m lenr r) = let (_, f') = unconsEL f
    in checkQ (Q (lenfm - 1) f' m lenr r)

checkQ :: BQueue a -> BQueue a
checkQ q@(Q lenfm f m lenr r) = if lenr <= lenfm
  then checkF q
  else checkF (Q (lenfm + lenr) f (snocEL lenr m (List (reverse r))) 0 [])

checkF :: BQueue a -> BQueue a
checkF (Q lenfm [] E lenr r) = E
checkF (Q lenfm [] m lenr r) = Q lenfm [Elem (head m)] (tail m) lenr r
checkF q = q

snocEL :: Int -> BQueue a -> EL a -> BQueue a
snocEL lenx E x = Q lenx [x] E 0 []
snocEL lenx (Q lenfm f m lenr r) x = checkQ (Q lenfm f m (lenr + lenx) (x : r))

unconsEL :: [EL a] -> (a, [EL a])
unconsEL [] = error "Empty"
unconsEL (Elem x : xs) = (x, xs)
unconsEL (List xs : xss) = let (x, xs') = unconsEL xs
  in case xs' of
    [] -> (x, xss)
    xs' -> (x, List xs' : xss)
