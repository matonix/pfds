module PFDS.Sec5.Ex8a where

import PFDS.Sec5.PairingHeap (PairingHeap (..))

data BinTree e = E' | T' e (BinTree e) (BinTree e) deriving (Show)

toBinary :: PairingHeap e -> BinTree (Maybe e)
toBinary E = E'
toBinary (T e []) = T' (Just e) E' E'
toBinary (T e [h]) = T' (Just e) (toBinary h) E'
toBinary (T e hs) = loop (Just e) (map toBinary hs) where
  loop _ [] = E'
  loop e' [t] = T' e' t E'
  loop e' ts@(_:_:_) = loop e' $ pairs ts where
    pairs [] = []
    pairs [t] = [t]
    pairs (t1:t2:ts') = T' Nothing t1 t2 : pairs ts'

{-| Doctests for PairingHeap

>>> (print . foldl (flip insert) (empty::PairingHeap Int)) [1..5]
T 1 [T 5 [],T 4 [],T 3 [],T 2 []]

>>> (print . toBinary . foldl (flip insert) (empty::PairingHeap Int)) [1..5]
T' (Just 1) (T' Nothing (T' Nothing (T' (Just 5) E' E') (T' (Just 4) E' E')) (T' Nothing (T' (Just 3) E' E') (T' (Just 2) E' E'))) E'

-}
