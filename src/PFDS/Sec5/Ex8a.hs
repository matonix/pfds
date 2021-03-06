module PFDS.Sec5.Ex8a where

import PFDS.Commons.PairingHeap (PairingHeap (..))

data BinTree e = E' | T' e (BinTree e) (BinTree e) deriving (Show)

toBinary :: PairingHeap e -> BinTree e
toBinary E = E'
toBinary (T e []) = T' e E' E'
toBinary (T e (h1:hs)) = T' e (toBinary h1) (unloop hs) where
  unloop [] = E'
  unloop (E:_) = error "invalid"
  unloop (T e' hs':hs'') = T' e' (unloop hs') (unloop hs'')

{-| Doctests for PairingHeap

>>> (print . foldl (flip insert) (empty::PairingHeap Int)) [1..5]
T 1 [T 5 [],T 4 [],T 3 [],T 2 []]

>>> (print . toBinary . foldl (flip insert) (empty::PairingHeap Int)) [1..5]
T' (Just 1) (T' Nothing (T' Nothing (T' (Just 5) E' E') (T' (Just 4) E' E')) (T' Nothing (T' (Just 3) E' E') (T' (Just 2) E' E'))) E'

-}
