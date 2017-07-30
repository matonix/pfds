{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec3.Ex5 where
{-# ANN module "HLint: ignore Eta reduce" #-}

class Ord e => BinomialHeap h e where
  empty :: [h e]
  isEmpty :: [h e] -> Bool
  rank :: h e -> Int
  root :: h e -> e
  link :: h e -> h e -> h e
  insTree :: h e -> [h e] -> [h e]
  insert :: e -> [h e] -> [h e]
  merge :: [h e] -> [h e] -> [h e]
  removeMinTree :: [h e] -> (h e, [h e])
  findMin :: [h e] -> e
  deleteMin :: [h e] -> [h e]

data Tree e = Node Int e [Tree e] deriving (Show)

instance Ord e => BinomialHeap Tree e where

  empty = []
  isEmpty = null

  rank (Node r _ _) = r
  root (Node _ x _) = x
  link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2
      then Node (r+1) x1 (t2:c1)
      else Node (r+1) x2 (t1:c2)

  insTree t [] = [t]
  insTree t ts@(t':ts') =
    if rank t < rank t'
      then t:ts
      else insTree (link t t') ts'

  insert x ts = insTree (Node 0 x []) ts

  merge ts1 [] = ts1
  merge [] ts2 = ts2
  merge ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1:merge ts1' ts2
    | rank t2 < rank t1 = t2:merge ts1 ts2'
    | otherwise = insTree (link t1 t2) (merge ts1' ts2')

  removeMinTree [] = undefined
  removeMinTree [t] = (t, [])
  removeMinTree (t:ts) = let
    (t', ts') = removeMinTree ts
    in if root t <= root t'
      then (t, ts)
      else (t', t:ts')

  -- 3.5
  findMin = minimum . map root

  deleteMin ts =
    let (Node _ _ ts1, ts2) = removeMinTree ts
    in merge (reverse ts1) ts2
