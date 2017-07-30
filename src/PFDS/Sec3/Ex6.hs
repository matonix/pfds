{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PFDS.Sec3.Ex6 where
{-# ANN module "HLint: ignore Eta reduce" #-}

class Ord e => BinomialHeap h e where
  empty :: [(Int, h e)]
  isEmpty :: [(Int, h e)] -> Bool
  rank :: (Int, h e) -> Int
  root :: (Int, h e) -> e
  link :: (Int, h e) -> (Int, h e) -> (Int, h e)
  insTree :: (Int, h e) -> [(Int, h e)] -> [(Int, h e)]
  insert :: e -> [(Int, h e)] -> [(Int, h e)]
  merge :: [(Int, h e)] -> [(Int, h e)] -> [(Int, h e)]
  removeMinTree :: [(Int, h e)] -> ((Int, h e), [(Int, h e)])
  findMin :: [(Int, h e)] -> e
  deleteMin :: [(Int, h e)] -> [(Int, h e)]

data Tree e = Node e [Tree e] deriving (Show)
type Heap e = [(Int, Tree e)]

instance Ord e => BinomialHeap Tree e where

  empty = []
  isEmpty = null

  rank = fst
  root (_, Node x _) = x
  link (r, Node x1 c1) (_, Node x2 c2) =
    if x1 <= x2
      then (r+1, Node x1 (Node x2 c2:c1))
      else (r+1, Node x2 (Node x1 c1:c2))

  insTree t [] = [t]
  insTree t ts@(t':ts') =
    if rank t < rank t'
      then t:ts
      else insTree (link t t') ts'

  insert x ts = insTree (0, Node x []) ts

  merge ts1 [] = ts1
  merge [] ts2 = ts2
  merge ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1:merge ts1' ts2
    | rank t2 < rank t1 = t2:merge ts1 ts2'
    | otherwise = insTree (link t1 t2) (merge ts1' ts2')

  removeMinTree [] = error "empty"
  removeMinTree [t] = (t, [])
  removeMinTree (t:ts) = let
    (t', ts') = removeMinTree ts
    in if root t <= root t'
      then (t, ts)
      else (t', t:ts')

  findMin = minimum . map root

  deleteMin ts =
    let ((_, Node _ ts1), ts2) = removeMinTree ts
    in merge (zip [0..] $ reverse ts1) ts2
