{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- p.191見ながら…

module PFDS.Sec3.Ex7 where

class Ord e => Heap h e where
  empty :: Ord e => h e
  isEmpty :: Ord e => h e -> Bool
  insert :: Ord e => e -> h e -> h e
  merge :: Ord e => h e -> h e -> h e
  findMin :: Ord e => h e -> e
  deleteMin :: Ord e => h e -> h e

data Tree e = Node e [Tree e] deriving (Show)
newtype BinomialHeap e = BH [(Int, Tree e)] deriving (Show)

rank :: (Int, Tree e) -> Int
rank = fst
root :: (Int, Tree e) -> e
root (_, Node x _) = x
link :: Ord e => (Int, Tree e) -> (Int, Tree e) -> (Int, Tree e)
link (r, Node x1 c1) (_, Node x2 c2) =
  if x1 <= x2
    then (r+1, Node x1 (Node x2 c2:c1))
    else (r+1, Node x2 (Node x1 c1:c2))

insTree :: Ord e => (Int, Tree e) -> [(Int, Tree e)] -> [(Int, Tree e)]
insTree t [] = [t]
insTree t ts@(t':ts') =
  if rank t < rank t'
    then t:ts
    else insTree (link t t') ts'

mrg :: Ord e => [(Int, Tree e)] -> [(Int, Tree e)] -> [(Int, Tree e)]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
  | rank t1 < rank t2 = t1:mrg ts1' ts2
  | rank t2 < rank t1 = t2:mrg ts1 ts2'
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree :: Ord e => [(Int, Tree e)] -> ((Int, Tree e), [(Int, Tree e)])
removeMinTree [] = undefined
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = let
  (t', ts') = removeMinTree ts
  in if root t <= root t'
    then (t, ts)
    else (t', t:ts')

instance Ord e => Heap BinomialHeap e where
  empty = BH []
  isEmpty (BH ts) = null ts
  insert x (BH ts) = BH (insTree (0, Node x []) ts)
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)
  findMin (BH ts) = root (fst (removeMinTree ts))
  deleteMin (BH ts) =
    let ((_, Node _ ts1), ts2) = removeMinTree ts
    in BH (mrg (zip [0..] (reverse ts1)) ts2)

data ExplicitMin h e = E | NE e (h e) deriving (Show)

-- data Tree e = Node e [Tree e] deriving (Show)

instance Ord e => Heap (ExplicitMin BinomialHeap) e where

  empty = E

  isEmpty E = True
  isEmpty (NE _ _) = False

  insert x E = NE x (BH (insTree (0, Node x []) []))
  insert x (NE y (BH ts)) = NE (min x y) (BH (insTree (0, Node x []) ts))

  merge E E = E
  merge (NE x h) E = NE x h
  merge E (NE x h) = NE x h
  merge (NE x (BH ts1)) (NE y (BH ts2)) = NE (min x y) (BH (mrg ts1 ts2))

  findMin E = error "empty"
  findMin (NE x _) = x

  deleteMin E = error "empty"
  deleteMin (NE _ (BH ts)) =
    let ((_, Node x ts1), ts2) = removeMinTree ts
    in NE x (BH (mrg (zip [0..] (reverse ts1)) ts2))

-- t = insert 5 $ insert 1 $ insert 10 $ insert 3 $ empty :: ExplicitMin (BinomialHeap Int) Int
