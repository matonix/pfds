{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import PFDS.Commons.Heap (Heap (..))
-- import PFDS.Commons.Ex7
-- import PFDS.Commons.SplayHeap (SplayHeap (..))
-- import PFDS.Commons.Ex8a
-- import PFDS.Commons.PairingHeap (PairingHeap (..))
-- import PFDS.Commons.Ex8b
import PFDS.Commons.BinomialHeap (BinomialHeap (..), Tree (..))

import Data.Graph.Inductive (mkGraph, LNode, LEdge, Node, Gr)
import Data.Graph.Inductive.Dot (showDot, fglToDotString)
import System.Process

path :: String
path = "src/PFDS/Sec5/Ex9BinomialAfter"

main :: IO ()
main = do
  -- let a = foldl (flip insert) (empty::BinomialHeap Int) [3,4,5,2]
  -- let a = foldl (flip insert) (empty::BinTree Int) [3,4,5,2]
  -- let b = foldl (flip insert) (empty::BinTree Int) [6,7,8,1]
  let dot = showDot . fglToDotString . constructGraphs
          $ map (foldl (flip insert) (empty::BinomialHeap Int)) [[1..8]]
          -- $ map (foldl (flip insert) (empty::PairingHeap Int)) [[4,5,6,3,7,8,9,2,10,11,12,1]]
          -- $ map (deleteMin . foldl (flip insert) (empty::PairingHeap Int)) [[1..10]]
          -- $ map (toBinary . foldl (flip insert) (empty::PairingHeap Int)) [[4,5,6,3,7,8,9,2,10,11,12,1]]
          -- $ [a, b, merge a b] ++ takeWhile (not . isEmpty) (iterate deleteMin (merge a b))
          -- $ map construct ([[1..x] | x <- [0..5::Int]] ++ [[1,2,3,4,5,0]])
          -- $ takeWhile (not . isEmpty) (iterate deleteMin a)
  writeFile (path ++ ".dot") dot
  print =<< system ("dot -Tpng -o" ++ path ++ ".png " ++ path ++ ".dot")

type GraphBase = ([LNode String], [LEdge String])

class DotView a where
  cons :: (GraphBase, [Node]) -> a -> (GraphBase, [Node])

constructGraphs :: DotView a => [a] -> Gr String String
constructGraphs = uncurry mkGraph . fst . foldl cons (([], []), [0..])

-- instance Show a => DotView (SplayHeap a) where
--   cons (les, n:ns) = cons' (les, ns) n where
--     cons' ((ls, es), n:ns) p E = (((n, "E"):ls, (p, n, ""):es), ns)
--     cons' ((ls, es), n:ns) p (T a x b) = cons' ((ls', es'), ns') n b where
--       ((ls', es'), ns') = cons' (((n, show x):ls, (p, n, ""):es), ns) n a

-- instance Show a => DotView (BinTree a) where
--   cons (les, n:ns) = cons' (les, ns) n where
--     cons' ((ls, es), n:ns) p E' = (((n, "E"):ls, (p, n, ""):es), ns)
--     cons' ((ls, es), n:ns) p (T' x a b) = cons' ((ls', es'), ns') n b where
--       ((ls', es'), ns') = cons' (((n, show x):ls, (p, n, ""):es), ns) n a

-- instance Show a => DotView (PairingHeap a) where
--   cons ((ls, es), p:c:ns) E = (((c, "E"):ls, (p, c, ""):es), ns)
--   cons ((ls, es), p:c:ns) (T x hs) = cons' (((c, show x):ls, (p, c, ""):es), c:ns) hs where
--     cons' ((ls, es), _:ns) [] = ((ls, es), ns)
--     cons' ((ls, es), p:c:ns) (h:hs) = cons' ((ls', es'), p:ns') hs where
--       ((ls', es'), ns') = cons ((ls, es), p:c:ns) h

instance Show a => DotView (BinomialHeap a) where
  cons a (BH xs) = foldl cons a xs

instance Show a => DotView (Tree a) where
  cons ((ls, es), p:c:ns) (Node _ x []) = (((c, show x):ls, (p, c, ""):es), ns)
  cons ((ls, es), p:c:ns) (Node _ x hs) = cons' (((c, show x):ls, (p, c, ""):es), c:ns) hs where
    cons' ((ls, es), _:ns) [] = ((ls, es), ns)
    cons' ((ls, es), p:c:ns) (h:hs) = cons' ((ls', es'), p:ns') hs where
      ((ls', es'), ns') = cons ((ls, es), p:c:ns) h
