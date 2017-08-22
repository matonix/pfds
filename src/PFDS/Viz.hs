{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import PFDS.Sec3.Heap (Heap (..))
-- import PFDS.Sec5.Ex7
-- import PFDS.Sec5.SplayHeap (SplayHeap (..))
import PFDS.Sec5.Ex8a
import PFDS.Sec5.PairingHeap (PairingHeap (..))

import Data.Graph.Inductive (mkGraph, LNode, LEdge, Node, Gr)
import Data.Graph.Inductive.Dot (showDot, fglToDotString)
import System.Process

path :: String
path = "src/PFDS/Sec5/PairingHeap"

main :: IO ()
main = do
  let dot = showDot . fglToDotString
          . constructGraphs
          $ map (foldl (flip insert) (empty::PairingHeap Int)) [[1..5]]
          -- $ map (toBinary . foldl (flip insert) (empty::PairingHeap Int)) [[1..5]]
          -- $ map construct [[1..x] | x <- [0..5::Int]]
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

instance Show a => DotView (BinTree a) where
  cons (les, n:ns) = cons' (les, ns) n where
    cons' ((ls, es), n:ns) p E' = (((n, "E"):ls, (p, n, ""):es), ns)
    cons' ((ls, es), n:ns) p (T' x a b) = cons' ((ls', es'), ns') n b where
      ((ls', es'), ns') = cons' (((n, show x):ls, (p, n, ""):es), ns) n a

instance Show a => DotView (PairingHeap a) where
  cons ((ls, es), p:c:ns) E = (((c, "E"):ls, (p, c, ""):es), ns)
  cons ((ls, es), p:c:ns) (T x hs) = cons' (((c, show x):ls, (p, c, ""):es), c:ns) hs where
    cons' ((ls, es), _:ns) [] = ((ls, es), ns)
    cons' ((ls, es), p:c:ns) (h:hs) = cons' ((ls', es'), p:ns') hs where
      ((ls', es'), ns') = cons ((ls, es), p:c:ns) h
