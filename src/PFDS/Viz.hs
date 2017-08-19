{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import PFDS.Sec5.Ex7
import PFDS.Sec5.SplayHeap (SplayHeap (..))
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import System.Process

path :: String
path = "src/PFDS/Sec5/Ex7"

main :: IO ()
main = do
  let dot = showDot . fglToDotString
          . constructGraphs
          $ map construct [[1..x] | x <- [0..5]]
  writeFile (path ++ ".dot") dot
  print =<< system ("dot -Tpng -o" ++ path ++ ".png " ++ path ++ ".dot")

constructGraphs :: [SplayHeap Int] -> Gr String String
constructGraphs = uncurry mkGraph . fst . foldl cons (([], []), [0..])

type GraphBase = ([LNode String], [LEdge String])

cons :: (GraphBase, [Node]) -> SplayHeap Int -> (GraphBase, [Node])
cons ((_, _), []) _ = error "empty"
cons ((lns, es), n:ns) h = cons' ((lns, es), ns) n h where
  cons' ((_, _), []) _ _ = error "empty"
  cons' ((lns, es), n:ns) parent E = (((n, "E"):lns, (parent, n, ""):es), ns)
  cons' ((lns, es), n:ns) parent (T a x b) = cons' ((lns', es'), ns') n b where
    ((lns', es'), ns') = cons' (((n, show x):lns, (parent, n, ""):es), ns) n a
