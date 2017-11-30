import PFDS.Sec6.Ex6 (PhyQueue)

import Prelude          hiding (head, tail)

data Opr = Snoc Int | Tail

main :: IO ()
main = putStr "todo!"
-- main = do
--   let qs = scanl f empty [Snoc 1, Snoc 2, Snoc 3, Tail, Tail]
--   mapM show qs
--
-- f :: PhyQueue -> Opr -> PhyQueue
-- f
