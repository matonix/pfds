module PFDS.Sec8.Ex6 where

import Prelude hiding (tail, init)
import PFDS.Commons.Deque
import PFDS.Commons.BankersDeque
import Debug.Trace

eval :: Int -> String -> BankersDeque Int
eval c = fst . foldl interplet (emptyc c, 0)

interplet :: (BankersDeque Int, Int) -> Char -> (BankersDeque Int, Int)
interplet (d, i) 'c' = trace ("cons: " ++ show (i `cons` d)) (i `cons` d, succ i)
interplet (d, i) 's' = trace ("snoc: " ++ show (d `snoc` i)) (d `snoc` i, succ i)
interplet (d, i) 't' = trace ("tail: " ++ show (tail d)) (tail d, i)
interplet (d, i) 'i' = trace ("init: " ++ show (init d)) (init d, i)
