module PFDS.Sec2.Ex1 where

-- suffixes [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]
suffixes :: [a] -> [[a]]
-- suffixes [] = [[]]
-- suffixes (x:xs) = (x:xs):suffixes xs
suffixes = scanr (:) []
