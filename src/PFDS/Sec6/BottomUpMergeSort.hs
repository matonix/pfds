module PFDS.Sec6.BottomUpMergeSort where

data Sortable a = Sortable Int [[a]]

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs
