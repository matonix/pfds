module PFDS.Commons.BottomUpMergeSort where

data Sortable a = Sortable Int [[a]]

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x:xs') ys@(y:ys') = if x <= y
  then x:mrg xs' ys
  else y:mrg xs ys'

empty :: Sortable a
empty = Sortable 0 []

add :: Ord a => a -> Sortable a -> Sortable a
add x (Sortable size segs) =
  Sortable (size+1) (addSeg [x] segs size)
  where
    addSeg seg (seg':segs) size = if even size
      then seg:seg':segs
      else addSeg (mrg seg seg') segs (size `div` 2)

sort :: Ord a => Sortable a -> [a]
sort (Sortable _ segs) = foldl mrg [] segs
