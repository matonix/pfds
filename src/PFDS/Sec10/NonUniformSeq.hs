module PFDS.Sec10.NonUniformSeq where

data Seq a = Nil' | Cons' a (Seq (a, a)) deriving (Show)

sizeS :: Seq a -> Int
sizeS Nil' = 0
sizeS (Cons' x ps) = 1 + 2 * sizeS ps

sample :: Seq Int
sample = Cons' 0 (Cons' (1, 2) (Cons' ((3, 4), (5, 6)) Nil'))

replicateLog :: Int -> a -> Seq a
replicateLog 0 _ = Nil'
replicateLog n x = Cons' x (replicateLog (n-1) (x, x))
