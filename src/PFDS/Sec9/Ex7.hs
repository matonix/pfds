-- Revisit PFDS.Sec3.Ex9

module PFDS.Sec9.Ex7 where

data Color = R | B deriving (Show)
data Tree a = E | T Color (Tree a) a (Tree a) deriving (Show)

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color e w f = T color e w f

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a' y' b' where
  T _ a' y' b' = ins s
  ins E = T R E x E
  ins s'@(T color a y b)
    | x < y = balance color (ins a) y b
    | x > y = balance color a y (ins b)
    | otherwise = s'

-- こっから

data Digit a = One a (Tree a)
             | Two a (Tree a) a (Tree a)

inc :: Digit a -> [Digit a] -> [Digit a]
inc (One e1 t1) [] = [One e1 t1]
inc (One e1 t1) (One e2 t2 : ds) = Two e1 t1 e2 t2 : ds
inc (One e1 t1) (Two e2 t2 e3 t3 : ds) =
  One e1 t1 : inc (One e2 (T B t2 e3 t3)) ds

add :: a -> [Digit a] -> [Digit a]
add e = inc (One e E)

link :: Tree a -> Digit a -> Tree a
link l (One e t) = T B l e t
link l (Two e1 t1 e2 t2) = T B (T R l e1 t1) e2 t2

fromOrdList :: Ord a => [a] -> Tree a
fromOrdList = foldl link E . foldr add []

data Shape = Emp | Top | Bar | Bot

instance Show Shape where
  show Emp = "    "
  show Top = "┌── "
  show Bar = "│   "
  show Bot = "└── "

kawaii :: Show a => Tree a -> [String]
kawaii = snd . kawaii'
  where
    kawaii' E = ([True], ["E"])
    kawaii' (T c t1 x t2) = (bs, ss)
      where
        ss = s1 ++ [kawa c ++ show x] ++ s2
        bs = genBools s1 ++ [True] ++ genBools s2
        s1 = genTrees shapeT Emp t1
        s2 = genTrees shapeB Bar t2
    genTrees f e = uncurry (zipWith (++) . map show . tail . scanl f e) . kawaii'
    genBools = flip replicate False . length
    kawa R = "○ "
    kawa B = "● "
    shapeT Emp False = Emp
    shapeT Emp True = Top
    shapeT Top False = Bar
    shapeT Bar False = Bar
    shapeB Bar False = Bar
    shapeB Bar True = Bot
    shapeB Bot False = Emp
    shapeB Emp False = Emp
