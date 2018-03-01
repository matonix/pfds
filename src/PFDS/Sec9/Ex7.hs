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


-- consTree :: Tree a -> RList a -> RList a
-- consTree t [] = [One t]
-- consTree t1 (One t2 : ts) = Two t1 t2 : ts
-- consTree t1 (Two t2 t3 : ts) = One t1 : consTree (link t2 t3) ts

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
    kawaii' (T c t1 x t2) = (bools, t1' ++ [kawa c ++ show x] ++ t2')
      where
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
        bools = replicate (length t1') False ++ [True] ++ replicate (length t2') False
        t1' = uncurry (zipWith (++) . map show . tail . scanl shapeT Emp) $ kawaii' t1
        t2' = uncurry (zipWith (++) . map show . tail . scanl shapeB Bar) $ kawaii' t2
