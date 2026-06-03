module Ficha9 where

data LTree a = Leaf a 
             | Fork (LTree a) (LTree a)

a1,a2,a3 :: LTree Int

a1 = Leaf 1

a2 = Fork (Leaf 2) (Leaf 3)

a3 = Fork a2 (Fork a1 a2)

size :: LTree a -> Int
size (Leaf _) = 1
size (Fork e d) = size e + size d

altura :: LTree a -> Int
altura (Leaf _) = 1
altura (Fork e d) = 1 + max (altura e) (altura d)

el :: LTree a -> [a]
el (Leaf x) = [x]
el (Fork e d) = el e ++ el d

el2 :: LTree a -> [(a,Int)]
--el2 a3 = [(2,3),(3,3),(1,3),(2,4),(3,4)] 
el2 (Leaf x) = [(x,1)]
el2 (Fork e d) = [(x,y+1) | (x,y) <- el2 e ++ el2 d]

instance Show a => Show (LTree a) where
    --show :: LTree a -> String
    show (Leaf a) = show a
    show (Fork e d) = "(" ++ show e ++ "^" ++ show d ++")"

mapLTree :: (a -> b) -> LTree a -> LTree b
mapLTree f (Leaf x) = Leaf (f x)
mapLTree f (Fork e d) = Fork (mapLTree f e) (mapLTree f d)

--class Functor t where  --CLASSES DE CONSTRUTORES 
--    fmap :: (a -> b) -> t a -> t b

instance Functor LTree where
    --fmap :: (a -> b) -> LTree a -> LTree b
    fmap = mapLTree

data Exp a = Const a
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

e = Mais (Mult (Const 3) (Const 1)) (Const 5)

instance Show a => Show (Exp a) where
    --show :: Exp a -> String
    show (Const a) = show a
    show (Mais l r) = "(" ++ show l ++ "+" ++ show r ++")"
    show (Menos l r) = "(" ++ show l ++ "-" ++ show r ++")"
    show (Mult l r) = "(" ++ show l ++ "*" ++ show r ++")"

instance Functor Exp where
    --fmap :: (a -> b) -> Exp a -> Exp b
    fmap f (Const x) = Const (f x)
    fmap f (Mais l r) = Mais (fmap f l) (fmap f r)
    fmap f (Menos l r) = Menos (fmap f l) (fmap f r)
    fmap f (Mult l r) = Mult (fmap f l) (fmap f r)

valor :: Num a => Exp a -> a
valor (Const a) = a
valor (Mais e1 e2) = valor e1 + valor e2
valor (Menos e1 e2) = valor e1 - valor e2
valor (Mult e1 e2) = valor e1 * valor e2

instance Num a => Num (Exp a) where
    (+) e1 e2 = Mais e1 e2
    (-) e1 e2 = Menos e1 e2
    (*) e1 e2 = Mult e1 e2
    negate e = Mult (Const (-1)) e
    abs e = Mult e (signum e)
    signum e = Const (signum (valor e))
    fromInteger i = Const (fromInteger i)
