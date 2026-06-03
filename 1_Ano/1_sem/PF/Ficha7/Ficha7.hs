module Ficha7 where

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

exp1 :: ExpInt
exp1 = Mais (Const 3) (Menos (Const 2) (Const 5)) -- (3 + (2 - 5))

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = (-1)*calcula e 
calcula (Mais e e1) = calcula e + calcula e1
calcula (Menos e e1) = calcula e - calcula e1
calcula (Mult e e1) = calcula e * calcula e1

infixa :: ExpInt -> String
infixa (Const x) = show x 
infixa (Simetrico e) = "-(" ++ infixa e ++ ")" 
infixa (Mais e e1) = "(" ++ infixa e ++ "+" ++ infixa e1 ++ ")" 
infixa (Menos e e1) = "(" ++ infixa e ++ "-" ++ infixa e1 ++ ")" 
infixa (Mult e e1) = "(" ++ infixa e ++ "*" ++ infixa e1 ++ ")" 

posfixa :: ExpInt -> String
posfixa (Const x) = show x ++ " " 
posfixa (Simetrico e) = posfixa e ++ "*(-1)" 
posfixa (Mais e e1) = posfixa e ++ posfixa e1 ++ "+" 
posfixa (Menos e e1) = posfixa e ++ posfixa e1 ++ "-" 
posfixa (Mult e e1) = posfixa e ++ posfixa e1 ++ "*"

data RTree a = R a [RTree a] deriving Show

a1 :: RTree Int
a1 = R 1 [R 2 [R 6 [R 7 [], 
                    R 8 [R 10 []], 
                    R 9 []], 
               R 3 [R 4 [], 
                    R 5 []]]]

soma :: Num a => RTree a -> a
--soma a1 = 55
soma (R a []) = a 
soma (R a r) = a + sum (map soma r)

size :: RTree a -> Int
size (R a []) = 1
size (R a l) = 1 + sum (map size l)

mirror :: RTree a -> RTree a 
mirror (R x []) = R x []
mirror (R z l) = R z (reverse (map mirror l))

sel :: [Int] -> RTree a -> Maybe (RTree a)
sel [] a = Just a
sel (h:t) (R x l) | h > (length l -1) = Nothing
                  | otherwise    = sel t (l !! h)

posfixa' :: RTree a -> [a]
posfixa' (R a []) = [a]
posfixa' (R a l) = concatMap posfixa' l++[a]

