module Ficha5 where
import Data.List

--foldr (resultado do caso base) (operação) (lista a aplicar) 

sum l = foldr 0 (+) l

ore :: [Bool] -> Bool
ore [] = False
ore (h:t) = h || (ore t)


--or' = foldr False (||) 

ande :: [Bool] -> Bool
ande [] = True
ande (h:t) = h && (ande t)

--and' = foldr True (&&)

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (h:t) | p h = True
             | otherwise = any' p t

--anyAlt :: (a -> Bool) -> [a] -> Bool
--anyAlt p l = or' (map p l)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t) 
      where insert x [] = [x]
            insert x (h:t) | x<=h = x:h:t
                           | otherwise = h:(insert x t)

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' cond (h:t) = ins cond h (sortOn' cond t)
    where ins c x [] = [x]
          ins c x (h:t) | c x <= c h = x:h:t
                        | otherwise = h:(ins c x t)

type Mat a = [[a]]

mat1 = [[1,2,3], 
        [0,4,5], 
        [0,0,6]]

mat2 = [[2,0,0], 
        [0,2,0], 
        [1,0,0]]

dimOk :: Mat a -> Bool
dimOk [] = True
dimOk l = all (== (length.head) l) (map length l)

dimMat :: Mat a -> (Int,Int)
dimMat m@(h:t) = (length m, length h) 

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m [] = m
addMat [] m = m
addMat (h:t) (h1:t1) = zipWith (+) h h1 : addMat t t1

transpose' :: Mat a -> Mat a
transpose' [] = []
transpose' m = map head m : transpose (map tail m)

transpos :: Mat a -> Mat a
transpos m = [ [(m !! j) !! i | j <- [0..l-1] ] | i <- [0..c-1]]
    where (l,c) = dimMat m

triSup :: Eq a => Num a => Mat a -> Bool
triSup [] = False
triSup m@(h:t) | fst (dimMat m) == snd (dimMat m) = isCres (map (length.takeWhile (==0)) m)
               | otherwise = False

isCres :: [Int] -> Bool
isCres [] = True
isCres [x] = True
isCres (h:s:t) | h<s = isCres (s:t)
               | otherwise = False