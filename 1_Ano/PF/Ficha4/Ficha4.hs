module Ficha4 where

divi :: Int -> Int -> Int
divi x y | x<y = 0
         | otherwise = 1+divi (x-y) y

modi :: Int -> Int -> Int
modi x y = x-(y*divi x y)

modi' x y | x>=y = modi (x-y) x
          | otherwise = x

divModi :: Int -> Int -> (Int,Int)
divModi x y | x>=y = (1+a,b)
            | otherwise = (0,x)
        where (a,b) = divModi (x-y) y

lengthOtmAux :: Int -> [a] -> Int 
lengthOtmAux x [] = x
lengthOtmAux x (h:t) = lengthOtmAux (x+1) t

lengthOtm :: [a] -> Int 
lengthOtm l = lengthOtmAux 0 l


reverseOtm :: [a] -> [a]
reverseOtm l = rev' [] l

rev' :: [a] -> [a] -> [a]
rev' l [] = l
rev' l (h:t) = rev' (h:l) t

fibb :: Int -> Int
fibb 0 = 0
fibb 1 = 1
fibb n = fibb (n-1) + fibb (n-2)


fibbOtm :: Integer -> Integer
fibbOtm n = fib' 0 1 n

fib' :: Integer -> Integer -> Integer -> Integer
fib' x y 0 = x
fib' x y 1 = y
fib' x y n = fib' y (x+y) (n-1)

digs :: Integer -> [Integer]
-- digs 14283 = [1,4,2,8,3]
digs x | div x 10 == 0 = [x]
digs x = digs (div x 10) ++ [mod x 10]

digs' :: Integer -> [Integer]
digs' x = digsAux [] x

digsAux :: [Integer] -> Integer -> [Integer] 
digsAux l x | x<10 = x:l
            | x>=10 = digsAux (mod x 10:l) (div x 10)

undigs :: [Integer] -> Integer
undigs [x] = x
undigs l = (10^(length l -1)* head l)+undigs (tail l)

undigs' :: [Integer] -> Integer
undigs' l = undigsAux 0 l

undigsAux :: Integer -> [Integer] -> Integer
undigsAux x [] = x
undigsAux x (h:t) = undigsAux (x*10+h) t 

undigsAlt :: [Integer] -> Integer
undigsAlt [x] = x
undigsAlt (x:h:t) = undigs (x*10+h:t)

nzp :: [Int] -> (Int,Int,Int)
nzp l = nzpAux 0 0 0 l

nzpAux :: Int -> Int -> Int-> [Int] -> (Int,Int,Int)
nzpAux n z p [] = (n,z,p)
nzpAux n z p (h:t) | h==0 = nzpAux n (z+1) p t
                   | h<0 = nzpAux (n+1) z p t
                   | h>0 = nzpAux n z (p+1) t

--Teórica
somatorio :: Num a => [a] -> a
somatorio l = somatorioAux l 0
     where somatorioAux :: Num a => [a] -> a -> a
           somatorioAux [] a = a
           somatorioAux (x:xs) a = somatorioAux xs (x+a)
        
maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (h:t) = maximoAc t h
     where maximoAc :: Ord a => [a] -> a -> a
           maximoAc [] ac = ac
           maximoAc (x:xs) ac | x>ac = maximoAc xs x
                              | otherwise = maximoAc xs ac
