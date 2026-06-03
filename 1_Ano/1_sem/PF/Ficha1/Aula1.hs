module Aula1 where

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

soma :: (Int,Int) -> Int
soma (x,y)= x+y

soma2 :: Int -> Int -> Int
soma2 x y = x+y

soma3 :: Int -> Int
soma3 = soma2 3
