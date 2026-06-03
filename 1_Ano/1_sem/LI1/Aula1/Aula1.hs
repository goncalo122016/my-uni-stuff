module Aula1 where

areaq :: Int -> Int
areaq l = l*l

perimetror :: Int -> Int -> Int
perimetror c l = 2*c+2*l

verif :: Char -> String -> Bool
verif x l = elem x l

remov :: [a] -> a
remov x = if mod (length x) 2 == 0 then head x else last x

parl :: [a] -> (a,a)
parl x = (head x, last x)

punome :: [String] -> (String, String)
punome x = (head x, last x)

list :: ([a],[b]) -> (a,[b])
list (xs,ys) = (head xs, ys)

punome' :: [String] -> String
punome' (x:y) = [head x] ++ "." ++ last y