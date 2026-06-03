module Aula5 where

--FUNÇÕES DE ORDEM SUPERIOR

--definição de map do Prelude
map' :: (a-> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x): map f xs

triplos l = map (3*) l

somapares l = map aux l 
  where aux (x,y) = x+y

-- (\ x -> x+x) 30 = 60
-- (\ y x -> y*x) 3 7 = 21
-- (\ (x:xs) (y:ys) -> x+y) [1..3] [2,1] = 3

somapares' l = map (\(x,y) -> x+y) l

--definição de filter do Prelude
filter' :: (a-> Bool) -> [a] -> [a]
filter' cond [] = [] 
filter' cond (x:xs) = if cond x then x:filter' cond xs 
                      else filter' cond xs

positivo l = filter (>0) l

pares l = filter even l

multiplos :: Int -> [Int] -> [Int]
multiplos n l = filter (\ x -> mod x n ==0) l

--definição de (.) do Prelude
(.) :: (b-> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

-- (head . tail) [1,2,3] = 2

--definição de flip do Prelude
flip' :: (a -> b-> c) -> b -> a -> c
flip' f x y = f y x

--definição de foldr do Prelude
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f c [] = c
foldr' f c (h:t) = f h (foldr f c t)

l = [(1,"Ana"), (2,"Ze"), (7,"To")]

--queremos o somatorio da 1ª componente do par

somaPrimComp = foldr f 0 l
      where f (num, n) r = num + r

somaPrimComp' l  = foldr (\ (num, n) r -> num + r) 0 l

--definição de foldl do Prelude
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f c [] = c
foldl' f c (h:t) = foldl f (f c h) t 

l' = map snd l

--l'' = foldl (\ (num,n) r -> num:r) 0 l