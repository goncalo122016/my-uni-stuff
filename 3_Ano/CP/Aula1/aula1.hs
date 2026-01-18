module Aula1 where

{- ghci aula1.hs -}

{- Exercicio 1 -}
length' :: [a] -> Int
length' [] = 0
length' (h:t) = 1 + length' t

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

data LTree a = Leaf a | Fork (LTree a) (LTree a) deriving Show

countLT :: LTree a -> Int
countLT (Leaf x) = 1
countLT (Fork e d) = countLT e + countLT d

mirrorLT :: LTree a -> LTree a
mirrorLT (Leaf x) = Leaf x
mirrorLT (Fork e d) = Fork (mirrorLT d) (mirrorLT e)

{- Exercicio 2 -}
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' x (h:t) = h : take' (x-1) t

{- Exercício 3 -}
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = f h : map' f t

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (h:t)
    | p h = h : filter' p t
    | otherwise = filter' p t

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (h:t) = y ++ filter2 p t
    where y = if p h then [h]
                     else []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _  i [] = i;
foldr' f i (h:t) =  f h (foldr' f i t)

product' l = foldr' (*) 1 l

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a

{- Exercício 4 -}
(#) :: (b -> c) -> (a -> b) -> a -> c
(#) f g x = f (g x)

main :: IO ()
main = do
    print (length' [1,2,3,4])
    print (reverse' [1,2,3,4])
    print (countLT (Fork (Leaf 1) (Leaf 2)))
    print (mirrorLT (Fork (Leaf 1) (Leaf 2)))
    print (take' 2 [1,2,3,4])    
    print (map' even [1,2,3,4])
    print (filter' even [1,2,3,4])
    print (foldr' (+) 0 [1,2,3,4])
    print (product' [1,2,3])
    print (uncurry' max (5,3))
    print (curry' fst 3 6)
    print (flip' zip "abc" [1,2,3])
    print (((1+) # (2*)) 3)

