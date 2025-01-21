module Aula4 where
import Test.HUnit

mySum :: Int -> Int -> Int
mySum x n | n==0 = x
          | n>0 = let f y = y + 1 in f(mySum x (n-1))
          | n<0 = let f y = y - 1 in f(mySum x (n+1))

testesMySum = test ["x+0 = x, para x=4"~: 4 ~=? mySum 4 0,
                    "1 + 2 = 3" ~: 1 ~=? mySum 1 2
                   ]

{-| Esta função procura um elemento na lista e devolve a sua associação, ou 0 caso contrário

== Exemplo

>>> busca "Ana" [("Ana",3),("João",4)] = 3

-}

busca :: Eq a => a -> [(a,Int)] -> Int
busca _ [] = 0
busca x ((a,b):t) | x==a = b
                  | otherwise = busca x t

testesBusca = test ["o valor de busca Ana tem de ser 3" ~: 3 ~=? busca "Ana" [("Ana",3),("João",4)],
                    " o valor da função em qualquer lista vazia (no segundo argumento) é 0" ~: 0 ~=? busca "Ana" []
                   ]

{-| Esta função subtrai um valor se possível.

== Exemplo

>>> subtrai 10 11 = 0
>>> subtrai 10 9 = 1
-}

subtrai :: Int -> Int -> Int
subtrai x y | x<y = 0
            | otherwise = x - y

testesSubtrai = test ["Se o primeiro número for menor que o segundo a função retorna 0" ~: 0 ~=? subtrai 10 11, 
                      "Se o primeiro número for maior que o segundo a função poderá ser realizada" ~: 1 ~=? subtrai 11 10
                     ]

{-| Esta função encontra a menor ordenada. 0, caso contrário.

== Exemplo

>>> menor [('x', 0), ('x', -1)] = -1
>>> menor [] = 0
-}

menor :: [(a,Int)] -> Int
menor [] = 0
menor [(_,b)] = b
menor ((x,a):(x1,b):t) | a<b = menor ((x,a):t)
                       | otherwise = menor ((x1,b):t)

testesMenor = test ["Se a função for invocada para lista vazia deverá retornar 0" ~: 0 ~=? menor [],
                    "Dados pares numa lista não vazia compara, dando o menor, entre as segundas componentes de cada par" ~: -1 ~=? menor [('x', 0), ('x', -1)]
                   ]

myMult :: Int -> Int -> Int
myMult x n | n==0 = 0
           | n>0 = let f y = mySum y x in f (myMult x (n-1))
           | n<0 = let f y = y - x in f (myMult x (n+1))

myExp :: Int -> Int -> Int
myExp x n | n==0 = 1
          | n>0 = let f y = myMult y x in f (myExp x (n-1))
          | n<0 = let f y = y `div` x in f (myExp x (n-1))

notafinal :: [Float] -> Float -> Float
notafinal c p = 0.4*(ncontinua c) + 0.6*p

ncontinua :: [Float] -> Float
ncontinua [] = 0
ncontinua (h:t) | (h <= 2 && h >= 0) = h+ncontinua t

type Matriz = [[Int]]

m1 :: Matriz
m1 = [[1,1,1,0],
      [1,0,2,0],
      [1,0,1,0],
      [0,0,0,1]]

trocaL :: Matriz -> Matriz
trocaL [] = [] 
trocaL (h:t) = [last t] ++ init t ++ [h] 

trocaC :: Matriz -> Matriz
trocaC [] = []
trocaC (h@(h1:t1):t) = [[last h]++(init t1)++[h1]] ++ trocaC t


elemInd :: Eq a => a -> [a] -> Int
elemInd x [] = -1
elemInd x l = elemIndAux x l 0

elemIndAux :: Eq a => a -> [a] -> Int -> Int
elemIndAux _ [] _ = -1
elemIndAux x (h:t) i | x == h = i
                     | otherwise = elemIndAux x t (i+1)

mudaElem :: [a] -> Int -> a -> [a]
mudaElem [] _ x = [x]
mudaElem (h:t) 0 a = (a:t)
mudaElem (h:t) i a | length (h:t) > i = h: mudaElem t (i-1) a 
                   | otherwise = (h:t)

posElem :: Int -> Matriz -> (Int,Int)
posElem x m = (contaL x m 0, contaC x m 0)

contaL x [] n = -1
contaL x (h:t) i | elem x h = i
                 | otherwise = contaL x t (i+1)

contaC x [] n = -1
contaC x ([]:t) i = contaC x t (i - length (head t))
contaC x ((hh:th):t) i | x == hh = i
                       | otherwise = contaC x (th:t) (i+1)

mudaElemMatr :: Matriz -> Int -> (Int,Int) -> Matriz
mudaElemMatr [] _ _ = []
mudaElemMatr (h:t) x (l,c) | l==0 = (mudaElem h c x):t
                           | otherwise = h:mudaElemMatr t x (l-1,c)

splitat :: Int -> [a] -> ([a], [a])
splitat 0 (h:t)  = ([h],t)
splitat n (h:t) = (h:a,b)
                  where (a,b) = splitat (n-1) t

drop' :: Int -> [a] -> [a]
drop' n x | n<=0 = x
drop' _ [] = []
drop' n (h:t) = drop' (n-1) t

take' :: Int -> [a] -> [a]
take' n _ | n<=0 = []
take' _ [] = []
take' n (h:t) = h:take' (n-1) t