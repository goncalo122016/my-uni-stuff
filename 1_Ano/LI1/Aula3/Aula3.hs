{-|
Module:Ficha3
Copyright: Gonçalo

Funções Recursivas...

Comandos importantes:

haddock -h Aula3.hs

xdg-open index.html

-}

module Aula3 where
import Data.Char

{-|  A função ’fact’ calcula o factorial (@fact n@ retorna o factorial
de de um inteiro @n@).
@
fact n = if n>0
then n*fact (n-1)
else 1
@
== Exemplos 

>>> fact 5
120

-} 
addi :: Int -> [Int] -> [Int]
addi x [] = []
addi x (h:t) = (h+x):addi x t 

striremov :: Char -> [String] -> [String]
striremov c [] = []
striremov c ("":t) = []
striremov c (h:t) | head h == c = striremov c t
                  | otherwise = h:striremov c t

dseg :: Char -> Char
dseg x | ord x==ord '9' = '0'
       | ord x>=ord '0' && ord x<=ord '8' = chr (1 + ord x)

dseg' :: [Char] -> [Char]
dseg' [] = []
dseg' (h:t) = dseg h: dseg' t 

vseg :: Char -> Char
vseg x | x=='a' = 'e'
       | x=='e' = 'i'
       | x=='o' = 'u'
       | x=='u' = 'a'

vseg' :: [Char] -> [Char]
vseg' [] = []
vseg' (h:t) = vseg h: vseg' t

type Nome = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao p [] = p
posicao (Pos n (x,y)) m | head m == N = posicao (Pos n (x,y+1)) (tail m)
                        | head m == S = posicao (Pos n (x,y-1)) (tail m)
                        | head m == E = posicao (Pos n (x+1,y)) (tail m)
                        | head m == W = posicao (Pos n (x-1,y)) (tail m)

posicoesM:: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM [] m = []
posicoesM (h:t) m = (posicao h [m]):(posicoesM t m)

posicoesMs:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs [] m = []
posicoesMs p [] = p
posicoesMs p m@(h:t) = posicoesMs (posicoesM p h) t

pessoasNorte' :: [PosicaoPessoa] -> Nome
pessoasNorte' [] = ""
pessoasNorte' (p:xs) = let (Pos nome _) = pessoasNorte p xs
                       in nome

pessoasNorte :: PosicaoPessoa -> [PosicaoPessoa] -> PosicaoPessoa
pessoasNorte p [] = p
pessoasNorte (Pos nome (x,y)) (Pos nome2 (x2,y2):xs) | y > y2 = pessoasNorte (Pos nome (x,y)) xs
                                                     | otherwise = pessoasNorte (Pos nome2 (x2,y2)) xs


movlpos :: Int -> [Int] -> [Int]
movlpos 0 x = x 
movlpos _ [] = []
movlpos n x | n>=1 = movlpos (n-1) (last x:init x)

movlpos' :: Int -> [Int] -> [Int]
movlpos' 0 x = x 
movlpos' _ [] = []
movlpos' n x@(h:t) | n>=1 = movlpos' (n-1) (t++[h])
