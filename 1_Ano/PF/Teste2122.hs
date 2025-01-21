module Teste2122 where
import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Data.Char

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:xs) = (h,x): zip' t xs

preCres :: Ord a => [a] -> [a]
preCres [] = []
preCres [x] = [x]
preCres (h:s:t) | h>=s = [h]
                | otherwise = h: preCres (s:t)

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = maximum l - minimum l

type Mat a = [[a]]

soma :: Num a => Mat a -> Mat a -> Mat a
soma [] [] = []
soma (h:t) (h':t') = zipWith (+) h h' : soma t t'

type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance Show Agenda where
    show Vazia = ""
    show agenda = inorder (sortByName agenda)

sortByName :: Agenda -> [(Nome, [Telefone])]
sortByName Vazia = []
sortByName (Nodo info esq dir) =
                sort (sortByName esq ++ [info] ++ sortByName dir)

inorder :: [(Nome, [Telefone])] -> String
inorder [] = ""
inorder ((nome, telefones):resto) =
                nome ++ ": " ++ showTelefones telefones ++ "\n" ++ inorder resto

showTelefones :: [Telefone] -> String
showTelefones [] = ""
showTelefones [t] = show t
showTelefones (t:ts) = show t ++ " / " ++ showTelefones ts

agen = Nodo ("Alice", [123456789, 987654321]) (Nodo ("Bob", [111222333]) Vazia Vazia) (Nodo ("Charlie", [444555666, 777888999]) Vazia Vazia)

randomSel :: Eq a => Int -> [a] -> IO [a]
randomSel n [] = return []
randomSel n l = do if n >= length l then return l
                                    else return (selAux n [] l)

selAux :: Eq a => Int -> [a] -> [a] -> [a]
selAux 0 x _ = x
selAux n noval l = selAux (n-1) (a:noval) (delete a l)
                        where a = exchange (escolhe l)
                              exchange :: IO a -> a
                              exchange = unsafePerformIO


escolhe :: Eq a => [a] -> IO a
escolhe l = do n <- randomRIO (0, length l - 1)
               return (l !! n)

organiza :: Eq a => [a] -> [(a,[Int])]
organiza [] = []
organiza l@(h:t) = map (\x -> (x, elemInd x l)) (nub l)

elemInd :: Eq a => a -> [a] -> [Int]
elemInd a l = aux a 0 l

aux :: Eq a => a -> Int -> [a] -> [Int]
aux _ _ [] = []
aux a x (h:t) | a==h = x:aux a (x+1) t
              | otherwise = aux a (x+1) t

func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

func' :: [[Int]] -> [Int]
func' [] = []
func' (h:t) | sum h >10 = h ++ func' t
            | otherwise = func' t

data RTree a = R a [RTree a] deriving Show
type Dictionary = [ RTree (Char, Maybe String) ] 

d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
          R ('r',Nothing) [
            R ('a',Just "...") [
              R ('s',Just "...") [] ],
            R ('o',Just "...") [],
            R ('r',Nothing) [
              R ('o',Just "...") [] ]
     ]] ] ]

insere :: String -> String -> Dictionary -> Dictionary
insere _ _ [] = []
insere palavra info (raiz@(R (c, desc) filhos) : restante)
    | head palavra == c =
        let novaRaiz = if null (tail palavra)
                            then R (c, Just info) filhos
                            else R (c, desc) (inserePalavra (tail palavra) info filhos)
        in novaRaiz : restante
    | otherwise = raiz : insere palavra info restante

inserePalavra :: String -> String -> [RTree (Char, Maybe String)] -> [RTree (Char, Maybe String)]
inserePalavra _ _ [] = []
inserePalavra palavra info (arvore : arvores)
    | null palavra = arvore : arvores
    | otherwise = insere (head palavra : "") info [arvore] ++ inserePalavra palavra info arvores

---------

split' :: Int -> [a] -> ([a],[a])
split' _ [] = ([],[])
split' 0 x = ([],x)
split' n (h:t) = (h:a,b)
               where (a,b) = split' (n-1) t

parte :: (Ord a) => a -> [a] -> ([a],[a])
parte _ [] = ([],[])
parte x (y:ys) | y < x = (y:as, bs)
               | otherwise = (as, y:bs)
          where (as,bs) = parte x ys

stringToInt :: String -> Int
stringToInt (h:t) = aux (digitToInt h) t 
     where aux :: Int -> String -> Int
           aux x [] = x
           aux x (s:t) = aux (x*10 + digitToInt s) t

produtoCart :: [a] -> [b] -> [(a,b)]
produtoCart l s = [(x,y) | x <- l, y <- s]

primos n = [x | x <- [1..n], divisores x == [1,x]]
        where divisores x = [y | y <- [1..x], mod x y == 0 ]

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

btIntort :: BTree a -> RTree a
btIntort Empty = R undefined []
btIntort (Node n Empty Empty) = R n [] 
btIntort (Node n e d) = R n [btIntort e, btIntort d]

data Time = AM Int Int | PM Int Int | Total Int Int

horavalida :: Time -> Bool
horavalida (Total h m) = (h>=0 && h<=23) && (m>=0 && m<=59)
horavalida (AM h m) = (h>=0 && h<=11) && (m>=0 && m<=59)
horavalida (PM h m) = (h>=0 && h<=11) && (m>=0 && m<=59)

minutos :: Time -> Int
minutos (AM h m) = h*60 + m
minutos (PM h m) = ((12+h)*60) + m
minutos (Total h m) = h*60 + m

instance Eq Time where
  (==) a b = minutos a == minutos b
  (/=) a b = minutos a /= minutos b

instance Ord Time where
  compare a b = compare (minutos a) (minutos b)

instance Show Time where
  show t@(AM h m) = if horavalida t then "AM " ++ show h ++ ":" ++ show m else "_ : _"
  show t@(PM h m) = if horavalida t then "PM " ++ show h ++ ":" ++ show m else "_ : _"
  show t@(Total h m) = if horavalida t then show h ++ ":" ++ show m else "_ : _"

instance Enum Time where
  toEnum n = let (h,m) = divMod n 60
             in Total h m
  fromEnum = minutos

dialogo :: String -> IO String
dialogo s = do putStr s
               r <- getLine
               return r

questionario :: [String] -> IO [String]
questionario [] = return []
questionario (q:qs) = do r <- dialogo q
                         rs <- questionario qs
                         return (r:rs)