module Teste1920 where
import Data.List

--1
interseta :: Eq a => [a] -> [a] -> [a]
interseta l [] = l
interseta [] _ = []
interseta (h:t) l | h `elem` l = h: interseta t l
                  | otherwise = interseta t l 

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l:tails' (tail l)

--2
type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []
elems l@((a,b):t) = [a..b] ++ elems t

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l@(h:t) = (h,d): geraconj (dropWhile (<=d) l)
    where d = foldl (\acc x -> if x == succ acc then x else acc) h t

--3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String  deriving (Show,Eq)
type Nome = String
type Agenda = [(Nome, [Contacto])]

agen = [("Ana", [Tlm 962121303, Trab 98546372]),("Rui", [Casa 913455261,Email "zeantonio@gmail.com"])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail _ _ [] = []
acrescEmail nom e ((n,l):t) | nom == n = (n,Email e:l):t
                            | otherwise = acrescEmail nom e t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nom ((n,l):t) | nom == n = Just (filterEmail l)
                        | otherwise = verEmails nom t

filterEmail :: [Contacto] -> [String]
filterEmail [] = []
filterEmail ((Email x):t) = x: filterEmail t
filterEmail (_:t) = filterEmail t

consulta :: [Contacto] -> ([Integer],[String])
consulta cont = (filterTel cont, filterEmail cont)

filterTel :: [Contacto] -> [Integer]
filterTel [] = []
filterTel (x:t) = case x of 
    Email email -> filterTel t
    Tlm n -> n:filterTel t
    Trab n -> n:filterTel t
    Casa n -> n:filterTel t

consultaIO :: Agenda -> IO () 
consultaIO ag = do putStr "Indique o nome que pretende consultar:"
                   n <- getLine
                   putStrLn (show (lookup n ag)) 

consultaIO' :: Agenda -> IO ()
consultaIO' agenda = do
    nome <- getLine
    let contactos = aux nome agenda
    putStr (concat [show x ++ "\n" | x <- contactos])
       where aux _ [] = []
             aux nome ((name,contactos):t) = if name == nome then contactos else aux nome t

--4
data RTree a = R a [RTree a] deriving (Show, Eq)

tree1 = R 1 [R 2 [],
             R 3 [R 4 [R 5 [],
                       R 6 []
                      ]
                 ],
             R 7 []
            ]

paths :: RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a l) = [a:x | x <- concatMap paths l]

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[x]] = R x []
unpaths ls = R (head $ head ls) [unpaths (foldl (\acc branch -> if head branch == y then branch:acc else acc) [] [tail x | x <- ls]) | y <- nub [head x | x <- [tail x | x <- ls]]]

onepath :: [a] -> RTree a
onepath l = R (head l) [onepath (tail l)]
