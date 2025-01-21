module Teste2021 where

import Data.List (sortBy)
import Data.Ord (comparing)

retira :: Eq a => [a] -> [a] -> [a]
retira [] _ = []
retira x [] = x
retira l (h:t) = retira (delete h l) t

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) | x==h = t
               | otherwise = h:delete x t

type MSet a = [(a,Int)]

set1 :: [(Char, Int)]
set1 = [('b',2), ('a',4), ('c',1)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet a [] = []
removeMSet a ((b,n):t) | a==b = if n==1 then t else (b,n-1):t
                       | otherwise = (b,n): removeMSet a t

calcula :: MSet a -> ([a],Int)
calcula [] = ([],0)
calcula ((a,n):t) = (a:as, n+ns)
        where (as,ns) = calcula t

{-partes :: String -> Char -> [String]
partes [p] x = [[p]]
partes p c = filter (==[c]) (partsHelper p [] "")
  where partsHelper :: String -> [String] -> String -> [String]
        partsHelper [] acc cur = acc ++ [cur]
        partsHelper (x:xs) acc cur
             | x == c = partsHelper xs (acc ++ [cur]) ""
             | otherwise = partsHelper xs acc (cur ++ [x])-}

partes :: String -> Char -> [String]
partes str delim = partsHelper str [] ""
  where
    partsHelper :: String -> [String] -> String -> [String]
    partsHelper [] acc cur = acc ++ [cur]
    partsHelper (x:xs) acc cur
      | x == delim = partsHelper xs (acc ++ [cur]) ""
      | otherwise = partsHelper xs acc (cur ++ [x])

data BTree a = Empty | Node a (BTree a) (BTree a)

a1 :: BTree Int
a1 = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove a (Node n e d) | a==n = Empty
                      | a<n = Node n (remove a e) d
                      | otherwise = Node n e (remove a d)

instance Show a => Show (BTree a) where
    show Empty = "*"
    show (Node n e d) = "(" ++ show e ++ " <-" ++ show n ++ "-> " ++ show d ++ ")"

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]], Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

fichs :: FileSystem -> [Nome]
fichs (File x) = [x]
fichs (Dir nm f) = concatMap fichs f

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles f [] = Nothing
dirFiles (Dir nm f) (h:t) | h==nm = case t of
                                       [] -> Just $ concatMap fichs f
                                       _ -> findFilesInDir f t
                          | otherwise = Nothing
dirFiles _ _ = Nothing

findFilesInDir :: [FileSystem] -> [Nome] -> Maybe [Nome]
findFilesInDir _ [] = Nothing
findFilesInDir [] _ = Nothing
findFilesInDir (f:fs) path' = case dirFiles f path' of
                                Just res -> Just res
                                Nothing -> findFilesInDir fs path'

listaFich :: FileSystem -> IO ()
listaFich fs = do putStrLn "Introduza uma path para os ficheiros que quer aceder no seguinte formato (ex.: xxx/yyy/dir)"
                  path <- getLine 
                  let cam = partes path '/'
                  case dirFiles fs cam of 
                      Nothing -> putStrLn "Não é uma path válida!"
                      Just x -> print x