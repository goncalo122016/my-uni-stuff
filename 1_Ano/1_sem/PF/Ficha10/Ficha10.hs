module Ficha10 where
import System.Random
import Data.List

--getCHar :: IO Char 
--putChar :: Char -> IO ()
--getLine :: IO String
--putStr :: String -> IO ()
--randomIO :: IO a
--randomRIO :: (a,a) -> IO a

prog :: IO ()
prog = do n <- getLine
          putStrLn (reverse n)

escolhe :: Eq a => [a] -> IO a
escolhe l = do n <- randomRIO (0, length l - 1)
               return (l !! n)

bingo :: IO ()
bingo = bingoAux [1..90]

bingoAux :: [Int] -> IO ()
bingoAux [] = return ()
bingoAux l = do getLine
                x <- escolhe l
                print x
                bingoAux (delete x l)

{-mastermind :: IO ()
mastermind = do d1 <- randomRIO (0,9)
                d2 <- randomRIO (0,9)
                d3 <- randomRIO (0,9)
                d4 <- randomRIO (0,9)
                let seg = [d1,d2,d3,d4]
                x <- getLine 
                putStrLn ("Número de dígitos com o valor correcto na posiçãoo correcta: " ++ show (valorposicaocorreto (sequence x) seg))
                putStrLn ("Número de dígitos com o valor correcto na posição errada: " ++ show (valorposicaocorreto (sequence x) seg))-}

digitoscorretos :: String -> String -> Int
digitoscorretos [] l = 0
digitoscorretos seg ten = length (intersect seg ten) - a
                       where a = valorposicaocorreto seg ten
                       
valorposicaocorreto :: String -> String -> Int
valorposicaocorreto [] [] = 0
valorposicaocorreto (h:t) (h':t') | h == h' = 1 + valorposicaocorreto t t'
                                  | otherwise = valorposicaocorreto t t'

--CORREÇÃO

mastermind' :: IO ()
mastermind' = do d1 <- randomRIO ('0','9')
                 d2 <- randomRIO ('0','9')
                 d3 <- randomRIO ('0','9')
                 d4 <- randomRIO ('0','9')
                 let seg = [d1,d2,d3,d4]
                 mm seg

mm :: String -> IO ()
mm s = do putStr "Introduza a Tentativa: "
          t <- getLine
          let (x,y) = avalia t s
          if x==4 then putStr ("Parabéns!!!") 
          else do putStrLn ("Número de dígitos na posição correcta: " ++ show x)
                  putStrLn ("Número de dígitos na posição errada: " ++ show y)
                  mm s

avalia :: String -> String -> (Int,Int) 
avalia l s = (valorposicaocorreto l s, digitoscorretos l s)

splits :: [a] -> [([a],a,[a])]
splits xs = [(take i xs, xs !! i, tail (drop i xs)) | i <- [0..length xs - 1]]