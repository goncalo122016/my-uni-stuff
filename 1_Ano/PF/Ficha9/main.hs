module Main where
import System.Random

dialogo :: String -> IO String
dialogo s = do putStr s
               r <- getLine
               return r

adivinha :: IO (String)
adivinha = do n <- dialogo "Introduza um valor de n:"
              let i = read n
              v <- randomRIO (1,i)
              joga v

joga :: Int -> IO (String)
joga i = do m <- dialogo "Introduza um número:"
            let n = read m
            if i == n then return ("GANHOU!!!")
            else do putStrLn (feedback i n)
                    joga i

feedback i v | v>i = "O Número introduzido é Menor!" 
             | otherwise = "O Número introduzido é Maior!" 

main = return ()