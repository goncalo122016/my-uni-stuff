module Aula9 where
import System.Random

dialogo :: String -> IO String
dialogo s = do putStr s
               r <- getLine
               return r

dialogo' :: String -> IO String
dialogo' s = (putStr s) >> (getLine >>= (\r -> return r))

questionario :: [String] -> IO [String]
questionario [] = return []
--questionario (q:qs) = do 

