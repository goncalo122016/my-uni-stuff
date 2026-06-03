module Aula10 where

data ArvExp = Const Int
            | Op Operador ArvExp ArvExp

type Operador = Char -- '+','-', ...

e :: ArvExp
e = (Op '+' (Const 2) (Op '*' (Const 4) (Const 5))) 

calcula :: ArvExp -> Int
calcula (Const a) = a
calcula (Op '+' e1 e2) = calcula e1 + calcula e2
calcula (Op '-' e1 e2) = calcula e1 - calcula e2
calcula (Op '*' e1 e2) = calcula e1 * calcula e2
calcula (Op '/' e1 e2) = calcula e1 `div` calcula e2

showExp :: ArvExp -> String
showExp (Const a) = show a
showExp (Op op e1 e2) = "(" ++ showExp e1 ++ [' ', op, ' '] ++ showExp e2 ++ ")"

instance Show ArvExp where
    show e = showExp e ++ " = " ++ show (calcula e)

arvores :: String -> [ArvExp]
arvores s | not (any isOp s) = [Const (read s)] --caso em que só temos uma constante na String
          | otherwise = [ Op op e1 e2 | (s1,op,s2) <- parte s, 
                                                e1 <- arvores s1, 
                                                e2 <- arvores s2]

parte :: String -> [(String,Char,String)]
-- break isOp "2 + 3" = ("2 ","+ 3") 
parte [] = []
parte s = case s2 of 
            [] -> []
            (op:t) -> (s1, op, t): [(s1 ++ [op] ++ x, o, y) | (x,o,y) <- parte t]
    where (s1,s2) = break isOp s

isOp :: Char -> Bool
isOp c = c `elem` "+-*/" 

haSolucao :: Int -> String -> Maybe ArvExp
haSolucao x s = let arvsExp = arvores s 
                    resultados = map calcula arvsExp
                    arvRes = zip resultados arvsExp
                in lookup x arvRes