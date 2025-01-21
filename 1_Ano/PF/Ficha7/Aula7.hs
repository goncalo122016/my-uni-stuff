module Aula7 where

data Exp = Add Exp Exp
         | Mult Exp Exp
         | Const Int 
         --deriving Show

e :: Exp
e = Add (Const 3) (Mult (Const 4) (Const 5))

showExp :: Exp -> String
showExp (Const x) = show x
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Mult e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"

instance Show Exp where
      show = showExp