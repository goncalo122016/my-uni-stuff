module Ficha8 where
import Data.Char
import Data.List


mdc :: Int -> Int -> Int
--mdc 42 18 = 6
mdc x y | x <= y = last [z | z <- divisores x, z `elem` divisores y]
        | otherwise = mdc y x

divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], mod x y == 0]

mdc' x y = head [z | z <- reverse [1..min x y],mod x z == 0, mod y z == 0]

-- Euclides : mdc x y = mdc (x+y) y 

mdc'' :: Int -> Int -> Int
mdc'' 0 x = x
mdc'' x 0 = x
mdc'' x y | x==y = x
          | otherwise = mdc (max x y - min x y) (min x y)

mdc''' :: Int -> Int -> Int
mdc''' 0 x = x
mdc''' x 0 = x
mdc''' x y | x<y = mdc''' (mod x y) y
           | otherwise = mdc''' x (mod y x)

--data Frac' = F Int Int

num' :: Frac -> Int
num' (F x _) = x

den' :: Frac -> Int
den' (F _ y) = y 

data Frac = F {num, den :: Int}

normaliza :: Frac -> Frac
-- normaliza (F 42 18) = F 7 3
normaliza (F n d) | n>=0 && d>0 = let x = mdc''' n d
                                 in F (div n x) (div d x)
                  | n<0 && d>0 = let y = mdc''' (-1*n) d
                                 in F (div n y) (div d y)
                  | otherwise = normaliza (F (-1*n) (-1*d))

-- signum 4 = 1    signum (-4) = -1     gcd = mdc
normaliza' :: Frac -> Frac
normaliza' (F n d) = let sinal = signum (n*d)
                         n' = abs n
                         d' = abs d
                         m = gcd n' d'
                     in F (sinal * (div n' m)) (div d' m)

instance Eq Frac where
   --(==) :: Frac -> Frac -> Bool
   (==) (F a b) (F x y) = b*x == a*y

instance Show Frac where
   --show :: Frac -> Frac -> Bool
   show (F x y) = show x ++ "/" ++ show y

-- class (Eq a, Show a) => Num a where
--     (+),(*),(-) :: a -> a -> a
--     negate, abs, signum :: a -> a
--     fromInteger :: Integer -> a

instance Num Frac where
    (+) (F x y) (F a b) = normaliza' (F (x*b + y*a) (y*b))
    (-) (F x y) (F a b) = normaliza' (F (x*b - y*a) (y*b))
    (*) (F x y) (F a b) = normaliza' (F (x*a) (y*b))
    negate (F x y) = F (-1*x) y
    abs (F x y) = F (abs x) (abs y)
    signum (F x y) = F (signum (x*y)) 1
    fromInteger i = F (fromInteger i) 1

instance Ord Frac where
   F x y >= F a b = x*b >= a*y
   F x y <= F a b = x*b <= a*y

condicao :: Frac -> [Frac] -> [Frac]
condicao f l = filter (>2*f) l


data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]
   --(==) :: Frac -> Frac -> Bool

instance Eq Data where
   --(==) :: Data -> Data -> Bool
   (==) (D ano mes dia) (D anO meS diA) = ano == anO && mes == meS && dia == diA

instance Ord Data where
   D ano mes dia >= D anO meS diA = ano>=anO || (ano == anO && mes>=meS) || (ano == anO && mes==meS && dia>=diA)
   D ano mes dia <= D anO meS diA = ano<=anO || (ano == anO && mes<=meS) || (ano == anO && mes==meS && dia<=diA)

instance Show Data where
    show (D dia mes ano) = intercalate "/" $ map show [dia,mes,ano]

ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

instance Show Extracto where
   show (Ext s l) = "Saldo anterior: " ++ show s ++
                    "\n---------------------------------------" ++
                    "\n  Data   Descricao   Credito   Debito  " ++
                    "\n---------------------------------------\n" ++ concatMap (\(dat,str,_) -> show dat ++ replicate (11 - length (show dat)) ' ' ++ map toUpper str ++ "    \n") l ++
                    "\n---------------------------------------" ++
                    "\nSaldo actual: " ++ show (saldo (Ext s l))


saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm

