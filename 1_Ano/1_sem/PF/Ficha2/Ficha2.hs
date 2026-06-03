module Ficha2 where
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) | x == h = numOcorre x t + 1
                  | otherwise = numOcorre x t 

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h<0 = h + somaNeg t 
              | otherwise = somaNeg t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b:segundos t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+sa,b+sb,c+sc)
                       where (sa,sb,sc) = sumTriplos t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta x [] = 0
conta x ((_,n):t) | x==n = 1+conta x t
                  | otherwise = conta x t

grau :: Polinomio -> Int
grau [(_,n)] = n
grau ((a,b):t) | b>grau t = b
               | otherwise = grau t

grau' :: Polinomio -> Int
grau' p = maximum g
    where g = snd (unzip p)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n []= []
selgrau n ((a,b):t) | n==b = (a,b):selgrau n t
                    | otherwise = selgrau n t

deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((c,0):t) = deriv t
deriv ((c,e):t) = (c*(fromIntegral e),e-1):deriv t

calcula :: Float -> Polinomio -> Float 
calcula x [] = 0
calcula x ((a,b):t) = a*(x^b) + calcula x t 

simp :: Polinomio -> Polinomio
simp [] = [] 
simp ((a,b):t) | a == 0 = (a,b):simp t
               | otherwise = simp t

mult :: Monomio -> Polinomio -> Polinomio
mult (0,0) _ = []
mult _ [] = []
mult (x,y) ((a,b):t) = (x*a,y+b): mult (x,y) t

normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a,b):(x,y):t) | b==y = normaliza ((a+x,b):t)
                          | conta b t == 0 = (a,b) : normaliza ((x,y) : t)
                          | otherwise = normaliza ((a,b):t++[(x,y)])