module Questoes501 where

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x>y = []
                | otherwise = x:enumFromTo' (x+1) y

enumFromThenTo' :: Int -> Int -> Int -> [Int] 
enumFromThenTo' x z y | x>=y || z<=x = []
                      | otherwise = x:enumFromThenTo' z (2*z - x) y

junta :: [a] -> [a] -> [a]
junta [] x = x
junta (h:t) l = h:junta t l

elemInd :: [a] -> Int -> a
elemInd (h:t) i |i==0 = h
                |otherwise = elemInd t (i-1)

inverte :: [a] -> [a]
inverte [] = []
inverte (h:t) = inverte t ++ [h]

tirar :: Int -> [a] -> [a] 
tirar _ [] = []
tirar n (h:t) | n<=0 = []
              | otherwise = h:tirar (n-1) t

cair :: Int -> [a] -> [a]
cair _ [] = []
cair x l@(h:t) | x<=0 = l
               | otherwise = cair (x-1) t

zipar :: [a] -> [b] -> [(a,b)]
zipar [] _ = []
zipar _ [] = []
zipar (h:t) (h1:t1) = (h,h1):zipar t t1

replicar :: Int -> a -> [a]
replicar x a | x<=0 = []
             | otherwise = a:replicar (x-1) a

intercalar :: a -> [a] -> [a] 
intercalar _ [] = []
intercalar _ [a] = [a]
intercalar x (h:t) = h:x:intercalar x t

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = [[]]
agrupar [x] = [[x]]                           
agrupar (h:t) | h == head t = (h:ht):tt
              | otherwise = [h]:agrupar t
               where (ht:tt) = agrupar t

concat' :: [[a]] -> [a]
concat' [] = []
concat' ([]:t) = concat' t
concat' (h:t) = h ++ concat' t

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits'(init l) ++ [l]

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l:tails'(tail l)

heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (h:t) = head h : heads' t

total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,_,c):t) = (a,c):fun t

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((n,_,_):t) = n ++ cola t

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano id ((n,a):t) | ano - a >= id = n: idade ano id t
                       | otherwise = idade ano id t

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m =  powerEnumFrom n (m-1) ++ [n^(m-1)]

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h1:t1) (h:t) | h1==h = isPrefixOf t1 t
                         | otherwise = False

isSuffixOf ::  Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf s@(h1:t1)  l@(h:t) | last s == last l = isSuffixOf (init s) (init l)
                              | otherwise = False

isSubOf :: Eq a => [a] -> [a] -> Bool
isSubOf [] _ = True
isSubOf _ [] = False
isSubOf l@(h1:t1) (h:t) | h1==h = isSubOf t1 t 
                        | otherwise = isSubOf l t

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndAux x l 0

elemIndAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndAux _ [] _ = []
elemIndAux x (h:t) i | x==h = i:elemIndAux x t (i+1)
                     | otherwise = elemIndAux x t (i+1)

semrep :: Eq a => [a] -> [a]
semrep [] = []
semrep (h:t) | h `elem` t = semrep t
             | otherwise = h:semrep t

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) | x==h = t
               | otherwise = h:delete x t

removeDe :: Eq a => [a] -> [a] -> [a]
removeDe [] _ = []
removeDe x [] = x
removeDe l (x:xs) = removeDe (delete x l) xs

union :: Eq a => [a] -> [a] -> [a]
union x [] = x
union l@(h:t) (x:xs) | x `notElem` l = union (l ++ [x]) xs
                     | otherwise = union l xs

intersect :: Eq a => [a] -> [a] -> [a] 
intersect [] _ = []
intersect (h:t) l | h `elem` l = h:intersect t l
                  | otherwise = intersect t l

insert :: Ord a => a -> [a] -> [a] 
insert x [] = [x]
insert x (h:t) | x<h = x:(h:t)
               | otherwise = h:insert x t

unwords' :: [String] -> String
unwords' [x] = x
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

pMaior :: Ord a => [a] -> Int
pMaior (h:t) = pMaiorAc 0 1 h t

pMaiorAc :: Ord a => Int -> Int -> a -> [a] -> Int
pMaiorAc i _ _ [] = i
pMaiorAc i x n (h:t) | h>=n = pMaiorAc x (x+1) h t
                     | otherwise = pMaiorAc i (x+1) n t
            
lookup' :: Eq a => a -> [(a,b)] -> Maybe b 
lookup' _ [] = Nothing
lookup' x ((a,b):t) | x==a = Just b
                    | otherwise = lookup' x t

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t) | h>=s = [h]
                     | otherwise = h:preCrescente (s:t)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (h:t) (x:xs) | h==x = menor t xs
                   | h<x = True
                   | otherwise = False

elemMSet :: Eq a => a -> [(a,Int)] -> Bool 
elemMSet _ [] = False
elemMSet x ((a,_):t) | x==a = True
                     | otherwise = elemMSet x t

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,n):t) | n==1 = a:converteMSet t
                       | otherwise = a:converteMSet ((a,n-1):t)

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)] 
insereMSet x ((a,b):t) | x==a = (a,b+1):t
                       | otherwise = (a,b): insereMSet x t

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = [] 
removeMSet x ((a,b):t) | x==a = if b==1 then t else (a,b-1):t
                       | otherwise = (a,b): removeMSet x t

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = insereMSet (last l) (constroiMSet (init l))

partEithers :: [Either a b] -> ([a],[b])
partEithers [] = ([],[])
partEithers l = (lefts l , rights l )
             where lefts l = [a | Left a <-  l]
                   rights l = [a | Right a <-  l]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes (Just a:t) = a:catMaybes t

data Movimento = Norte | Sul | Este | Oeste
                  deriving (Show,Eq)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi>xf = Oeste: caminho (xi-1,yi) (xf,yf)
                        | xi<xf = Este: caminho (xi+1,yi) (xf,yf)
                        | yi<yf = Norte: caminho (xi,yi+1) (xf,yf)
                        | yi>yf = Sul: caminho (xi,yi-1) (xf,yf)
                        | otherwise = []

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops p m = p == pfinal p m || hasLoops p (init m)

pfinal :: (Int,Int) -> [Movimento] -> (Int,Int)
pfinal p [] = p
pfinal (x,y) (h:t) | h == Norte = pfinal (x,y+1) t
                   | h == Sul = pfinal (x,y-1) t 
                   | h == Oeste = pfinal (x-1,y) t
                   | h == Este = pfinal (x+1,y) t

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQua :: [Rectangulo] -> Int
contaQua [] = 0
contaQua ((Rect (x1,y1) (x,y)):t) | abs (x1-x) == abs (y1-y) = 1 + contaQua t
                                  | otherwise = contaQua t

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x,y)):t) = abs (x1-x) * abs (y1-y) + areaTotal t

data Equipamento = Bom | Razoavel | Avariado
                   deriving (Show,Eq)

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) | h==Bom = 1 + naoReparar t
                 | h==Razoavel = 1 + naoReparar t
                 | h==Avariado = naoReparar t
