module Questoes50 where
import Data.Char
--1
enumFromto :: Int -> Int -> [Int]
enumFromto x y = if x>y then [] else x:enumFromto (x+1) y

enumFromto' x y | x>y = []
                | otherwise = x:enumFromto' (x+1) y
--2
enumFromThento :: Int -> Int -> Int -> [Int]
enumFromThento x y z = if x>=z && y>x || x<=z && y<x then []
                       else x:enumFromThento y (2*y-x) z

enumFromThento' x y z | x>=z && y>x || x<=z && y<x = []
                      | otherwise = x:enumFromThento' y (2*y-x) z
--3
conc :: [a] -> [a] -> [a]
conc [] x = x
conc (a:b) x = a:conc b x
--4
escol :: [a] -> Int -> a
escol (h:t) 0 = h
escol (h:t) n = escol t (n-1)
--5
rev :: [a] -> [a]
rev [] = []
rev (h:t) = conc (rev t) [h]

rev' :: [a] -> [a]
rev' [] = []
rev' (h:t) = rev t ++ [h]
--6
tirar :: Int -> [a] -> [a]
tirar 0 _ = []
tirar _ [] = []
tirar n (h:t) = if length (h:t) < n then (h:t)
            else [h] ++ tirar (n-1) t

tirar' n _ | n<=0 = []
tirar' _ [] = []
tirar' n (h:t) = h:tirar' (n-1) t
--7
cair :: Int -> [a] -> [a]
cair 0 x = x
cair _ [] = []
cair n (h:t) = if n == length (h:t) then []
               else cair (n-1) t

cair' n x | n<=0 = x
cair' _ [] = []
cair' n (h:t) = cair' (n-1) t
--8
zipar :: [a] -> [b] -> [(a,b)]
zipar _ [] = []
zipar [] _ = []
zipar (h:t) (h1:t1) = (h,h1):zipar t t1
--9
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n x = if n<0 then [] else x:replicar (n - 1) x

replicar' n _ | n<=0 = []
replicar' n x = x:replicar' (n-1) x
--10
intercalar :: a -> [a] -> [a]
intercalar _ [] = []
intercalar _ [h] = [h]
intercalar x (h:t)= h:x:intercalar x t
--11
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar [x] = [[x]]
agrupar (h:t) | h == head t = (h:ht):tt
              | otherwise = [h]:ht:tt
               where (ht:tt) = agrupar t

group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = insere h (group t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)

--12
conca :: [[a]] -> [a]
conca [] = []
conca [x] = x
conca (h:t) = h ++ conca t
--13
prefx :: [a] -> [[a]]
prefx [] = [[]]
prefx l = prefx (init l) ++ [l]
--14
sufx :: [a] -> [[a]]
sufx [] = [[]]
sufx l@(h:t) = [l] ++ sufx t
--15
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = head h : heads t
--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t
--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a, c):fun t
--18
cola :: [(String,b,c)] -> String
cola [] = []
cola ((a,_,_):t) = a ++ cola t
--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((n,an):t) | a-an >= i = n:idade a i t
                     | otherwise = idade a i t
--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m>1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                  | otherwise = []
--21
isPrime :: Int -> Bool
isPrime n | n>=2 = checkPrime n 2
          | otherwise = False
                where checkPrime :: Int -> Int -> Bool
                      checkPrime n m
                            | m*m > n = True
                            | mod n m == 0 = False
                            | otherwise = checkPrime n (m+1)
--22
isPrefxOf :: Eq a => [a] -> [a] -> Bool
isPrefxOf [] _ = True
isPrefxOf _ [] = False
isPrefxOf s l@(h:t) = head s == h && isPrefxOf (tail s) t
--23
isSufxOf :: Eq a => [a] -> [a] -> Bool
isSufxOf [] _ = True
isSufxOf _ [] = False
isSufxOf s@(h:t) l@(h':t') = s == l || isSufxOf s t'

isSufxOf' :: Eq a => [a] -> [a] -> Bool
isSufxOf' s l = isPrefxOf (reverse s) (reverse l)
--24
isSubOf :: Eq a => [a] -> [a] -> Bool
isSubOf [] _ = True
isSubOf _ [] = False
isSubOf l@(y:t) (x:t1) | y==x = isSubOf t t1
                       | otherwise = isSubOf l t1
--25
elemInd :: Eq a => a -> [a] -> [Int]
elemInd x [] = []
elemInd x l = elemIndAux x l 0

elemIndAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndAux _ [] _ = []
elemIndAux x (h:t) i | x == h = i : elemIndAux x t (i+1)
                     | otherwise = elemIndAux x t (i+1)
--26
nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) | elem h t = nub t
          | otherwise = h:nub t
--27
delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t) | h==x = t
               | otherwise = h:delete x t
--28
removeDe :: Eq a => [a] -> [a] -> [a]
removeDe [] _ = []
removeDe l [] = l
removeDe l (hs:ts) = removeDe (delete hs l) ts
--29
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union [] _ = []
union l s@(h:t) | notElem h l =  union (l ++ [h]) t
                | otherwise = union l t
--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect l s@(h:t) | elem h l = h:intersect l t
                    | otherwise = intersect l t
--31
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x l@(h:t) | x>=h = h:insert x t
                 | otherwise = x:l
--32
unword :: [String] -> String
unword [] = ""
unword [x] = x
unword (h:t) = h ++ " " ++ unword t
--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "/n" ++ unlines' t
--34
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) | h >= (t !! pMaior t) = 0
             | otherwise = 1 + pMaior t

pMaior' :: Ord a => [a] -> Int
pMaior' (h:t) = pMaiorAc 0 1 h t

pMaiorAc :: Ord a => Int -> Int -> a -> [a]  -> Int
pMaiorAc i _ _ [] = i
pMaiorAc i x n (h:t) | n>=h = pMaiorAc i (x+1) n t
                     | otherwise = pMaiorAc x (x+1) h t
                     
--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):t) | x==a = Just b
                    | otherwise = lookup' x t
--36
preCres :: Ord a => [a] -> [a]
preCres [] = []
preCres [x] = [x]
preCres (h:s:t) | h<=s = h:preCres (s:t)
                | otherwise = [h]
--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (h:t) = qSort peq ++ [h] ++ qSort gra
              where
                peq = [a | a <- t, a<h]
                gra = [b | b <- t, b>h]

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort met1) (mergeSort met2)
               where (met1,met2) = splitAt (div (length l) 2) l

merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (a:b) (c:d) | a < c = a:merge b (c:d) 
                  | otherwise = c:merge (a:b) d
--38
mDic :: String -> String -> Bool
mDic "" _ = True
mDic _ "" = False
mDic (p:p1) (l:l1) | ord p < ord l = True
                   | ord p==ord l = mDic p1 l1
                   | otherwise = False
mDic' "" _ = True
mDic' _ "" = False
mDic' (p:p1) (l:l1) | p < l = True
                    | p==l = mDic' p1 l1
                    | otherwise = False
--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,_):t) | a == x = True
                     | otherwise = elemMSet a t
--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,n):t) | n==0 = converteMSet t
                       | otherwise = a:converteMSet ((a,n-1):t)
--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet c [] = [(c,1)]
insereMSet c ((x,n):t) | x==c = (x,n+1):t
                       | otherwise = (x,n):insereMSet c t
--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet c [] = []
removeMSet c ((x,n):t) | x==c = if n==1 then removeMSet c t else (x,n-1):t
                       | otherwise = (x,n):removeMSet c t
--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = insereMSet (last l) (constroiMSet (init l))

constroiMSet' [] = []
constroiMSet' (h:t) = constroiMSetAux t [h]

constroiMSetAux [] x = [(head x, length x)]
constroiMSetAux (h:t) x
    | h `elem` x = constroiMSetAux t (h:x)
    | otherwise = (head x, length x) : constroiMSetAux t [h]
--44
partEithers :: [Either a b] -> ([a],[b])
partEithers [] = ([],[])
partEithers l = (lefts l,rights l)
              where lefts x = [a | Left a <- x]
                    rights x = [a | Right a <- x]

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a : ls,rs)
    where (ls,rs) = partitionEithers' t
partitionEithers' ((Right b):t) = (ls,b : rs)
    where (ls,rs) = partitionEithers' t
--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:t) = catMaybes t
catMaybes ((Just x):t) = x:catMaybes t
--46
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi>xf = Oeste : caminho (xi,yi) (xf+1,yf)
                        | xi<xf = Este:caminho (xi+1,yi) (xf,yf)
                        | yi>yf = Sul:caminho (xi,yi) (xf,yf+1)
                        | yi<yf = Norte:caminho (xi,yi+1) (xf,yf)
                        | otherwise = []
--47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops p m = p == posicaoAux p m || hasLoops p (init m)

posicaoAux :: (Int,Int) -> [Movimento] -> (Int,Int)
posicaoAux (x,y) [] = (x,y)
posicaoAux (x,y) (Norte:t) = posicaoAux (x,y+1) t
posicaoAux (x,y) (Sul:t) = posicaoAux (x,y-1) t
posicaoAux (x,y) (Este:t) = posicaoAux (x+1,y) t
posicaoAux (x,y) (Oeste:t) = posicaoAux (x-1,y) t
--48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (x1,y1)):t) | abs (x1-x) == abs (y1-y) = 1+contaQuadrados t
                                        | otherwise = contaQuadrados t
--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (h:t) = areaRect h + areaTotal t

areaRect :: Rectangulo -> Float
areaRect (Rect (x1,y1) (x2,y2)) = abs (x2-x1) * abs (y2-y1)
--50
data Equipamento = Bom | Razoavel | Avariado
                   deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1+naoReparar t
naoReparar (Razoavel:t) = 1+naoReparar t
naoReparar (Avariado:t) = naoReparar t