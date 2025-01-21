module Aula6 where

--ÁRVORES BINÁRIAS

data Btree a = Empty
            | Node a (Btree a) (Btree a )
            deriving Show
arv :: Btree Int
arv = Node 3                                      -- 3
      (Node 4                               --4            5
              Empty                   --  _     7        _   _
              (Node 7 Empty Empty))         --_  _
      (Node 5 Empty Empty)
arv1 :: Btree String
arv1 = Node "Ana"                                
      (Node "To"                           
              Empty                 
              (Node "Bruno" Empty Empty))
      (Node "Rui" Empty Empty)

contaNode :: Btree a -> Int
contaNode Empty = 0
contaNode (Node i l r) = 1 + contaNode l + contaNode r

somNode :: Num a => Btree a -> a
somNode Empty = 0
somNode (Node x l r) = x + somNode l + somNode r

alturaBtree :: Btree a -> Int
alturaBtree Empty = 0
alturaBtree (Node _ l r) | alturaBtree l > alturaBtree r = 1 + alturaBtree l
                         | otherwise = 1+ alturaBtree r

alturaBtree' :: Btree a -> Int
alturaBtree' Empty = 0
alturaBtree' (Node _ l r) = 1 + max (alturaBtree l) (alturaBtree r)

prefixa :: Btree a -> [a]
prefixa Empty = []
prefixa (Node v l r) = [v] ++ prefixa l ++ prefixa r

infixa :: Btree a -> [a]
infixa Empty = []
infixa (Node v l r) = infixa l ++ [v] ++ infixa r

posfixa :: Btree a -> [a]
posfixa Empty = []
posfixa (Node v l r) = posfixa l  ++ posfixa r ++ [v]

mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree _ Empty = Empty
mapBtree f (Node i l r) = Node (f i) (mapBtree f l) (mapBtree f r)

zipBtree :: Btree a -> Btree b -> Btree (a,b)
zipBtree (Node i e d) (Node i1 e1 d1) = Node (i,i1) (zipBtree e e1) (zipBtree d d1)
zipBtree _ _ = Empty

balanceamento :: Btree a -> Btree a
balanceamento t = constroi (infixa t)

constroi :: [a] -> Btree a
constroi [] = Empty
constroi l = let c = length l
                 (le,ld) = splitAt (div c 2) l
             in Node (head ld) (constroi le) (constroi (tail ld))

-- Implementação de um MAP
lookupBT :: Ord a => a -> Btree (a,b) -> Maybe b
lookupBT _ Empty = Nothing
lookupBT k (Node (n,v) e d) | k==n = Just v
                            | k<n = lookupBT k e
                            | k>n = lookupBT k d

data RTree a = R a [RTree a] 
               deriving Show

bt :: Btree Int
bt = Node 5 Empty (Node 4 Empty Empty)

empty :: RTree String
empty = R "Empty" []

node :: Show a => a -> RTree String -> RTree String -> RTree String 
node i l r = R "Node" [R (show i) [],l,r]

data TTree a = Leaf a | Fork (TTree a) (TTree a) (TTree a) deriving Show

leaf i = R "Leaf" [R (show i) []]

fork t1 t2 t3 = R "Fork" [t1,t2,t3]

contanodeRT :: RTree a -> Int
contanodeRT (R _ l) = 1 + sum (map contanodeRT l)

alturaRT :: RTree a -> Int
alturaRT (R _ []) = 1
alturaRT (R _ l) = 1 +  maximum (map alturaRT l)

elemRT :: Eq a => a -> RTree a -> Bool
elemRT x (R a l) | x==a = True
                 | otherwise = or (map (elemRT x) l)

preorder :: RTree a -> [a]
preorder (R i l) = i:concatMap preorder l