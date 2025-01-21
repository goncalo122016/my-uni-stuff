module Ficha6 where

data Btree a = Empty
            | Node a (Btree a) (Btree a )
            deriving (Show,Eq)

a1 :: Btree Int
a1 = Node 40 (Node 50 Empty Empty)
             (Node 60 (Node 70 Empty Empty)
                      (Node 80 Empty Empty))
a2 :: Btree Int 
a2 = Node 30 (Node 90 Empty Empty)
             (Node 4 (Node 95 Empty Empty) Empty)

a3 :: Btree Int --Árvore de Procura
a3 = Node 70 (Node 60 (Node 20 (Node 5 Empty Empty) 
                               (Node 40 (Node 30 Empty Empty)
                                        (Node 50 Empty Empty))) 
                       Empty)
             (Node 90 (Node 80 Empty Empty)
                      (Node 110 (Node 100 Empty Empty) Empty))

nfolhas :: Btree a -> Int
nfolhas Empty = 0
nfolhas (Node i l r) = nfolhas l + nfolhas r

prune :: Int -> Btree a -> Btree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node i l r) = Node i (prune (n-1) l) (prune (n-1) r)

isTrace :: Eq a => Btree a -> [a] -> Bool
isTrace Empty [] = True
isTrace Empty _ = False
isTrace (Node i l r) (h:t) | i==h = isTrace l t || isTrace r t
                           | otherwise = False

traces :: Btree a -> [[a]]
traces Empty = [[]]
traces (Node i Empty Empty) = [[i]]
traces (Node i l r) = map (i:) (traces l ++ traces r)

untraces :: Eq a => [[a]] -> Btree a 
untraces [] = Empty
untraces (p:ps) = foldl (flip insPath) (createNode p) ps

insPath :: Eq a => [a] -> Btree a -> Btree a
insPath [] b = b
insPath l Empty = createNode l
insPath p@(h:t) (Node i l r) | h==i = Node i (insPath t l) r
                           | otherwise = Node i l (insPath p r)

createNode :: [a] -> Btree a
createNode [] = Empty
createNode [x] = Node x Empty Empty
createNode (h:t) = Node h (createNode t) Empty


isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (h:s:t) = (h<=s) || isSorted (s:t)

elemB :: Ord a => a -> Btree a -> Bool -- Recebe uma árvore de Procura
elemB _ Empty = False
elemB x (Node i l r) | x==i = True 
                     | x<i = elemB x l
                     | x>i = elemB x r

add :: Ord a => a -> Btree a -> Btree a -- Recebe uma árvore de Procura
add x Empty = Node x Empty Empty
add x (Node i l r) | x<i = Node i (add x l) r
                   | otherwise = Node i l (add x r)
