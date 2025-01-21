module Aula5 where

procura :: Eq a => a -> [a] -> Int
procura x s = case lookup x (zip s [0..]) of
      Nothing -> -1
      Just i -> i

subListas :: Int -> [a] -> [[a]]
subListas _ [] = []
subListas x l@(h:t) | length l <= x = [l]
                    | otherwise = [take x l] ++ subListas x (drop x l)

sub :: Int -> [a] -> [[a]]
sub _ [] = []
sub x l@(h:t) | length l <= x = [l]
              | otherwise = [fst(splitAt x l)] ++ sub x (snd (splitAt x l))
