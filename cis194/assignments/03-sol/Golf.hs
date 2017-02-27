module Golf where

import Data.List

skips :: [a] -> [[a]]
skips []= []
skips xs = [ (skip x xs) | x <- [1,2..(length xs)]]

skip :: Int -> [a] -> [a]
skip n [] = []
skip n ls@(x:xs) = x:(skip n (drop n ls))

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:xs) = 
	if (x>y) 
		then (x:p) 
		else if(y>x) 
			then localMaxima (y:xs) 
			else p
	where p = notMax (y:xs) 
localMaxima l = l

notMax :: [Integer]-> [Integer]
notMax (x:y:xs) = 
	if(y>x) 
		then localMaxima (y:xs) 
		else notMax (y:xs)
notMax _ = []

histogram :: [Integer] -> String
histogram l = unlines ((getBase l) ++ [(replicate 10 '=')] ++ [['0'..'9']])

getBase :: [Integer] -> [String]
getBase l = getHist (map (count l) [0..9])

count :: [Integer] -> Integer -> Integer
count [] _= 0
count (x:xs) y = if(x==y) then 1 + p else p
	where p = (count xs y)

getHist :: [Integer] -> [String]
getHist l= if(p == 0) then [] else [getIthRow l x |x <- [p..1]]
	where p = maximum l

getIthRow :: [Integer] -> Integer -> String
getIthRow [] _ = []
getIthRow (x:xs) m = if(x>=m) then "*" ++ (getIthRow xs m) else " " ++ (getIthRow xs m)