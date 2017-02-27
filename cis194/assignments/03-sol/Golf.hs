module Golf where

import Data.List

skips []= []
skips q = [ (s x q) | x <- [1..(length q)]]

t n [] = []
t n (x:q) = x:(s n q)

s n [] = []
s n l@(x:q) = t n (drop (n-1) l)

localMaxima (x:l@(y:z:q)) 
  | (y>x && y>z) = y:p 
  | True = p
  where p = localMaxima l 
localMaxima _ = []

histogram l = unlines ((h (map (c l) [0..9])) ++ "==========":[['0'..'9']])

c (x:q) y 
  |(x==y) = (1 + p)
  | True = p
  where p = c q y
c _ _= 0

h l 
  | (p == 0) = [] 
  | True = reverse [w l x |x <- [1..p]]
    where p = maximum l

w (x:q) m 
  | (x>=m) = '*':p 
  | True = ' ':p
  where p=w q m
w _ _ = []