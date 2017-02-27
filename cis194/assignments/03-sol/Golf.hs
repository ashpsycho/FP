module Golf where

r=length

skips q = [ s x (drop (x-1) q) | x <- [1..r q]]

s n [] = []
s n l@(x:q) = x:(s n (drop n l))

localMaxima = k
k (x:l@(y:z:q))
  | (y>x && y>z) = y:p
  | True = p
  where p = k l
k _ = []

histogram l = unlines (h (map (c l) [0..9]) ++ "==========":[['0'..'9']])

c l y = r (filter (\x -> (x==y)) l)

h l = reverse [map (f x) l |x <- [1..maximum l]]

f m x
  | (m<=x) = '*'
  | True = ' '