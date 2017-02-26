type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a,b)]
hanoi n a b c = concat[(hanoi (n-1) a c b),[(a,b)],(hanoi (n-1) c b a)]

hanoiLength :: Integer -> Int
hanoiLength n = length (hanoi n "a" "b" "c")

hanoiFourTry :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFourTry 0 _ _ _ _ _ = []
hanoiFourTry 1 _ a b _ _ = [(a,b)]
hanoiFourTry n m a b c d = concat [hanoiFour (n-m) a d b c, hanoi m a b c, hanoiFour (n-m) d b a c ]

getSmallestList :: [[t]] -> [t]
getSmallestList [] = []
getSmallestList [x] = x
getSmallestList (x:xs) = if( length x < length y) then x else y where y=getSmallestList xs

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 _ _ _ _ = []
hanoiFour 1 a b _ _ = [(a,b)]
hanoiFour n a b c d = getSmallestList [(hanoiFourTry n x a b c d) | x <- [1,2.. (1+n`div`2)] ]

hanoiFourLength :: Integer -> Int
hanoiFourLength n = length (hanoiFour n "a" "b" "c" "d")