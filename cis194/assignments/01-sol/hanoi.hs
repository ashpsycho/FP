type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a,b)]
hanoi n a b c = concat[(hanoi (n-1) a c b),[(a,b)],(hanoi (n-1) c b a)]

hanoiLength :: Integer -> Int
hanoiLength n = length (hanoi n "a" "b" "c")

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 0 _ _ _ _ = []
hanoiFour 1 a b _ _ = [(a,b)]
hanoiFour n a b c d = concat[ (hanoiFour (n-2) a d b c) ,[(a,c),(a,b),(c,b)], (hanoiFour (n-2) d b a c) ]

hanoiFourLength :: Integer -> Int
hanoiFourLength n = length (hanoiFour n "a" "b" "c" "d")