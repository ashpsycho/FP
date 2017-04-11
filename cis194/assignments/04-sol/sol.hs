import Data.List

fun1 :: [Integer] -> Integer
fun1  = (foldr (*) 1).(map (\x -> x-2)).(filter even)

fun2 :: Integer -> Integer
fun2 = sum.(filter even).(takeWhile (>1) ).(iterate (\x -> if even x then (x `div` 2) else 1 + 3*x))

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Need to find better way to do this
foldTree :: [a] -> Tree a
foldTree = foldr addToTree Leaf 

addToTree :: a -> Tree a -> Tree a
addToTree a Leaf = Node 0 Leaf a Leaf
addToTree b (Node 0 _ a _) = (Node 1 (addToTree b Leaf) a Leaf)
addToTree c (Node 1 b a Leaf) = (Node 1 b a (addToTree c Leaf))
addToTree d (Node x b@(Node x1 _ _ _) a c@(Node x2 _ _ _))
  | x2<x1 = (Node x b a (addToTree d c))
  | True = (Node (h+1) (addToTree d b) a c) where h = getHeight (addToTree d b)

getHeight :: Tree a-> Integer
getHeight Leaf = -1
getHeight (Node x _ _ _) = x

xor:: [Bool] -> Bool
xor = odd.length.(filter (\x -> x)) 

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x bs-> (f x):bs) []

-- this is wrong
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc = foldr (\x acc -> f acc x) acc 

sieveSundaram :: Integer -> [Integer]
sieveSundaram n= (map (\x -> (1+2*x))
  ([1..n] \\
  (sort [(i+j+2*i*j)|i <-[1..n],j <- [1..i], (i+j+2*i*j)<=n])))
