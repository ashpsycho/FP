fun1 :: [Integer] -> Integer
fun1  = (foldr (*) 1).(map (\x -> x-2)).(filter even)

fun2 :: Integer -> Integer
fun2 = sum.(filter even).(takeWhile (>1) ).(iterate (\x -> if even x then (x `div` 2) else 1 + 3*x))

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addToTree Leaf 

addToTree :: a -> Tree a -> Tree a
addToTree a Leaf = Node 0 Leaf a Leaf
addToTree b (Node 0 _ a _) = (Node 1 (addToTree b Leaf) a Leaf)
addToTree d (Node x b@(Node x1 _ b2 _) a c@(Node x2 _ c2 _))
  | x1==x2 && x2 == (x-1) = (Node (x+1) (addToTree d b) a c)
addToTree d (Node x b a c) = (Node x b a (addToTree d c))