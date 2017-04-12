fib :: Integer -> Integer
fib x
  | (x<1) = 0
  | (x<3) = 1 
  | True = fib (x-1) + fib (x-2) 

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fib2 [0..]
fib2 :: Int -> Integer
fib2 x
  | (x<1) = 0
  | (x<3) = 1
  | True  = (fibs2!!(x-1)) + (fibs2!!(x-2))

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a as) = a:(streamToList as)

instance (Show a) => Show (Stream a) where
	show = show.(take 40).streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a as) = Stream (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a)) 

nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap (getRuler 0) nats

getRuler :: Integer -> Integer -> Integer
getRuler curr x
  | (x `mod` 2)==1 = curr
  | True = getRuler (curr+1) (x `div` 2) 
