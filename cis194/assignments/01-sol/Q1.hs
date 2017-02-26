toDigitsRev:: Integer -> [Integer]
toDigitsRev x
  | x<0 = []
  | x<10 = [x]
  | otherwise = (x `mod` 10):(toDigitsRev (x`div`10))

reverseList::[Integer] -> [Integer]
reverseList []=[]
reverseList [x]=[x]
reverseList (x:xs) = concat [(reverseList xs),[x]] 

toDigits:: Integer -> [Integer]
toDigits = reverseList . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []=[]
doubleEveryOtherRev [x]=[x]
doubleEveryOtherRev (x:y:xs)=x:(2*y):(doubleEveryOtherRev xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverseList . doubleEveryOtherRev . reverseList

sumDigitsSingle :: Integer -> Integer
sumDigitsSingle x
  | x<0 = 0
  | x<10 = x
  | otherwise = (x `mod` 10) + sumDigitsSingle (x `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] =0
sumDigits (x:xs) = (sumDigitsSingle x) + sumDigits xs

validate :: Integer -> Bool
validate xs = (sumDigits (doubleEveryOther (toDigits xs))) `mod` 10 ==0  