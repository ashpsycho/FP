myReverse xs = foldl (\acc x -> (x:acc)) [] xs
isPalindrome xs = (xs == myReverse xs)