myButLast ([]) = Nothing
myButLast (_:[]) = Nothing
myButLast (x:_:[]) = Just x
myButLast (x:xs) = myButLast xs