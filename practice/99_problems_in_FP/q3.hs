elementAt:: (Num b, Eq b) => [a] -> b -> a
elementAt ([]) k = error "need more elements"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)