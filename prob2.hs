myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Only has one element"
myButLast (x:[_]) = x
myButLast (x:xs) = myButLast xs
