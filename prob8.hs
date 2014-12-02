compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs) = if x == head xs
                  then compress(xs)
                  else x : compress(xs)
