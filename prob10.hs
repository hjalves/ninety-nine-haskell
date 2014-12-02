encode :: (Eq a) => [a] -> [(Int, a)]
encode list = [(length x, head x) | x <- pack list]

pack :: (Eq a) => [a] -> [[a]]
pack = packElems [[]]

packElems :: (Eq a) => [[a]] -> [a] -> [[a]]
packElems list [] = list
packElems list (x:xs) = packElems (packElem list x) xs

packElem :: (Eq a) => [[a]] -> a -> [[a]]
packElem [[]] e = [[e]]
packElem list e = if e == last (last list)
                  then init list ++ [last list ++ [e]]
                  else list ++ [[e]]
