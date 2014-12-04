data Elem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Elem a]
encodeModified list = map tupleToElem (encode list)

tupleToElem :: (Int, a) -> Elem a
tupleToElem (l, h) = if l == 1
                     then Single h
                     else Multiple l h

-- prob 9/10

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
