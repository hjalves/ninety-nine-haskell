data Elem a = Single a | Multiple Int a deriving (Show)

decodeModified :: (Eq a) => [Elem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeElem x ++ decodeModified xs

decodeElem :: Elem a -> [a]
decodeElem (Single x) = [x]
decodeElem (Multiple n x) = take n $ repeat x
