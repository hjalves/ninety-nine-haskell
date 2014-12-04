data Elem a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Elem a]
encodeDirect = encodeHelper []

encodeHelper :: (Eq a) => [Elem a] -> [a] -> [Elem a]
encodeHelper ls [] = ls
encodeHelper [] (x:xs) = encodeHelper [Single x] xs
encodeHelper ls (x:xs) = let elems = encodeOne (last ls) x
                         in encodeHelper (init ls ++ elems) xs

encodeOne :: (Eq a) => Elem a -> a -> [Elem a]
encodeOne el@(Single e) x = if x == e
                            then [Multiple 2 e]
                            else [el, Single x]
encodeOne el@(Multiple n e) x = if x == e
                                then [Multiple (n + 1) e]
                                else [el, Single x]
