isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome list = head list == last list && 
                    isPalindrome (init $ tail list)
