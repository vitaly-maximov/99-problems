module Task6 where

import Common

{-
(*) Find out whether a list is a palindrome. 
A palindrome can be read forward or backward; e.g. (x a m a x).

*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

main :: IO()
main = do
    test "#1"
        "isPalindrome []"
        (isPalindrome ([] :: [Int]))
        True

    test "#2"
        "isPalindrome [1]"
        (isPalindrome [1])
        True

    test "#3"
        "isPalindrome [1, 2, 3]"
        (isPalindrome [1, 2, 3])
        False

    test "#4"
        "isPalindrome \"madamimadam\""
        (isPalindrome "madamimadam")
        True

    test "#5"
        "isPalindrome [1,2,4,8,16,8,4,2,1]"
        (isPalindrome [1,2,4,8,16,8,4,2,1])
        True
