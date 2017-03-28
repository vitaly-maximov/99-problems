module Task2 where

import Common

{-
Find the last but one element of a list.

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

secondFromEnd :: [a] -> a
secondFromEnd [] = error "list is too small"
secondFromEnd [x, _] = x
secondFromEnd (_ : xs) = secondFromEnd xs


main :: IO()
main = do
    testError "#1"
        "secondFromEnd []"
        (secondFromEnd ([] :: [Int]))
        "list is too small"

    testError "#2"
        "secondFromEnd [1]"
        (secondFromEnd [1])
        "list is too small"

    test "#3"
        "secondFromEnd [1,2,3,4]"
        (secondFromEnd [1,2,3,4])
        3

    test "#4"
        "secondFromEnd ['a'..'z']"
        (secondFromEnd ['a'..'z'])
        'y'