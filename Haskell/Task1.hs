module Task1 where

import Common

{-
Find the last element of a list.

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

lastElement :: [a] -> a
lastElement [] = error "list is empty"
lastElement [x] = x
lastElement (_ : xs) = lastElement xs

main :: IO()
main = do
    testError "#1"
        "lastElement []"
        (lastElement ([] :: [Int]))
        "list is empty"

    test "#2"
        "lastElement [1]"
        (lastElement [1])
        1

    test "#3"
        "lastElement [1,2,3,4]"
        (lastElement [1,2,3,4])
        4

    test "#4"
        "lastElement ['x','y','z']"
        (lastElement ['x','y','z'])
        'z'