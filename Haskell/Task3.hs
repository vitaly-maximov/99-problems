module Task3 where

import Common

{-
Find the K'th element of a list. The first element in the list is number 1.

Example:
* (element-at '(a b c d e) 3)
c

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}

--elementAt :: [a] -> Num -> Maybe a
elementAt _ i | i < 1 = error "index must be positive"
elementAt [] _ = error "index is out of range"
elementAt (x : _) 1 = x
elementAt (_ : xs) i = elementAt xs (i - 1)



main :: IO()
main = do    
    testError 
        "elementAt [1, 2, 3] (-4) => Error: index must be positive"
        (elementAt [1, 2, 3] (-4)) "index must be positive"

    testError
        "elementAt [1, 2, 3] 5 ==> Error: index is out of range"
        (elementAt [1, 2, 3] 5) "index is out of range"

    test 
        "elementAt [1, 2, 3] 2 ==> 2"
        (elementAt [1, 2, 3] 2 == 2)

