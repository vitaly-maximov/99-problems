module Task3 where

import Common

{-
Find the K'th element of a list. The first element in the list is number 1.

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}

elementAt :: (Num b, Ord b) => [a] -> b -> a
elementAt _ i | i < 1 = error "index must be positive"
elementAt [] _ = error "index is out of range"
elementAt (x : _) 1 = x
elementAt (_ : xs) i = elementAt xs (i - 1)

main :: IO()
main = do    
    testError "#1" 
        "elementAt [1, 2, 3] (-4)" 
        (elementAt [1, 2, 3] (-4)) 
        "index must be positive"

    testError "#2"
        "elementAt [1, 2, 3] 5"
        (elementAt [1, 2, 3] 5) 
        "index is out of range"

    test "#3" 
        "elementAt [1, 2, 3] 2" 
        (elementAt [1, 2, 3] 2) 
        2

    test "#4" 
        "elementAt \"haskell\" 5" 
        (elementAt "haskell" 5) 
        'e'