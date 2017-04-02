module Task5 where

import Common

{-
(*) Reverse a list.

Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
-}

reverseList :: [a] -> [a]
reverseList = reverseList' [] where
    reverseList' accumulator [] = accumulator
    reverseList' accumulator (x : xs) = (reverseList' $! (x : accumulator)) xs

main :: IO()
main = do
    test "#1"
        "reverseList []"
        (reverseList ([] :: [Int]))
        ([] :: [Int])

    test "#2"
        "reverseList \"A man, a plan, a canal, panama!\""
        (reverseList "A man, a plan, a canal, panama!")
        "!amanap ,lanac a ,nalp a ,nam A"

    test "#3"
        "reverseList [1, 2, 3, 4]"
        (reverseList [1, 2, 3, 4])
        [4, 3, 2, 1]

    
