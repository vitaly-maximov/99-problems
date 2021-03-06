module Task7 where

import Common

{-
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list 
by replacing each list with its elements (recursively).

We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}

data NestedList a = Elem a | List [NestedList a]
    deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = (flatten x) ++ (flatten $ List xs)

main :: IO()
main = do
    test "#1"
        "flatten $ List []"
        (flatten $ List ([] :: [NestedList Int]))
        ([] :: [Int])

    test "#2"
        "flatten (Elem 5)"
        (flatten (Elem 5))
        [5]

    test "#3"
        "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])"
        (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
        [1, 2, 3, 4, 5]
