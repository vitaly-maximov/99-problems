module Task4 where

import Common

{-
(*) Find the number of elements of a list.

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}

getLength :: Num b => [a] -> b
getLength = getLength' 0 where
    getLength' accumulator [] = accumulator
    getLength' accumulator (x : xs) = (getLength' $! (accumulator + 1)) xs

main :: IO()
main = do
    test "#1" 
        "getLength []" 
        (getLength ([] :: [Char])) 
        0

    test "#2"
        "getLength [123, 456, 789]"
        (getLength [123, 456, 789])
        3

    test "#3"
        "getLength \"Hello, World!\""
        (getLength "Hello, World!")
        13