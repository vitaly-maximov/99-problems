module Task1 where

{-
Find the last element of a list.

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

lastElement :: [a] -> Maybe a
lastElement [] = Nothing
lastElement [x] = Just x
lastElement (_ : xs) = lastElement xs

main :: IO()
main = do
    putStr "lastElement [] == Nothing \t\t\t\t"
    print $ case lastElement [] of
        Nothing -> True
        _ -> False

    putStr "lastElement [1] == Just 1 \t\t\t\t"
    print $ lastElement [1] == Just 1

    putStr "lastElement [1,2,3,4] == Just 4 \t\t"
    print $ lastElement [1,2,3,4] == Just 4

    putStr "lastElement ['x','y','z'] == Just 'z' \t"
    print $ lastElement ['x','y','z'] == Just 'z'