module Task2 where

{-
Find the last but one element of a list.

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

secondFromEnd :: [a] -> Maybe a
secondFromEnd [x, _] = Just x
secondFromEnd (_ : xs) = secondFromEnd xs
secondFromEnd _ = Nothing

main :: IO()
main = do
    putStr "secondFromEnd [] == Nothing \t\t\t"
    print $ case secondFromEnd [] of
        Nothing -> True
        _ -> False

    putStr "secondFromEnd [1] == Nothing \t\t\t"
    print $ secondFromEnd [1] == Nothing

    putStr "secondFromEnd [1,2,3,4] == Just 3 \t\t"
    print $ secondFromEnd [1,2,3,4] == Just 3

    putStr "secondFromEnd ['a'..'z'] == Just 'y' \t"
    print $ secondFromEnd ['a'..'z'] == Just 'y'
