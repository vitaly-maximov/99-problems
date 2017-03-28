module Common (test, testError) where

import Control.Exception

test :: (Show a, Show b) => String -> String -> a -> b -> IO()
test _ _ expression expectedResult | show expression == show expectedResult = return ()
test testNumber testDescription expression expectedResult = do
    putStrLn $ "Test " ++ testNumber ++ " is failed:"
    putStr $ "\t" ++ testDescription ++ " => "    
    putStr $ show expression
    putStr " instead of "
    putStrLn $ show expectedResult

testError :: Show a => String -> String -> a -> String -> IO()
testError testNumber testDescription expression expectedError = do
    value <- try $ evaluate expression
    case value of
        (Left exception) -> 
            let
                expressionError = show (exception :: SomeException)
            in case expressionError == expectedError of
                True -> return ()
                False -> test' $ wrapError expressionError
        (Right something) -> test' something
    where
        wrapError :: String -> String
        wrapError = (++) "Error: "

        test' :: Show a => a -> IO()
        test' expression = 
            test testNumber testDescription expression (wrapError expectedError)

main :: IO()
main = do    
    putStrLn "Test #1 is ok"
    test "#1" "1 + 1" (1 + 1) 2
    test "#2" "2 * 2" (2 * 3) 4
    putStrLn "Test #3 is ok"
    testError "#3" "error \"A\"" (error "A" :: String) "A"
    testError "#4" "error \"A\"" (error "B" :: String) "A"
    testError "#5" "error \"A\"" 0 "A"
