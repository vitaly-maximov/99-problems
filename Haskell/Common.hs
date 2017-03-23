module Common where

import Control.Exception

test :: Show a => Num -> String -> a -> a -> IO()
test _ _ expression result | show expression == show result = return ()
test number description expression result = do    
    print "Test #" ++ number ++ " is failed:"    
    putStr "\t" ++ description ++ " == "    
    putStr $ "'" ++ show expression ++ "'"
    putStr " instead of "
    putStr $ "'" ++ show result ++ "'"

testError :: Show a => Num -> String -> a -> String -> IO()
testError number description expression errorMessage = do
    hasError <- throwsError expression
    case hasError of
        (Just someError) -> case someError == errorMessage of
            True -> return ()
            False -> 
        False -> do
            print "Test #" ++ number ++ " is failed:"
            putStr "\t" ++ description ++ " == "        

throwsError :: Show a => a -> IO (Maybe String)
throwsError expression = do
    value <- try $ evaluate expression
    case value of
        (Left exception) -> return $ Just $ show (exception :: SomeException)
        (Right _) -> return $ Nothing
