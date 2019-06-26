module Utils
    (printWrap
    ) where

printWrap :: Show a => String -> a -> IO ()
printWrap msg = putStrLn . (msg ++) . show

defaultErrorHandler :: String -> SomeException -> IO ()
defaultErrorHandler = printWrap
