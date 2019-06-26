module Utils
    ( printWrap
    , defaultErrorHandler
    ) where

import Control.Exception (SomeException)

printWrap :: Show a => String -> a -> IO ()
printWrap msg = putStrLn . (msg ++) . show

defaultErrorHandler :: String -> SomeException -> IO ()
defaultErrorHandler = printWrap
