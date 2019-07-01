module Utils
    ( printWrap
    , defaultErrorHandler
    , parseCustomTime
    , loop
    ) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

printWrap :: Show a => String -> a -> IO ()
printWrap msg = putStrLn . (msg ++) . show

defaultErrorHandler :: String -> SomeException -> IO ()
defaultErrorHandler = printWrap

parseCustomTime :: String -> Maybe ZonedTime
parseCustomTime = parseTimeM False defaultTimeLocale "%d.%m.%Y"

loop :: Int -> IO () -> IO ()
loop interval action = forever $ action >> threadDelay interval
