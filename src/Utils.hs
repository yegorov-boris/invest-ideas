module Utils
    ( defaultErrorHandler
    , parseCustomTime
    , loop
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

defaultErrorHandler :: String -> SomeException -> IO ()
defaultErrorHandler s e = putStrLn $ s ++ show e

parseCustomTime :: String -> Maybe ZonedTime
parseCustomTime = parseTimeM False defaultTimeLocale "%d.%m.%Y"

loop :: MonadIO f => f () -> Int -> f ()
loop action interval = forever $ action >> (liftIO $ threadDelay interval)
