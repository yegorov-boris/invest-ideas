{-# LANGUAGE FlexibleContexts #-}

module Utils
    (
      parseCustomTime
    , loop
    , logInfo
    , logWarning
    , logError
    ) where

import qualified Data.Text as T
import qualified Control.Monad.Log as L
import Control.Monad.Log.Label (Label(..), withLabel)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)
import Control.Monad (forever)
import Control.Concurrent.Lifted (threadDelay)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

parseCustomTime :: String -> Maybe ZonedTime
parseCustomTime = parseTimeM False defaultTimeLocale "%d.%m.%Y"

loop :: Control.Monad.Base.MonadBase IO f => Int -> f a -> f b
loop interval action = forever $ action >> threadDelay interval

logAction :: MonadIO m => (T.Text -> L.LogT Label m a) -> String -> L.Logger Label -> String -> m a
logAction action label logger' msg = L.runLogT' logger' $ do
  withLabel (Label $ T.pack label) $ do
    action $ T.pack msg

logInfo :: MonadIO m => String -> L.Logger Label -> String -> m ()
logInfo = logAction L.info

logWarning :: MonadIO m => String -> L.Logger Label -> String -> m ()
logWarning = logAction L.warning

logError :: MonadIO m => String -> L.Logger Label -> String -> m ()
logError = logAction L.error
