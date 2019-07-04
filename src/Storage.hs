{-# LANGUAGE RecordWildCards #-}

module Storage
    ( connect
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Database.PostgreSQL.Simple as DB
import Flags.Flags (CliFlags(..))
import Common (Context(..))

connect :: ReaderT (Context a) IO DB.Connection
connect = do
  connectHost     <- askFlags dbHost
  connectPort     <- askFlags dbPort
  connectUser     <- askFlags dbUser
  connectPassword <- askFlags dbPassword
  connectDatabase <- askFlags dbName
  liftIO $ DB.connect DB.ConnectInfo {..}
  where
    askFlags = (asks $) . (. flags)
