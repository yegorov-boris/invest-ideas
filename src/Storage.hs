{-# LANGUAGE RecordWildCards #-}

module Storage
    ( connect
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Database.PostgreSQL.Simple as DB
import Flags.Flags (CliFlags(..))

connect :: ReaderT CliFlags IO DB.Connection
connect = do
  connectHost <- asks dbHost
  connectPort <- asks dbPort
  connectUser <- asks dbUser
  connectPassword <- asks dbPassword
  connectDatabase <- asks dbName
  liftIO $ DB.connect DB.ConnectInfo {..}
