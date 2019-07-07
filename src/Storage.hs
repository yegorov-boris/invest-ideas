{-# LANGUAGE RecordWildCards #-}

module Storage
    ( connect
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as DB
import Flags.Flags (CliFlags(..))
import Common (Pipe, askFlags)

connect :: Pipe DB.Connection
connect = do
  connectHost     <- askFlags dbHost
  connectPort     <- askFlags dbPort
  connectUser     <- askFlags dbUser
  connectPassword <- askFlags dbPassword
  connectDatabase <- askFlags dbName
  liftIO $ DB.connect DB.ConnectInfo {..}
