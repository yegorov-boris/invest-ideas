{-# LANGUAGE RecordWildCards #-}

module Storage
    ( connect
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as DB
import Flags.Flags (CliFlags(..))
import Common (Context(..), Pipe, askFlags)

connect :: Pipe a DB.Connection
connect = do
  connectHost     <- askFlags dbHost
  connectPort     <- askFlags dbPort
  connectUser     <- askFlags dbUser
  connectPassword <- askFlags dbPassword
  connectDatabase <- askFlags dbName
  liftIO $ DB.connect DB.ConnectInfo {..}
