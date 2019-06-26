module Storage
    ( getConnectionInfo
    ) where

import Database.PostgreSQL.Simple (ConnectInfo(..))
import Flags.Flags (CliFlags(..))

getConnectionInfo :: CliFlags -> ConnectInfo
getConnectionInfo cf = ConnectInfo {
    connectHost     = dbHost cf
  , connectPort     = dbPort cf
  , connectUser     = dbUser cf
  , connectPassword = dbPassword cf
  , connectDatabase = dbName cf
  }
