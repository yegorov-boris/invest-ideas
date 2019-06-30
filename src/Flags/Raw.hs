module Flags.Raw
    ( CliFlags(..)
    ) where

import Data.Word (Word16)
import Data.IP (IP)

data CliFlags = CliFlags {
    ideasURL              :: String
  , token                 :: String
  , ideasPollingInterval  :: String
  , httpTimeout           :: Int
  , httpMaxAttempts       :: Int
  , dbHost                :: IP
  , dbPort                :: Word16
  , dbName                :: String
  , dbUser                :: String
  , dbPassword            :: String
  , stocksPollingInterval :: String
  }
