module Flags
    ( parseCliFlags
    , ideasUrl
    , token
    , ideasPollingInterval
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

data CliFlagsRaw = CliFlagsRaw
  { ideasUrlRaw             :: String
  , tokenRaw                :: String
  , ideasPollingIntervalRaw :: String }

data CliFlags = CliFlags
  { ideasUrl             :: String
  , token                :: String
  , ideasPollingInterval :: Int }

cliFlagsRaw :: Parser CliFlagsRaw
cliFlagsRaw = CliFlagsRaw
      <$> strOption
          ( long "ideas-url"
         <> metavar "IDEAS_URL"
         <> value "https://invest-idei.ru/api/v1.1" )
      <*> strOption
          ( long "token"
         <> metavar "TOKEN"
         <> value "9DveVCkwkhrPWeUhp9wezHPVMpAGKRRJ" )
      <*> strOption
          ( long "ideas-polling-interval"
         <> metavar "POLLING_INTERVAL"
         <> value "15m" )

parseCliFlags :: IO (Either String CliFlags)
parseCliFlags = do
  raw <- execParser opts
  return $ fromCliFlagsRaw raw
  where
    opts = info (cliFlagsRaw <**> helper)
      ( fullDesc
     <> progDesc "invest-ideas fetcher"
     <> header "hello" )

fromCliFlagsRaw :: CliFlagsRaw -> Either String CliFlags
fromCliFlagsRaw raw = Right CliFlags
  { ideasUrl = ideasUrlRaw raw
  , token = tokenRaw raw
  , ideasPollingInterval = 2000000
  }
