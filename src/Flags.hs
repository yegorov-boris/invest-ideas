module Flags
    ( parseCliFlags
    , ideasUrl
    , token
    , ideasPollingInterval
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

data CliFlags = CliFlags
  { ideasUrl        :: String
  , token           :: String
  , ideasPollingInterval :: String }

cliFlags :: Parser CliFlags
cliFlags = CliFlags
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

parseCliFlags :: IO CliFlags
parseCliFlags = execParser opts
  where
    opts = info (cliFlags <**> helper)
      ( fullDesc
     <> progDesc "invest-ideas fetcher"
     <> header "hello" )
