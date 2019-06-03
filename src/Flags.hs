module Flags
    ( parseCliFlags
    , ideasUrl
    , token
    , ideasPollingInterval
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import qualified Data.List.Safe as L
import Control.Applicative (liftA2)

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
  return $ fromRaw raw <$> parsePollingInterval (ideasPollingIntervalRaw raw)
  where
    opts = info (cliFlagsRaw <**> helper)
      ( fullDesc
      <> progDesc "invest-ideas fetcher"
      <> header "hello" )
    fromRaw raw interval = CliFlags
      { ideasUrl = ideasUrlRaw raw
      , token = tokenRaw raw
      , ideasPollingInterval = interval
      }

parsePollingInterval :: String -> Either String Int
parsePollingInterval s = liftA2 (*) (safeLast s >>= parseLetter) (safeInit s >>= safeRead) >>= validate
  where
    safeLast = maybe (Left "failed to parse ideas-polling-interval") Right . L.last
    parseLetter 'm' = Right 1
    parseLetter 'h' = Right 60
    parseLetter _   = Left "wrong format of ideas-polling-interval"
    safeInit = maybe (Left "unsupported unit of time measurement in ideas-polling-interval") Right . L.init
    safeRead = \v -> readEither v :: Either String Int

validate :: Int -> Either String Int
validate n
  | n < 1     = Left "ideas-polling-interval should be at least 1m"
  | n > 24*60 = Left "ideas-polling-interval should be no more than 24h"
  | otherwise = Right $ 60*1000*1000*n
