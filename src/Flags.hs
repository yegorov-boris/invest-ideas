module Flags
    ( CliFlags
    , parseCliFlags
    , ideasURL
    , token
    , ideasPollingInterval
    , httpTimeout
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import qualified Data.List.Safe as L
import Control.Applicative (liftA2)
import Control.Error (ExceptT, hoistEither)
import Control.Monad.IO.Class (liftIO)

data CliFlagsRaw = CliFlagsRaw
  { ideasURLRaw             :: String
  , tokenRaw                :: String
  , ideasPollingIntervalRaw :: String
  , httpTimeoutRaw          :: Int }

data CliFlags = CliFlags
  { ideasURL             :: String
  , token                :: String
  , ideasPollingInterval :: Int
  , httpTimeout          :: Int }

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
      <*> option auto
          ( long "http-timeout"
         <> metavar "HTTP_TIMEOUT"
         <> value 30 )

parseCliFlags :: ExceptT String IO CliFlags
parseCliFlags = do
  raw <- liftIO $ execParser opts
  interval <- hoistEither $ parsePollingInterval $ ideasPollingIntervalRaw raw
  timeout <- hoistEither $ validateHttpTimeout $ httpTimeoutRaw raw
  return CliFlags
    { ideasURL             = ideasURLRaw raw
    , token                = tokenRaw raw
    , ideasPollingInterval = interval * 60 * second
    , httpTimeout          = timeout * second
    }
  where
    opts = info (cliFlagsRaw <**> helper)
      ( fullDesc
      <> progDesc "invest-ideas fetcher"
      <> header "hello" )
    second = 1000 * 1000

parsePollingInterval :: String -> Either String Int
parsePollingInterval s =
  liftA2 (*) (safeLast s >>= parseLetter) (safeInit s >>= safeRead)
  >>= validatePollingInterval
  where
    safeLast = maybe (Left "failed to parse ideas-polling-interval") Right . L.last
    parseLetter 'm' = Right 1
    parseLetter 'h' = Right 60
    parseLetter _   = Left "wrong format of ideas-polling-interval"
    safeInit = maybe (Left "unsupported unit of time measurement in ideas-polling-interval") Right . L.init
    safeRead = \v -> readEither v :: Either String Int

validatePollingInterval :: Int -> Either String Int
validatePollingInterval n
  | n < 1       = Left "ideas-polling-interval should be at least 1m"
  | n > 24 * 60 = Left "ideas-polling-interval should be no more than 24h"
  | otherwise = Right n

validateHttpTimeout :: Int -> Either String Int
validateHttpTimeout n
  | n < 10 = Left "http-timeout should be at least 10 seconds"
  | n > 60 = Left "http-timeout should be no more than 1 minute"
  | otherwise = Right n
