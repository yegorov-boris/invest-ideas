module Flags.Flags
    ( CliFlags(..)
    , parseCliFlags
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Read (readEither)
import qualified Data.List.Safe as L
import Control.Applicative (liftA2)
import Control.Error (ExceptT, hoistEither)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word16)
import Data.IP (IP)
import qualified Flags.Raw as R

data CliFlags = CliFlags {
    ideasURL              :: String
  , token                 :: String
  , ideasPollingInterval  :: Int
  , httpTimeout           :: Int
  , httpMaxAttempts       :: Int
  , dbHost                :: String
  , dbPort                :: Word16
  , dbName                :: String
  , dbUser                :: String
  , dbPassword            :: String
  , stocksPollingInterval :: Int
  }

cliFlagsRaw :: Parser R.CliFlags
cliFlagsRaw = R.CliFlags
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
         <> metavar "IDEAS_POLLING_INTERVAL"
         <> value "15m" )
      <*> option auto
          ( long "http-timeout"
         <> metavar "HTTP_TIMEOUT"
         <> value 30 )
      <*> option auto
          ( long "http-max-attempts"
         <> metavar "HTTP_MAX_ATTEMPTS"
         <> value 3 )
      <*> strOption
          ( long "db-host"
         <> metavar "DB_HOST"
         <> value (read "127.0.0.1" :: IP) )
      <*> option auto
          ( long "db-port"
         <> metavar "DB_PORT"
         <> value 5432 )
      <*> strOption
          ( long "db-name"
         <> metavar "DB_NAME"
         <> value "feed_test" )
      <*> strOption
          ( long "db-user"
         <> metavar "DB_USER"
         <> value "user" )
      <*> strOption
          ( long "db-password"
         <> metavar "DB_PASSWORD"
         <> value "passwd" )
      <*> strOption
          ( long "stocks-polling-interval"
         <> metavar "POLLING_INTERVAL"
         <> value "24h" )

parseCliFlags :: ExceptT String IO CliFlags
parseCliFlags = do
  raw <- liftIO $ execParser opts
  ideasInterval <- hoistEither $ parsePollingInterval "ideas-polling-interval" $ R.ideasPollingInterval raw
  stocksInterval <- hoistEither $ parsePollingInterval "stocks-polling-interval" $ R.stocksPollingInterval raw
  timeout <- hoistEither $ validateHttpTimeout $ R.httpTimeout raw
  maxAttempts <- hoistEither $ validateHttpMaxAttempts $ R.httpMaxAttempts raw
  return CliFlags {
      ideasURL              = R.ideasURL raw
    , token                 = R.token raw
    , ideasPollingInterval  = ideasInterval * 60 * second
    , stocksPollingInterval = stocksInterval * 60 * second
    , httpTimeout           = timeout * second
    , httpMaxAttempts       = maxAttempts
    , dbHost                = show $ R.dbHost raw
    , dbPort                = R.dbPort raw
    , dbName                = R.dbName raw
    , dbUser                = R.dbUser raw
    , dbPassword            = R.dbPassword raw
    }
  where
    opts = info (cliFlagsRaw <**> helper)
      ( fullDesc
      <> progDesc "invest-ideas fetcher"
      <> header "hello" )
    second = 1000 * 1000

parsePollingInterval :: String -> String -> Either String Int
parsePollingInterval name s =
  liftA2 (*) (safeLast s >>= parseLetter) (safeInit s >>= safeRead)
  >>= validatePollingInterval name
  where
    safeLast = maybe (Left $ "failed to parse " ++ name) Right . L.last
    parseLetter 'm' = Right 1
    parseLetter 'h' = Right 60
    parseLetter _   = Left $ "wrong format of " ++ name
    safeInit = maybe (Left $ "unsupported unit of time measurement in " ++ name) Right . L.init
    safeRead = \v -> readEither v :: Either String Int

validatePollingInterval :: String -> Int -> Either String Int
validatePollingInterval name n
  | n < 1       = Left $ name ++ " should be at least 1m"
  | n > 24 * 60 = Left $ name ++ " should be no more than 24h"
  | otherwise = Right n

validateHttpTimeout :: Int -> Either String Int
validateHttpTimeout n
  | n < 10 = Left "http-timeout should be at least 10 seconds"
  | n > 60 = Left "http-timeout should be no more than 1 minute"
  | otherwise = Right n

validateHttpMaxAttempts :: Int -> Either String Int
validateHttpMaxAttempts n
  | n < 1 = Left "http-max-attempts should be at least 1"
  | n > 3 = Left "http-max-attempts should be no more than 3"
  | otherwise = Right n
