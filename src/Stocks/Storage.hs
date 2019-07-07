{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Stocks.Storage
    ( stocksCache
    ) where

import Control.Exception (SomeException, displayException)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Catch (catch)
import Database.PostgreSQL.Simple (close, query_, fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags)
import Storage (connect)
import Utils (logInfo, logError)
import Common (Context(..), Cache, Pipe)

stocksCache :: Pipe Cache
stocksCache = do
  logger' <- asks logger
  (flip catch) onErr $ connect >>= \conn -> do
    tickers <- liftIO $ query_ conn [sql|SELECT ticker FROM stocks|]
    liftIO $ close conn
    logInfo' logger' "found stocks"
    return $ HashSet.fromList $ filter (not . T.null) $ map fromOnly tickers

onErr :: SomeException -> Pipe Cache
onErr e = do
  logger' <- asks logger
  logError' logger' $ printf "failed to find stocks: %s" (displayException e)
  return HashSet.empty

label = "stocks_storage"::String -- TODO
logInfo' = logInfo label
logError' = logError label
