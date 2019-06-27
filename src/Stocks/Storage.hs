-- TODO: dup
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Stocks.Storage
    ( stocksCache
    ) where

import Control.Exception (handle)
import Database.PostgreSQL.Simple (connect, close, query_, fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags)
import Storage (getConnectionInfo)
import Utils (defaultErrorHandler)

stocksCache :: CliFlags -> IO (Maybe (HashSet.Set T.Text))
stocksCache cf = do
  handle ((Nothing <$) . defaultErrorHandler "failed to find stocks: ") (doStocksCache cf)

doStocksCache :: CliFlags -> IO (Maybe (HashSet.Set T.Text))
doStocksCache cf = do
  conn <- connect $ getConnectionInfo cf
  tickers <- query_ conn [sql|SELECT ticker FROM stocks|]
  close conn
  putStrLn "found stocks"
  return $ Just $ HashSet.fromList $ fromOnly <$> tickers
