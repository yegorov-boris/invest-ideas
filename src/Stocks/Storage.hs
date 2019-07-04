{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Stocks.Storage
    ( stocksCache
    ) where

import Control.Exception (handle)
import Database.PostgreSQL.Simple (close, query_, fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags)
import Storage (connect)
import Utils (defaultErrorHandler)
import Common (Cache)

stocksCache :: CliFlags -> IO Cache
stocksCache cf = do
  return HashSet.empty
--  "failed to find stocks: "
--  tickers <- query_ conn [sql|SELECT ticker FROM stocks|]
--  close conn
--  putStrLn "found stocks"
--  return $ Just $ HashSet.fromList $ filter (not . T.null) $ map fromOnly tickers
