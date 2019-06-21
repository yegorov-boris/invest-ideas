module Brokers.Storage
    ( batchUpsert
    ) where

import qualified Brokers.Broker as B
import Flags.Flags (CliFlags)

batchUpsert :: CliFlags -> [B.Broker] -> IO ()
batchUpsert cf brokers = do
  putStrLn "started storing brokers"
  putStrLn $ show brokers
