module Brokers
    ( brokersFetcher
    ) where

import Control.Concurrent (MVar, putMVar)

brokersFetcher :: MVar () -> IO ()
brokersFetcher m = do
  putStrLn "started fetching brokers"
  putMVar m ()
