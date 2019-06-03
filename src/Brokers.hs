module Brokers
    ( brokersFetcher
    ) where

import Control.Concurrent (MVar, putMVar, threadDelay)

brokersFetcher :: MVar () -> IO ()
brokersFetcher m = do
  putStrLn "started fetching brokers"
  update 2000000
  putMVar m ()

update :: Int -> IO ()
update d = do
  putStrLn "tick"
  threadDelay d
  update d
