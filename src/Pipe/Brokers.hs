module Pipe.Brokers
    ( brokersFetcher
    ) where

import Control.Concurrent (MVar, putMVar, threadDelay)

brokersFetcher :: Int -> MVar () -> IO ()
brokersFetcher d m = do
  putStrLn "started fetching brokers"
  update d
  putMVar m ()

update :: Int -> IO ()
update d = do
  putStrLn "tick"
  threadDelay d
  update d
