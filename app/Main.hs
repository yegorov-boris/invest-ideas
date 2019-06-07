module Main where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar)
import Flags (parseCliFlags)
import Brokers.Pipe (runFetcher)

main :: IO ()
main = parseCliFlags >>= either
  (putStrLn . ("failed to parse CLI flags: " ++))
  (\cf -> do
    m <- newEmptyMVar
    forkIO $ runFetcher cf m
    takeMVar m)
