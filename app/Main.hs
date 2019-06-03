module Main where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar)
import Flags (parseCliFlags, ideasUrl, token, ideasPollingInterval)
import Brokers (brokersFetcher)

main :: IO ()
main = do
  cliFlags <- parseCliFlags
  m <- newEmptyMVar
  forkIO $ brokersFetcher m
  takeMVar m
  putStrLn "the end"
