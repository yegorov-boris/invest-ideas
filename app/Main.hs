module Main where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar)
import Flags (parseCliFlags, ideasUrl, token, ideasPollingInterval)
import Brokers (brokersFetcher)

main :: IO ()
main = do
  cliFlags <- parseCliFlags
  case cliFlags of
    Left errMsg -> putStrLn $ "failed to parse CLI flags: " ++ errMsg
    Right cf -> do
      m <- newEmptyMVar
      forkIO $ brokersFetcher (ideasPollingInterval cf) m
      takeMVar m
