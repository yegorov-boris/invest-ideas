module Main where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar)
import Control.Error (runExceptT)
import Flags.Flags (parseCliFlags)
import Brokers.Pipe (runFetcher)
import Utils (printWrap)

main :: IO ()
main = runExceptT parseCliFlags >>= either
  (printWrap "failed to parse CLI flags: ")
  (\cf -> do
    m <- newEmptyMVar
    forkIO $ runFetcher cf m
    takeMVar m)
