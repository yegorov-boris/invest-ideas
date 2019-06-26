module Main where

import Control.Concurrent (newEmptyMVar, forkIO, takeMVar)
import Control.Error (runExceptT)
import Flags.Flags (parseCliFlags)
import qualified Brokers.Pipe as BrokersPipe
import qualified Ideas.Pipe as IdeasPipe
import Utils (printWrap)

main :: IO ()
main = runExceptT parseCliFlags >>= either
  (printWrap "failed to parse CLI flags: ")
  (\cf -> do
    m <- newEmptyMVar
    forkIO $ BrokersPipe.runFetcher cf m
    forkIO $ IdeasPipe.runFetcher cf m
    takeMVar m)
