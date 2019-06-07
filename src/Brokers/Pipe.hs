module Brokers.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (MVar, putMVar, threadDelay)
import Flags (CliFlags, ideasPollingInterval)
import Brokers.Client (fetch)

runFetcher :: CliFlags -> MVar () -> IO ()
runFetcher cf m = do
  update cf
  putMVar m ()

update :: CliFlags -> IO ()
update cf = do
  fetch cf
  threadDelay $ ideasPollingInterval cf
  update cf
