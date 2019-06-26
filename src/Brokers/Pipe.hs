module Brokers.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (MVar, putMVar, threadDelay)
import Flags.Flags (CliFlags(..))
import Brokers.Client (fetch)
import Brokers.Storage (batchUpsert)

runFetcher :: CliFlags -> MVar () -> IO ()
runFetcher cf m = do
  update cf
  putMVar m ()

update :: CliFlags -> IO ()
update cf = do
  fetch cf >>= maybe (return ()) (batchUpsert cf)
  threadDelay $ ideasPollingInterval cf
  update cf
