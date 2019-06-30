module Brokers.Pipe
    ( runFetcher
    ) where

import Control.Monad (forever, mzero)
import Control.Concurrent (threadDelay)
import Flags.Flags (CliFlags(..))
import Brokers.Client (fetch)
import Brokers.Storage (batchUpsert)

runFetcher :: CliFlags -> IO ()
runFetcher cf = forever $ do
  fetch cf >>= maybe mzero (batchUpsert cf)
  threadDelay $ ideasPollingInterval cf -- TODO: move `forever` with `threadDelay` to a separate helper
