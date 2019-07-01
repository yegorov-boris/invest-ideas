module Brokers.Pipe
    ( runFetcher
    ) where

import Control.Monad (mzero)
import Flags.Flags (CliFlags(..))
import Brokers.Client (fetch)
import Brokers.Storage (batchUpsert)
import Utils (loop)

runFetcher :: CliFlags -> IO ()
runFetcher cf = loop
  (ideasPollingInterval cf)
  (fetch cf >>= maybe mzero (batchUpsert cf))
