module Ideas.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (forkIO, threadDelay, newChan, readChan)
import Control.Monad (forever)
import Flags.Flags (CliFlags(..))
import Ideas.Client (fetch)
import Ideas.Validator as Validator
import Ideas.Storage (batchUpsert)
import Ideas.Response (IdeaResponse)

runFetcher :: CliFlags -> IO ()
runFetcher cf = do
  ideasCh <- newChan
  Validator.start cf ideasCh >>= maybe
    mempty
    (\validIdeasCh -> do
      forkIO $ forever $ readChan validIdeasCh >>= batchUpsert cf
      forever $ fetch cf ideasCh >> (threadDelay $ ideasPollingInterval cf)
    )
