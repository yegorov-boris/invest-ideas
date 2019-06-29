module Ideas.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (forkIO, MVar, putMVar, threadDelay)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (forever)
import Flags.Flags (CliFlags(..))
import Ideas.Client (fetch)
import Ideas.Validator as Validator
import Ideas.Storage (batchUpsert)
import Ideas.Response (IdeaResponse)

-- TODO: try to use MaybeT
runFetcher :: CliFlags -> MVar () -> IO ()
runFetcher cf m = do
  ideasCh <- newChan
  Validator.start cf ideasCh >>= maybe
    mempty
    (\validIdeasCh -> do
      forkIO $ forever $ readChan validIdeasCh >>= batchUpsert cf
      forever $ fetch cf ideasCh >> (threadDelay $ ideasPollingInterval cf)
      putMVar m ()
    )
