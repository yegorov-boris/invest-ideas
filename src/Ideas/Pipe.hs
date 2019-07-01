module Ideas.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (forkIO, newChan, readChan)
import Control.Monad (forever)
import Flags.Flags (CliFlags(..))
import Ideas.Client (fetch)
import Ideas.Validator as Validator
import Ideas.Storage (batchUpsert)
import Ideas.Response (IdeaResponse)
import Utils (loop)

runFetcher :: CliFlags -> IO ()
runFetcher cf = do
  ideasCh <- newChan
  Validator.start cf ideasCh >>= maybe
    mempty
    (\validIdeasCh -> do
      forkIO $ forever $ readChan validIdeasCh >>= batchUpsert cf
      loop (ideasPollingInterval cf) (fetch cf ideasCh)
    )
