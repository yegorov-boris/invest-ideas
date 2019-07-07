module Ideas.Pipe
    ( pipe
    ) where

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.Chan.Lifted (newChan, readChan)
import Control.Monad (forever)
import Flags.Flags (CliFlags(..))
import Ideas.Client (fetch)
import Ideas.Validator as Validator
import Ideas.Storage (batchUpsert)
import Ideas.Response (IdeaResponse)
import Utils (loop)
import Common (Pipe, askFlags)

pipe :: Pipe ()
pipe = do
  ideasCh <- newChan
  Validator.start ideasCh >>= maybe
    (return ())
    (\validIdeasCh -> do
      fork $ forever $ readChan validIdeasCh >>= batchUpsert
      interval <- askFlags ideasPollingInterval
      loop interval $ fetch ideasCh
    )
