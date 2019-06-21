module Brokers.Pipe
    ( runFetcher
    ) where

import Control.Concurrent (MVar, putMVar, threadDelay)
import Data.Either (rights, lefts)
import Control.Monad (mapM_)
import Flags.Flags (CliFlags, ideasPollingInterval)
import Brokers.Client (fetch)
import Brokers.Mapper (fromResponse)
import Brokers.Storage (batchUpsert)

runFetcher :: CliFlags -> MVar () -> IO ()
runFetcher cf m = do
  update cf
  putMVar m ()

update :: CliFlags -> IO ()
update cf = do
  fetch cf >>= maybe
    (return ())
    (\brokersResponse -> do
      brokers <- return $ map fromResponse brokersResponse
      mapM_ putStrLn $ lefts brokers
      batchUpsert cf $ rights brokers)
  threadDelay $ ideasPollingInterval cf
  update cf
