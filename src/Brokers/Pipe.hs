module Brokers.Pipe
    ( runFetcher
    ) where

import Network.Http.Client (jsonHandler)
import Control.Monad.Trans.Reader (runReaderT)
import Flags.Flags (CliFlags(..))
import Client (Context(..), fetch)
import Brokers.Response (Body)
import Brokers.Storage (batchUpsert)
import Utils (loop)
import Common (Context(..))

runFetcher :: CliFlags -> IO ()
runFetcher cf = loop (ideasPollingInterval cf) $ do
  let brokersHandler = \r i -> jsonHandler r i :: IO Body
  let ctx = Context {
      flags = cf
    , url = "foo"
    , httpHandler = brokersHandler
    }
  runReaderT $ fetch ctx >>= batchUpsert cf . results
