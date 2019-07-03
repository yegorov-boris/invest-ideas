module Brokers.Pipe
    ( runFetcher
    ) where

import Network.Http.Client (jsonHandler)
import Control.Monad.Trans.Reader (runReaderT, withReaderT)
import Flags.Flags (CliFlags(..))
import Client (Context(..), fetch)
import Brokers.Response (Body(..))
import Brokers.Storage (batchUpsert)
import Utils (loop)
import Common (Context(..))

runFetcher :: CliFlags -> IO ()
runFetcher cf = do
  loop (ideasPollingInterval cf) $ runReaderT pipe $ Context {
     flags = cf
    , url = "foo"
    , httpHandler = \r i -> jsonHandler r i :: IO Body
    }
  where
    pipe = do
      body <- fetch
      withReaderT flags $ batchUpsert $ results body
