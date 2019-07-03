module Brokers.Pipe
    ( runFetcher
    ) where

import Network.Http.Client (jsonHandler)
import Control.Monad (mzero)
import Control.Monad.Trans.Reader (runReaderT)
import Flags.Flags (CliFlags(..))
import Client (Context(..), attemptFetch)
import Brokers.Response (Body(..))
import Brokers.Storage (batchUpsert)
import Utils (loop)

runFetcher :: CliFlags -> IO ()
runFetcher cf = loop (ideasPollingInterval cf) $ do
  let brokersHandler = \r i -> jsonHandler r i :: IO Body
  let ctx = Context {
      flags = cf
    , url = "foo" -- "https://invest-idei.ru/api/v1.1/brokers?api_key=9DveVCkwkhrPWeUhp9wezHPVMpAGKRRJ"
    , httpHandler = brokersHandler
    }
  runReaderT attemptFetch ctx >>= maybe mzero (batchUpsert cf . results)
