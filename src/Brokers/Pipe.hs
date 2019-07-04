module Brokers.Pipe
    ( pipe
    ) where

import Text.Printf (printf)
import Network.HTTP.Base (urlEncodeVars)
import Network.Http.Client (jsonHandler)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Flags.Flags (CliFlags(..))
import Client (fetch)
import Brokers.Response (Body(..))
import Brokers.Storage (batchUpsert)
import Utils (loop)
import Common (Context)

pipe :: ReaderT (Context a) IO ()
pipe = (asks $ ideasPollingInterval . flags) >>=
  loop (results <$> fetch url handler >>= batchUpsert)
  where
    handler = (\r i -> jsonHandler r i :: IO Body)
    url = printf "%s/brokers?%s" (ideasURL cf) $ urlEncodeVars [("api_key", token cf)]
