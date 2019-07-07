module Brokers.Pipe
    ( pipe
    ) where

import Text.Printf (printf)
import Network.HTTP.Base (urlEncodeVars)
import Network.Http.Client (jsonHandler)
import Flags.Flags (CliFlags(..))
import Client (fetch)
import Brokers.Response (Body(..))
import Brokers.Storage (batchUpsert)
import Utils (loop)
import Common (Pipe, askFlags)

pipe :: Pipe ()
pipe = do
  baseURL <- askFlags ideasURL
  interval <- askFlags ideasPollingInterval
  token' <- askFlags token
  let url' = printf "%s/brokers?%s" baseURL $ urlEncodeVars [("api_key", token')]
  loop interval $ fetch url' handler >>= maybe
    (return ())
    (batchUpsert . results)
  where
    handler = (\r i -> jsonHandler r i :: IO Body)
