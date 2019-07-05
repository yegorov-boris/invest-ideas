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
import Common (Context(..), Pipe, askFlags)

pipe :: Pipe a ()
pipe = do
  baseURL <- askFlags ideasURL
  interval <- askFlags ideasPollingInterval
  token' <- askFlags token
  let url' = printf "%s/brokers?%s" baseURL $ urlEncodeVars [("api_key", token')]
  loop interval (results <$> fetch url' handler >>= batchUpsert)
  where
    handler = (\r i -> jsonHandler r i :: IO Body)
