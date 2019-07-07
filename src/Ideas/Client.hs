module Ideas.Client
    (fetch
    ) where

import Network.Http.Client (jsonHandler)
import Network.HTTP.Base (urlEncodeVars)
import Text.Printf (printf)
import Control.Monad.Trans.Reader (asks)
import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Control.Concurrent.Chan.Lifted (Chan, writeChan)
import Flags.Flags (CliFlags(..))
import Ideas.Response (IdeaResponse, Body(..))
import qualified Client as C
import Common (Context(..), Pipe, askFlags)
import Utils (logInfo)

limit = 100::Int

fetch :: Chan [IdeaResponse] -> Pipe ()
fetch ideasCh = do
  mapConcurrently_ (worker ideasCh) [0, limit]
  logger' <- asks logger
  logInfo' logger' "finished fetching ideas"

worker :: Chan [IdeaResponse] -> Int -> Pipe ()
worker ideasCh offset = do
  baseURL <- askFlags ideasURL
  interval <- askFlags ideasPollingInterval
  token' <- askFlags token
  let url' = printf "%s/ideas?%s" baseURL $ urlEncodeVars [ ("api_key", token') -- TODO: dup
                                                          , ("offset", show offset)
                                                          , ("limit", show limit)
                                                          ]
  body <- C.fetch url' handler
  case body of
    Nothing             -> return ()
    Just (Body _ [])    -> return ()
    Just (Body _ ideas) -> writeChan ideasCh ideas >> worker ideasCh (offset + 2 * limit)
  where
    handler = (\r i -> jsonHandler r i :: IO Body) -- TODO: dup

label = "ideas_client"::String
logInfo' = logInfo label
