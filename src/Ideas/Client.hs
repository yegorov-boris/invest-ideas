module Ideas.Client
    (fetch
    ) where

import Control.Monad ((>=>))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (Chan, writeChan)
import Flags.Flags (CliFlags)
import Ideas.Response (IdeaResponse)
import Client (attemptFetch)

fetch :: CliFlags -> Chan [IdeaResponse] -> IO ()
fetch cf ideasCh = do
  return ()
--  mapConcurrently_ (worker cf ideasCh) [0, limit]
--  putStrLn "finished fetching ideas"
--
--worker :: CliFlags -> Chan [IdeaResponse] -> Int -> IO ()
--worker cf ideasCh offset = attemptFetch cf "ideas" offset >>= maybe
--  mempty
--  (writeChan ideasCh >=> (\_ -> worker cf ideasCh $ offset + 2 * limit))
