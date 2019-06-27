module Ideas.Pipe
    ( runFetcher
    ) where

import Control.Conditional (if')
import Control.Concurrent (forkIO, MVar, putMVar, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Data.Either (lefts, rights)
import Data.HashSet (Set)
import qualified Data.Text as T
import Flags.Flags (CliFlags(..))
import Ideas.Client (fetch)
import Ideas.Validator (validate)
import Ideas.Storage (batchUpsert)
import Stocks.Storage (stocksCache)
import Ideas.Response (IdeaResponse)

runFetcher :: CliFlags -> MVar () -> IO ()
runFetcher cf m = do
  update cf
  putMVar m ()

update :: CliFlags -> IO ()
update cf = do
  ideasCh <- newChan
  stocksCache cf >>= maybe
    (return ())
    (\stocks -> ((forkIO $ fetch cf ideasCh) >> processIdeas cf stocks ideasCh))
  putStrLn "finished fetching ideas"
  threadDelay $ ideasPollingInterval cf
  update cf

processIdeas :: CliFlags -> Set T.Text -> Chan (Maybe [IdeaResponse]) -> IO ()
processIdeas cf stocks ideasCh = readChan ideasCh >>= maybe
  (return ())
  (\ideas ->
    let
      validatedIdeas = validate stocks `map` ideas
    in
      do
        putStrLn `mapM_` lefts validatedIdeas
        cf `batchUpsert` rights validatedIdeas
        processIdeas cf stocks ideasCh
  )
