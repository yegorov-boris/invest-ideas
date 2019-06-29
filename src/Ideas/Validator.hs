module Ideas.Validator
    ( start
    ) where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Control.Conditional (if', select)
import Control.Monad (forever, filterM, (>=>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent (forkIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Ideas.Response (IdeaResponse(..))
import Stocks.Storage (stocksCache)
import Flags.Flags (CliFlags(..))

type Cache = HashSet.Set T.Text
type Finder = IdeaResponse -> Bool
type IdeasChan = Chan [IdeaResponse]

-- TODO: try to use MaybeT
start :: CliFlags -> Chan [IdeaResponse] -> IO (Maybe (IdeasChan))
start cf ideasCh = stocksCache cf >>= maybe
  (return Nothing)
  (\stocks -> do
    validIdeasCh <- newChan -- TODO: try to use bimap
    forkIO $ forever $ validateBatch stocks ideasCh validIdeasCh
    return $ Just validIdeasCh
  )

-- TODO: update the stocks by timeout
validateBatch :: Cache -> IdeasChan -> IdeasChan -> IO ()
validateBatch stocks ideasCh validIdeasCh = do
  ideas <- readChan ideasCh -- TODO: try to use uncurry
  now <- getCurrentTime
  let
    finder = (`HashSet.member` stocksCache) . ticker
  in
    (filterM validateM finder now $ setTicker finder `map` ideas)
    >>= writeChan validIdeasCh

validateBy :: Finder -> UTCTime -> IdeaResponse -> IO Bool
validateBy finder now idea
  | not $ isOpen idea                             = putStrLn closed >> return False
  | not $ finder idea                             = putStrLn tickerMsg >> return False
  | hasDateEnd && (zonedTimeToUTC dateEnd') < now = putStrLn outdated >> return False
  | hasDateEnd && (dateStart idea > dateEnd')     = putStrLn dateStartMsg >> return False
  | otherwise = return True
  where
    dateEnd'     = fromJust $ dateEnd idea
    hasDateEnd   = isJust $ dateEnd idea
    closed       = "idea is closed"
    tickerMsg    = "ticker not found"
    outdated     = "idea is outdated"
    dateStartMsg = "an idea should have its date start before its date end"

setTicker :: Finder -> IdeaResponse -> IdeaResponse
setTicker finder idea = select
  finder
  id
  (\i -> let t = ticker i in i {ticker = T.takeWhile (== '.') t})
