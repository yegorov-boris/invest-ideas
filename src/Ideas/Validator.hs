module Ideas.Validator
    ( start
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft, partitionEithers)
import Text.Printf (printf)
import Control.Monad.Trans.Reader (asks)
import Data.Maybe (fromJust, isJust)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Control.Conditional (select)
import Control.Monad (forever, mapM_, (>=>), when)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.MVar.Lifted (MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.Chan.Lifted (Chan, newChan, readChan, writeChan)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Ideas.Response (IdeaResponse(..))
import Stocks.Storage (stocksCache)
import Flags.Flags (CliFlags(..))
import Utils (loop, logInfo, logWarning)
import Common (Context(..), Cache, Pipe, askFlags)

type IdeasChan = Chan [IdeaResponse]
type Finder = IdeaResponse -> Bool

start :: IdeasChan -> Pipe (Maybe IdeasChan)
start ideasCh = stocksCache >>= select
  (HashSet.null)
  (const $ return Nothing)
  (\stocks -> do
    m <- newMVar stocks
    interval <- askFlags stocksPollingInterval
    fork $ loop interval (updateCache m)
    validIdeasCh <- newChan
    fork $ forever $ validateBatch m ideasCh validIdeasCh
    return $ Just validIdeasCh
  )

updateCache :: MVar Cache -> Pipe ()
updateCache m = stocksCache >>= select
  (HashSet.null)
  (const $ return ())
  (\stocks -> do
    swapMVar m stocks
    logger' <- asks logger
    logInfo' logger' "stocks cache updated"
  )

validateBatch :: MVar Cache -> IdeasChan -> IdeasChan -> Pipe ()
validateBatch stocksCh ideasCh validIdeasCh = do
  ideas <- readChan ideasCh
  stocks <- readMVar stocksCh
  now <- liftIO $ getCurrentTime
  let finder = (`HashSet.member` stocks) . ticker
  let ideas' = map (validate finder now . setTicker finder) ideas
  let (warnings, validIdeas) = partitionEithers ideas'
  writeChan validIdeasCh validIdeas
  logger' <- asks logger
  mapM_ (logWarning' logger') warnings

validate :: Finder -> UTCTime -> IdeaResponse -> Either String IdeaResponse
validate finder now idea
  | not $ isOpen idea                     = Left closed
  | not $ finder idea                     = Left tickerMsg
  | hasDateEnd && dateEnd' < now          = Left outdated
  | hasDateEnd && (dateStart' > dateEnd') = Left dateStartMsg
  | otherwise = Right idea
  where
    dateEnd'     = zonedTimeToUTC $ fromJust $ dateEnd idea
    dateStart'   = zonedTimeToUTC $ dateStart idea
    hasDateEnd   = isJust $ dateEnd idea
    closed       = printf "idea is closed %d" id
    tickerMsg    = printf "ticker not found %d" id
    outdated     = printf "idea is outdated %d" id
    dateStartMsg = printf "an idea should have its date start before its date end %d" id
    id = externalID idea

setTicker :: Finder -> IdeaResponse -> IdeaResponse
setTicker finder = select
  finder
  id
  (\i -> let t = ticker i in i {ticker = T.takeWhile (== '.') t})

label = "validator"::String
logInfo' = logInfo label
logWarning' = logWarning label
