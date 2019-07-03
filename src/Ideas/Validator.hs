module Ideas.Validator
    ( start
    ) where

import Data.Maybe (fromJust, isJust)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Control.Conditional (select)
import Control.Monad (forever, filterM, (>=>), mzero)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, swapMVar, Chan, newChan, readChan, writeChan)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Ideas.Response (IdeaResponse(..))
import Stocks.Storage (stocksCache)
import Flags.Flags (CliFlags(..))
import Utils (loop)

---- TODO: move to the proper place and import in other places
type Cache = HashSet.Set T.Text
type Finder = IdeaResponse -> Bool
type IdeasChan = Chan [IdeaResponse]

start :: CliFlags -> IdeasChan -> IO (Maybe IdeasChan)
start cf ideasCh = newChan >>= return . Just
--start cf ideasCh = stocksCache cf >>= maybe
--  (return Nothing)
--  (\stocks -> do
--    m <- newMVar stocks
--    forkIO $ loop (stocksPollingInterval cf) (updateCache cf m)
--    validIdeasCh <- newChan
--    forkIO $ forever $ validateBatch m ideasCh validIdeasCh
--    return $ Just validIdeasCh
--  )
--
--updateCache :: CliFlags -> MVar Cache -> IO ()
--updateCache cf m = do
--  stocksCache cf >>= maybe
--    mzero
--    (swapMVar m >=> \_ -> putStrLn "stocks cache updated")
--
--validateBatch :: MVar Cache -> IdeasChan -> IdeasChan -> IO ()
--validateBatch stocksCh ideasCh validIdeasCh = do
--  ideas <- readChan ideasCh
--  stocks <- readMVar stocksCh
--  now <- getCurrentTime
--  let finder = (`HashSet.member` stocks) . ticker
--  filterM (validateM finder now) (setTicker finder `map` ideas) >>= writeChan validIdeasCh
--
--validateM :: Finder -> UTCTime -> IdeaResponse -> IO Bool
--validateM finder now idea
--  | not $ isOpen idea                     = putStrLn closed >> return False
--  | not $ finder idea                     = putStrLn tickerMsg >> return False
--  | hasDateEnd && dateEnd' < now          = putStrLn outdated >> return False
--  | hasDateEnd && (dateStart' > dateEnd') = putStrLn dateStartMsg >> return False
--  | otherwise = return True
--  where
--    dateEnd'     = zonedTimeToUTC $ fromJust $ dateEnd idea
--    dateStart'   = zonedTimeToUTC $ dateStart idea
--    hasDateEnd   = isJust $ dateEnd idea
--    closed       = "idea is closed"
--    tickerMsg    = "ticker not found"
--    outdated     = "idea is outdated"
--    dateStartMsg = "an idea should have its date start before its date end"
--
--setTicker :: Finder -> IdeaResponse -> IdeaResponse
--setTicker finder = select
--  finder
--  id
--  (\i -> let t = ticker i in i {ticker = T.takeWhile (== '.') t})
