module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (get, getStatusCode)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, handle)
import Control.Conditional (if')
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, forkIO, threadDelay, killThread)
import Flags (CliFlags, ideasURL, token, httpTimeout, httpMaxAttempts)
import Utils (printWrap)

-- TODO: read flags
-- TODO: chain conditions
fetch :: CliFlags -> IO ()
fetch cf = attemptFetch cf 1

attemptFetch :: CliFlags -> Int -> IO ()
attemptFetch cf currentAttempt = do
  m <- newMVar False
  id <- forkIO $ doFetch cf m currentAttempt
  threadDelay $ httpTimeout cf
  success <- takeMVar m
  if'
    (success == True)
    (return ())
    (killThread id >> if'
      (currentAttempt < httpMaxAttempts cf)
      (attemptFetch cf (succ currentAttempt))
      (return ()))

doFetch :: CliFlags -> MVar Bool -> Int -> IO ()
doFetch cf m currentAttempt = do
  putStrLn $ "started fetching brokers, attempt " ++ show currentAttempt
  handle (onErr currentAttempt) (get url processStatusCode)
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)
    processStatusCode response inputStream = do
      takeMVar m
      putMVar m True
      let statusCode = getStatusCode response in if'
        (statusCode == 200)
        (putStrLn $ "finished fetching brokers, attempt " ++ show currentAttempt)
        (printWrap ("failed to fetch brokers, attempt " ++ show currentAttempt ++ ": status code ") statusCode)

onErr :: Int -> SomeException -> IO ()
onErr currentAttempt = printWrap ("failed to fetch brokers, attempt " ++ show currentAttempt ++ ": ")

--import Data.Time.LocalTime (ZonedTime)
--
--data Broker = Broker { externalID :: String
--                     , source :: String
--                     , name :: String
--                     , rating :: String
--                     , ideasCount :: Int
--                     , ideasPositive :: Int
--                     , description :: String
--                     , accuracy :: Double
--                     , profitableIdeasAvgYield :: Double
--                     , totalProfitableIdeas :: Int
--                     , unprofitableIdeasAvgYield :: Double
--                     , totalUnprofitableIdeas :: Int
--                     , bestIdeaExternalID :: String
--                     , newIdeasPerMonth :: Int
--                     , ideaAvgDaysLong :: Int
--                     , specializationResumeAsset :: String
--                     , specializationResumeCurrency :: String
--                     , specializationResumeDescription :: String
--                     , createdAt :: ZonedTime
--                     , updatedAt :: ZonedTime
--                     , isDeleted :: Bool
--                     , isVisibleMM :: Bool
--                     , isVisibleWM :: Bool
--                     }
