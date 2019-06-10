module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (get, getStatusCode)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, handle)
import Control.Conditional (if')
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar, forkIO, threadDelay, killThread)
import Flags (CliFlags, ideasURL, token, httpTimeout)
import Utils (printWrap)

-- TODO: retry
fetch :: CliFlags -> IO ()
fetch cf = do
  m <- newMVar False
  id <- forkIO $ attemptFetch cf m
  threadDelay $ httpTimeout cf
  success <- takeMVar m
  if'
    (success == True)
    (putStrLn "foo")
    (killThread id >> putStrLn "bar")

attemptFetch :: CliFlags -> MVar Bool -> IO ()
attemptFetch cf m = do
  putStrLn "started fetching brokers"
  handle onErr (get url processStatusCode)
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)
    processStatusCode response inputStream = do
      takeMVar m
      putMVar m True
      let statusCode = getStatusCode response in if'
        (statusCode == 200)
        (putStrLn "finished fetching brokers")
        (printWrap "failed to fetch brokers: status code " statusCode)

onErr :: SomeException -> IO ()
onErr = printWrap "failed to fetch brokers: "

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
