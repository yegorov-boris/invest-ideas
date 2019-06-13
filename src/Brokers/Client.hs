{-# LANGUAGE DeriveGeneric #-}

module Brokers.Client
    (fetch
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
--import Data.Time.LocalTime (ZonedTime)
import Network.Http.Client (get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, handle)
import Control.Conditional (if')
import Control.Concurrent (MVar, newEmptyMVar, readMVar, tryPutMVar, forkIO, threadDelay)
import Flags (CliFlags, ideasURL, token, httpTimeout, httpMaxAttempts)
import Utils (printWrap)

data Body = Body {
    success :: Bool
  , results :: [Broker]
  } deriving (Generic, Show)

instance FromJSON Body

data Broker = Broker {
  id :: Int
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
  } deriving (Generic, Show)

instance FromJSON Broker

fetch :: CliFlags -> IO ()
fetch cf = attemptFetch cf 1

attemptFetch :: CliFlags -> Int -> IO ()
attemptFetch cf currentAttempt = do
  m <- newEmptyMVar
  forkIO $ doFetch cf m currentAttempt
  forkIO (threadDelay (httpTimeout cf) >> tryPutMVar m False >> return ())
  success <- readMVar m
  if'
    (success || (currentAttempt == maxAttempts))
    (return ())
    (attemptFetch cf (succ currentAttempt))
  where
    maxAttempts = httpMaxAttempts cf

-- TODO: chain the nested conditions
doFetch :: CliFlags -> MVar Bool -> Int -> IO ()
doFetch cf m currentAttempt = do
  printWrap "started fetching brokers, attempt " currentAttempt
  handle (onErr currentAttempt) (get url processStatusCode)
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)
    processStatusCode response inputStream = do
      allowed <- tryPutMVar m True
      if'
        allowed
        (let statusCode = getStatusCode response in if'
          (statusCode == 200)
          (do
            printWrap "finished fetching brokers, attempt " currentAttempt
            printWrap "started parsing brokers response body, attempt " currentAttempt
            body <- jsonHandler response inputStream :: IO Body -- TODO: on err
            printWrap "finished parsing brokers response body, attempt " currentAttempt
            putStrLn $ show body
          )
          (printWrap ("failed to fetch brokers, attempt " ++ show currentAttempt ++ ": status code ") statusCode))
        (printWrap "failed to fetch brokers: timed out, attempt " currentAttempt)

onErr :: Int -> SomeException -> IO ()
onErr currentAttempt = printWrap ("failed to fetch brokers, attempt " ++ show currentAttempt ++ ": ")
