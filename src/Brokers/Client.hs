{-# LANGUAGE DeriveGeneric #-}

module Brokers.Client
    (fetch
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
--import Data.Time.LocalTime (ZonedTime)
import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString, fromString)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, handle)
import Control.Conditional (if')
import Control.Concurrent (MVar, newEmptyMVar, readMVar, tryPutMVar, forkIO, threadDelay)
import Control.Error (ExceptT, hoistEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import System.IO.Streams (InputStream)
import Flags (CliFlags, ideasURL, token, httpTimeout, httpMaxAttempts)
import Utils (printWrap)
import Brokers.Broker (Broker)

data Body = Body {
    success :: Bool
  , results :: [Broker]
  } deriving (Generic, Show)

instance FromJSON Body

fetch :: CliFlags -> IO ()
fetch cf = attemptFetch cf 1

attemptFetch :: CliFlags -> Int -> IO ()
attemptFetch cf currentAttempt = do
  m <- newEmptyMVar
  forkIO $ handle (onErr currentAttempt) (get url $ handleResponse m)
  forkIO (threadDelay (httpTimeout cf) >> tryPutMVar m False >> return ())
  ok <- readMVar m
  if'
    (ok || (currentAttempt == maxAttempts))
    (return ())
    (attemptFetch cf (succ currentAttempt))
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)
    maxAttempts = httpMaxAttempts cf
    handleResponse m response inputStream =
      (runExceptT $ responseHandler m currentAttempt response inputStream)
      >>= either putStrLn (putStrLn . show)

responseHandler :: MVar Bool -> Int -> Response -> InputStream ByteString -> ExceptT String IO [Broker]
responseHandler m currentAttempt response inputStream = do
  liftIO $ printWrap "started fetching brokers, attempt " currentAttempt
  allowed <- liftIO $ tryPutMVar m True
  statusCode <- hoistEither $ if'
    allowed
    (Right $ getStatusCode response)
    (Left $ "failed to fetch brokers: timed out, attempt " ++ show currentAttempt)
  hoistEither $ if'
      (statusCode == 200)
      (Right ())
      (Left $ "failed to fetch brokers, attempt " ++ show currentAttempt ++ ": status code " ++ show statusCode)
  body <- liftIO (jsonHandler response inputStream :: IO Body)
  brokers <- hoistEither $ if'
    (success body == True)
    (Right $ results body)
    (Left $ "body.success is false, attempt " ++ show currentAttempt)
  liftIO $ printWrap "finished fetching brokers, attempt " currentAttempt
  return brokers

onErr :: Int -> SomeException -> IO ()
onErr currentAttempt = printWrap ("failed to fetch brokers, attempt " ++ show currentAttempt ++ ": ")
