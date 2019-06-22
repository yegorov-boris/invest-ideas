module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString, fromString)
import Control.Exception (SomeException, handle)
import Control.Conditional (if')
import Control.Concurrent (threadDelay)
import Control.Error (ExceptT(..), hoistEither, runExceptT)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import System.IO.Streams (InputStream)
import Control.Concurrent.Async (race)
import Flags.Flags (CliFlags(..))
import Utils (printWrap)
import Brokers.Response (Body(..), BrokerResponse)

fetch :: CliFlags -> IO (Maybe [BrokerResponse])
fetch cf = doFetch cf 1

doFetch :: CliFlags -> Int -> IO (Maybe [BrokerResponse])
doFetch cf currentAttempt =
  handle (onErr currentAttempt) (runExceptT $ attemptFetch cf 1)
  >>= either
    (putStrLn >=> (\_ -> if currentAttempt < maxAttempts then tryAgain else return Nothing))
    (return . Just)
  where
    tryAgain = doFetch cf $ succ currentAttempt
    maxAttempts = httpMaxAttempts cf

attemptFetch :: CliFlags -> Int -> ExceptT String IO [BrokerResponse]
attemptFetch cf currentAttempt = do
  result <- ExceptT $ race
    (
      threadDelay (httpTimeout cf)
      >> return ("failed to fetch brokers: timed out, attempt " ++ show currentAttempt)
    )
    (get url $ responseHandler currentAttempt)
  (statusCode, body) <- hoistEither result
  liftIO $ printWrap "started fetching brokers, attempt " currentAttempt
  hoistEither $ if'
    (statusCode == 200)
    (Right ())
    (Left $ "failed to fetch brokers, attempt " ++ show currentAttempt ++ ": status code " ++ show statusCode)
  brokers <- hoistEither $ if'
    (success body == True)
    (Right $ results body)
    (Left $ "body.success is false, attempt " ++ show currentAttempt)
  liftIO $ printWrap "finished fetching brokers, attempt " currentAttempt
  return brokers
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)

responseHandler :: Int -> Response -> InputStream ByteString -> IO (Either String (Int, Body))
responseHandler currentAttempt response inputStream = handle
  (onErr currentAttempt)
  (fmap (Right . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response

onErr :: Int -> SomeException -> IO (Either String a)
onErr currentAttempt e =
  return $ Left $ "failed to fetch brokers, attempt " ++ show currentAttempt ++ ": " ++ show e
