module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString)
import Text.Printf (printf)
import Control.Exception (handle)
import Control.Conditional (select)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad ((>=>), mzero, when)
import Control.Monad.IO.Class (liftIO)
import System.IO.Streams (InputStream)
import System.Timeout (timeout)
import Flags.Flags (CliFlags(..))
import Utils (defaultErrorHandler)
import Brokers.Response (Body(..), BrokerResponse)
import Client (url)

fetch :: CliFlags -> IO (Maybe [BrokerResponse])
fetch cf = doFetch cf 1

-- TODO: find a lib for retry; Client.attemptFetch
doFetch :: CliFlags -> Int -> IO (Maybe [BrokerResponse])
doFetch cf currentAttempt = handle
  (onFailure >=> \_ -> nextAction)
  ((runMaybeT $ fetcher cf 1) >>= maybe nextAction (return . Just))
  where
    tryAgain = doFetch cf $ succ currentAttempt
    nextAction = if currentAttempt < maxAttempts then tryAgain else return Nothing
    maxAttempts = httpMaxAttempts cf
    onFailure = defaultErrorHandler $ printf "failed to fetch brokers, attempt %d:" currentAttempt

fetcher :: CliFlags -> Int -> MaybeT IO [BrokerResponse]
fetcher cf currentAttempt = do
  liftIO $ printf "started fetching brokers, attempt %d" currentAttempt
  (statusCode, body) <- MaybeT $ get (url cf "/brokers" 0 100) $ responseHandler currentAttempt
  let statusMsg = (printf "failed to fetch brokers, status code %d, attempt %d" statusCode currentAttempt)::String
  when (statusCode /= 200) ((liftIO $ putStrLn statusMsg) >> mzero)
  let bodyMsg = (printf "failed to fetch brokers because body.success = false, attempt %d" currentAttempt)::String
  when (success body /= True) ((liftIO $ putStrLn bodyMsg) >> mzero)
  liftIO $ printf "finished fetching brokers, attempt %d" currentAttempt
  select null (\_ -> mzero) return (results body)

responseHandler :: Int -> Response -> InputStream ByteString -> IO (Maybe (Int, Body))
responseHandler currentAttempt response inputStream = handle
  ((Nothing <$) . (defaultErrorHandler msg))
  (fmap (Just . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response
    msg = (printf "failed to process brokers response, attempt %d" currentAttempt)::String
