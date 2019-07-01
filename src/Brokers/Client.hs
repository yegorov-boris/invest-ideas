module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString)
import Control.Exception (handle)
import Control.Conditional (select)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad ((>=>), mzero, when)
import Control.Monad.IO.Class (liftIO)
import System.IO.Streams (InputStream)
import System.Timeout (timeout)
import Flags.Flags (CliFlags(..))
import Utils (printWrap, defaultErrorHandler)
import Brokers.Response (Body(..), BrokerResponse)
import Client (url)

fetch :: CliFlags -> IO (Maybe [BrokerResponse])
fetch cf = doFetch cf 1

-- TODO: find a lib for retry
doFetch :: CliFlags -> Int -> IO (Maybe [BrokerResponse])
doFetch cf currentAttempt = handle
  (onFailure >=> \_ -> nextAction)
  ((runMaybeT $ fetcher cf 1) >>= maybe nextAction (return . Just))
  where
    tryAgain = doFetch cf $ succ currentAttempt
    nextAction = if currentAttempt < maxAttempts then tryAgain else return Nothing
    maxAttempts = httpMaxAttempts cf
    onFailure = defaultErrorHandler $ "failed to fetch brokers, attempt " ++ show currentAttempt ++ ": "

fetcher :: CliFlags -> Int -> MaybeT IO [BrokerResponse]
fetcher cf currentAttempt = do
  liftIO $ printWrap "started fetching brokers, attempt " currentAttempt
  (statusCode, body) <- MaybeT $ get (url cf "/brokers" 0 100) $ responseHandler currentAttempt
  let statusMsg = "failed to fetch brokers, status code " ++ show statusCode ++ ", attempt "
  when (statusCode /= 200) ((liftIO $ printWrap statusMsg currentAttempt) >> mzero)
  let bodyMsg = "failed to fetch brokers because body.success = false, attempt "
  when (success body /= True) ((liftIO $ printWrap bodyMsg currentAttempt) >> mzero)
  liftIO $ printWrap "finished fetching brokers, attempt " currentAttempt
  select null (\_ -> mzero) return (results body)

responseHandler :: Int -> Response -> InputStream ByteString -> IO (Maybe (Int, Body))
responseHandler currentAttempt response inputStream = handle
  ((Nothing <$) . (defaultErrorHandler msg))
  (fmap (Just . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response
    msg = "failed to process brokers response, attempt " ++ show currentAttempt ++ ": "
