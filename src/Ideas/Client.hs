module Brokers.Client
    (fetch
    ) where

import Data.ByteString.UTF8 (ByteString)
import System.IO.Streams (InputStream)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Conditional (if')
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan)
import System.Timeout (timeout)
import Control.Exception (handle)
import Flags.Flags (CliFlags(..))
import Ideas.Response (Body(..), IdeaResponse)
import Client (attemptFetch, url)
import Utils (printWrap, defaultErrorHandler)

limit :: Int
limit = 100

fetch :: CliFlags -> Chan (Maybe [IdeaResponse]) -> IO ()
fetch cf ideasCh = do
  forkIO $ worker cf 0 ideasCh
  forkIO $ worker cf limit ideasCh
  writeChan $ ideasCh Nothing

worker :: CliFlags -> Int -> Chan (Maybe [IdeaResponse]) -> IO ()
worker cf offset ideasCh = attemptFetch (httpMaxAttempts cf) (doFetch cf offset) >>=
  maybe
    return ()
    (\ideas -> if'
      null ideas
      return ()
      (writeChan ideasCh $ Just ideas) >> worker cf (offset + limit) ideasCh
    )

doFetch :: CliFlags -> Int -> Int -> IO (Maybe [IdeaResponse])
doFetch cf offset currentAttempt = handle
  (defaultErrorHandler $ "failed to fetch brokers, attempt " ++ show currentAttempt ++ ": ")
  (timeout (httpTimeout cf) $ runMaybeT $ fetcher cf offset currentAttempt) >>= return . join

fetcher :: CliFlags -> Int -> Int -> MaybeT IO [IdeaResponse]
fetcher cf offset currentAttempt = do
  liftIO $ printWrap ("started fetching ideas, offset " ++ offset ++ ", attempt ") currentAttempt
  (statusCode, body) <- MaybeT $ get (url cf [offset, limit]) $ responseHandler currentAttempt
  if'
    statusCode == 200
    return $ Just ()
    let
      msg = "failed to fetch ideas, status code " ++ statusCode ++ ", offset " ++ offset ++ ", attempt "
    in
      liftIO $ Nothing <$ printWrap msg currentAttempt
  ideas <- if'
    success body == True
    return $ results body
    let
      msg = "failed to fetch ideas because body.success = false, offset " ++ offset ++ ", attempt "
    in
      liftIO $ Nothing <$ printWrap msg currentAttempt
  liftIO $ printWrap ("finished fetching ideas, offset " ++ offset ++ ", attempt ") currentAttempt
  return ideas

responseHandler :: Int -> Response -> InputStream ByteString -> IO (Maybe (Int, Body))
responseHandler currentAttempt response inputStream = handle
  Nothing <$ (defaultErrorHandler $ "failed to process brokers response, attempt " ++ show currentAttempt ++ ": ")
  (fmap (Just . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response
