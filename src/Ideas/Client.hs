module Ideas.Client
    (fetch
    ) where

import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString)
import System.IO.Streams (InputStream)
import Control.Monad (join, mzero, (>=>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Conditional (if')
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (Chan, writeChan)
import System.Timeout (timeout)
import Control.Exception (handle)
import Flags.Flags (CliFlags(..))
import Ideas.Response (Body(..), IdeaResponse)
import Client (attemptFetch, url)
import Utils (printWrap, defaultErrorHandler)

limit = 100 :: Int

fetch :: CliFlags -> Chan [IdeaResponse] -> IO ()
fetch cf ideasCh = do
  mapConcurrently_ (worker cf ideasCh) [0, limit]
  putStrLn "finished fetching ideas"

worker :: CliFlags -> Chan [IdeaResponse] -> Int -> IO ()
worker cf ideasCh offset = attemptFetch (httpMaxAttempts cf) (doFetch cf offset) >>= maybe
  mempty
  (writeChan ideasCh >=> (\_ -> worker cf ideasCh $ offset + 2 * limit))

doFetch :: CliFlags -> Int -> Int -> IO (Maybe [IdeaResponse])
doFetch cf offset currentAttempt = handle
  ((Nothing <$) . (defaultErrorHandler $ "failed to fetch ideas, attempt " ++ show currentAttempt ++ ": ")) -- TODO: strings interpolation
  (join <$> (timeout (httpTimeout cf) $ runMaybeT $ fetcher cf offset currentAttempt))

fetcher :: CliFlags -> Int -> Int -> MaybeT IO [IdeaResponse]
fetcher cf offset currentAttempt = do
  liftIO $ printWrap ("started fetching ideas, offset " ++ show offset ++ ", attempt ") currentAttempt
  (statusCode, body) <- MaybeT $ get (url cf "/ideas" offset limit) $ responseHandler currentAttempt
  if' -- TODO: when
    (statusCode == 200)
    (return ())
    (let
      msg = "failed to fetch ideas, status code " ++ show statusCode ++ ", offset " ++ show offset ++ ", attempt "
    in
      (liftIO $ printWrap msg currentAttempt) >> mzero)
  ideas <- if'
    (success body == True)
    (return $ results body)
    (let
      msg = "failed to fetch ideas because body.success = false, offset " ++ show offset ++ ", attempt "
    in
      (liftIO $ printWrap msg currentAttempt) >> mzero)
  liftIO $ printWrap ("finished fetching ideas, offset " ++ show offset ++ ", attempt ") currentAttempt
  if' (null ideas) mzero (return ideas)

responseHandler :: Int -> Response -> InputStream ByteString -> IO (Maybe (Int, Body))
responseHandler currentAttempt response inputStream = handle
  ((Nothing <$) . (defaultErrorHandler $ "failed to process ideas response, attempt " ++ show currentAttempt ++ ": "))
  (fmap (Just . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response
