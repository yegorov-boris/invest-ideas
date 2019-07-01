module Ideas.Client
    (fetch
    ) where

import Text.Printf (printf)
import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Data.ByteString.UTF8 (ByteString)
import System.IO.Streams (InputStream)
import Control.Monad (join, mzero, (>=>), when)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Conditional (select)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (Chan, writeChan)
import System.Timeout (timeout)
import Control.Exception (handle)
import Flags.Flags (CliFlags(..))
import Ideas.Response (Body(..), IdeaResponse)
import Client (attemptFetch, url)
import Utils (defaultErrorHandler)

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
  ((Nothing <$) . (defaultErrorHandler $ printf "failed to fetch ideas, attempt %d:" currentAttempt))
  (join <$> (timeout (httpTimeout cf) $ runMaybeT $ fetcher cf offset currentAttempt))

-- TODO: dup
fetcher :: CliFlags -> Int -> Int -> MaybeT IO [IdeaResponse]
fetcher cf offset currentAttempt = do
  liftIO $ printf "started fetching ideas, offset %d, attempt %d" offset currentAttempt
  (statusCode, body) <- MaybeT $ get (url cf "/ideas" offset limit) $ responseHandler currentAttempt
  let statusF = "failed to fetch ideas, status code %d, offset %d, attempt %d"
  let statusMsg = (printf statusF statusCode offset currentAttempt)::String
  when (statusCode /= 200) ((liftIO $ putStrLn statusMsg) >> mzero)
  let bodyF = "failed to fetch ideas because body.success = false, offset %d, attempt %d"
  let bodyMsg = (printf bodyF offset currentAttempt)::String
  when (success body /= True) ((liftIO $ putStrLn bodyMsg) >> mzero)
  liftIO $ printf "finished fetching ideas, offset %d, attempt %d" offset currentAttempt
  select null (\_ -> mzero) return (results body)

-- TODO: dup
responseHandler :: Int -> Response -> InputStream ByteString -> IO (Maybe (Int, Body))
responseHandler currentAttempt response inputStream = handle
  ((Nothing <$) . (defaultErrorHandler msg))
  (fmap (Just . (,) statusCode) (jsonHandler response inputStream :: IO Body))
  where
    statusCode = getStatusCode response
    msg = printf "failed to process ideas response, attempt %d:" currentAttempt
