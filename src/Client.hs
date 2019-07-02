module Client
    ( Context(..)
    , attemptFetch
    , makeURL
    ) where

import Data.Maybe (isNothing)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (ReaderT, asks, mapReaderT)
import Control.Retry (limitRetries, retrying, rsIterNumber)
import Text.Printf (printf)
import Network.Http.Client (Response, get, getStatusCode)
import Control.Conditional (if')
import Data.ByteString.UTF8 (ByteString, fromString)
import System.IO.Streams (InputStream)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Exception (SomeException, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Timeout (timeout)
import Flags.Flags (CliFlags(..))
import Response (Body(..), Handler)

data Context a = Context {
    flags       :: CliFlags
  , url         :: String
  , httpHandler :: Handler a
  }

attemptFetch :: Body b => ReaderT (Context b) IO (Maybe b)
attemptFetch = do
  maxAttempts <- asks $ httpMaxAttempts . flags
  u <- asks url
  liftIO $ printf "started fetching %s\n" u
  retrying
    (limitRetries $ maxAttempts - 1)
    (const $ return . isNothing)
    (\rs -> do
      let i = rsIterNumber rs
      result <- mapReaderT runExceptT (catch fetcher onErr)
      liftIO $ either
        (printf "failed to fetch %s, attempt %d: %s\n" u i)
        (const $ printf "finished fetching %s, attempt %d\n" u i)
        result
      return $ either (const Nothing) Just result
    )

type Fetcher a = ReaderT (Context a) (ExceptT String IO) b

fetcher :: Body b => Fetcher b
fetcher = do
  t <- asks $ httpTimeout . flags
  u <- asks url
  handler <- asks httpHandler
  (statusCode, body) <- (liftIO $ timeout t $ get (fromString u) handler) >>= maybe
    (throwE "canceled by timeout")
    return
  when (statusCode /= 200) (throwE $ printf "status code %d" statusCode)
  if' (success body == True) (return body) (throwE "body.success = false")

onErr :: Body b => SomeException -> Fetcher b -- TODO: pattern-match the exception
onErr = lift . throwE . displayException

responseHandler :: Body b => (Handler b) -> Response -> InputStream ByteString -> IO (Int, b)
responseHandler handler response inputStream = do
  body <- handler response inputStream
  return (getStatusCode response, body)

limit = 100 :: Int

makeURL :: CliFlags -> String -> Int -> String
makeURL cf path offset = printf
  "%s%s?api_key=%s&offset=%d&limit=%d"
  (ideasURL cf)
  path
  (token cf)
  offset
  limit
