module Client
    ( Context(..)
    , fetch
    , makeURL
    ) where

import Control.Monad ((>=>))
import Control.Applicative (empty, (<|>))
import Data.Maybe (isNothing, fromJust)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (ReaderT, asks, mapReaderT)
import Text.Printf (printf)
import Network.Http.Client (get, getStatusCode)
import Control.Conditional (if')
import Data.ByteString.UTF8 (fromString)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Exception (SomeException, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Timeout (timeout)
import Flags.Flags (CliFlags(..))
import Response (Body(..))
import Common (Context(..))

fetch :: Body b => ReaderT (Context b) IO b
fetch = do
  maxAttempts <- asks $ httpMaxAttempts . flags
  u <- asks url
  liftIO $ printf "started fetching %s\n" u
  foldr (<|>) empty $ map attemptFetch [1..maxAttempts]

attemptFetch :: Body b => Int -> ReaderT (Context b) IO b
attemptFetch i = do
  u <- asks url
  (mapReaderT runExceptT $ catch fetcher onErr) >>= liftIO . either
    (printf "failed to fetch %s, attempt %d: %s\n" u i >=> const empty)
    (\result -> printf "finished fetching %s, attempt %d\n" u i >> return result)

type Fetcher a = ReaderT (Context a) (ExceptT String IO) a

fetcher :: Body b => Fetcher b
fetcher = do
  t <- asks $ httpTimeout . flags
  u <- asks url
  handler <- asks httpHandler
  result <- (liftIO $ timeout t $ get
      (fromString u)
      (\response inputStream -> handler response inputStream >>= return . (,) response)
    )
  when (isNothing result) (lift $ throwE "canceled by timeout")
  (response, body) <- return $ fromJust result
  let statusCode = getStatusCode response
  when (statusCode /= 200) (lift $ throwE $ printf "status code %d" statusCode)
  if' (success body == True) (return body) (lift $ throwE "body.success = false")

onErr :: Body b => SomeException -> Fetcher b -- TODO: pattern-match the exception
onErr = lift . throwE . displayException

limit = 100 :: Int

makeURL :: CliFlags -> String -> Int -> String -- TODO: find a lib to construct URLs
makeURL cf path offset = printf
  "%s%s?api_key=%s&offset=%d&limit=%d"
  (ideasURL cf)
  path
  (token cf)
  offset
  limit
