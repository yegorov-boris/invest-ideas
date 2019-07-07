module Client
    ( fetch
    ) where

import Data.Foldable (asum)
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
import Response (Body(..), Handler)
import Common (Context(..), Pipe)
import Utils (logInfo, logError)

fetch :: Body a => String -> Handler a -> Pipe (Maybe a)
fetch url httpHandler = do
  logger' <- asks logger
  maxAttempts <- asks $ httpMaxAttempts . flags
  logInfo' logger' $ printf "started fetching %s" url
  asum $ map (attemptFetch url httpHandler) [1..maxAttempts]

attemptFetch :: Body a => String -> Handler a -> Int -> Pipe (Maybe a)
attemptFetch url httpHandler i = do
  logger' <- asks logger
  eitherBody <- mapReaderT runExceptT $ catch (fetcher url httpHandler) onErr
  either (onFail logger') (onSuccess logger') eitherBody
  either (const $ return Nothing) (return . Just) eitherBody
  where
    onSuccess l = const $ do
      logInfo' l $ printf "finished fetching %s, attempt %d" url i
    onFail l msg = do
      logError' l $ printf "failed to fetch %s, attempt %d: %s" url i msg

type Fetcher a b = ReaderT Context (ExceptT String IO) b

fetcher :: Body b => String -> Handler b -> Fetcher a b
fetcher url httpHandler = do
  t <- asks $ httpTimeout . flags
  result <- (liftIO $ timeout t $ get
      (fromString url)
      (\response inputStream -> httpHandler response inputStream >>= return . (,) response)
    )
  when (isNothing result) (lift $ throwE "canceled by timeout")
  (response, body) <- return $ fromJust result
  let statusCode = getStatusCode response
  when (statusCode /= 200) (lift $ throwE $ printf "status code %d" statusCode)
  if' (success body == True) (return body) (lift $ throwE "body.success = false")

onErr :: Body b => SomeException -> Fetcher a b
onErr = lift . throwE . displayException

label = "client"::String
logInfo' = logInfo label
logError' = logError label
