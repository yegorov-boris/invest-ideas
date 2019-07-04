module Client
    ( Context(..)
    , fetch
    ) where

import Data.Foldable (asum)
import Control.Monad ((>=>))
import Control.Applicative (empty)
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
import Common (Context(..), Handler)

fetch :: Body b => String -> Handler b -> ReaderT (Context a) IO b
fetch url httpHandler = do
  logger' <- asks logger
  maxAttempts <- asks $ httpMaxAttempts . flags
  liftIO $ L.runLogT' logger' $ L.error $ T.pack $ printf
    "started fetching %s"
    url
  asum $ map (attemptFetch url httpHandler) [1..maxAttempts]

attemptFetch :: Body b => String -> Handler b -> Int -> ReaderT (Context a) IO b
attemptFetch url httpHandler i = do
  logger' <- asks logger
  let onSuccess = \body -> do
    L.runLogT' logger' $ L.error $ T.pack $ printf
      "finished fetching %s, attempt %d\n"
      url
      i
    return body
  let onFail = \msg -> do
    L.runLogT' logger' $ L.error $ T.pack $ printf
      "failed to fetch %s, attempt %d: %s\n"
      url
      i
      msg
    empty
  (mapReaderT runExceptT $ catch (fetcher url httpHandler) onErr) >>=
    liftIO . either onFail onSuccess


type Fetcher a b = String -> Handler b -> ReaderT (Context a) (ExceptT String IO) b

fetcher :: Body b => Fetcher a b
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

onErr :: Body b => SomeException -> Fetcher b
onErr = lift . throwE . displayException
