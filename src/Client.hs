module Client
    ( Context(..)
    , attemptFetch
    , makeURL
    ) where

import Data.Maybe (isNothing)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.RWS.Lazy (RWST, runRWST, liftCatch)
import Control.Monad.Trans.Reader (ReaderT, asks, mapReaderT)
import Data.Either (isLeft)
import Control.Retry (limitRetries, retrying, rsIterNumber)
import Text.Printf (printf)
import Network.Http.Client (Response, get, getStatusCode, jsonHandler)
import Control.Conditional (if', select)
import Data.ByteString.UTF8 (ByteString, fromString)
import System.IO.Streams (InputStream)
import Control.Monad (mzero, (>=>), when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Exception (handle)
import System.Timeout (timeout)
import Flags.Flags (CliFlags(..))
import Utils (defaultErrorHandler)
import Response (Body(..), Handler)

data Context a = Context {
    flags :: CliFlags
  , url   :: String
  , httpHandler :: Handler a
  }

attemptFetch :: Body b => ReaderT (Context b) IO (Maybe String)
attemptFetch = do
  maxAttempts <- asks $ httpMaxAttempts . flags
  retrying
    (limitRetries $ maxAttempts - 1)
    (const $ return . isNothing)
    (const $ mapReaderT runExceptT fetcher >>= onResult)
  where
    onResult = either
      (liftIO . putStrLn >=> (const $ return Nothing))
      (return . Just)






--doAttemptFetch cf name offset 1

--doAttemptFetch :: Body b => CliFlags -> String -> (Handler b) -> Int -> IO (Maybe b)


--doAttemptFetch cf url handler currentAttempt = handle
--  ((Nothing <$) . onFailure)
--  (runMaybeT $ fetcher cf url handler currentAttempt)
--  where
--  TODO: pattern-match the exception
--printf "failed to process %s response, attempt %d:" url currentAttempt
--    onFailure = defaultErrorHandler $ printf "failed to fetch %s, attempt %d:" url currentAttempt

fetcher :: Body b => ReaderT (Context b) (ExceptT String IO) String
fetcher = do
  liftIO $ putStrLn "fetcher"
--  lift $ throwE "42"
  return "42"




--  liftIO $ printf "started fetching %s, offset %d, attempt %d" name offset currentAttempt
--  (statusCode, body) <- MaybeT $ timeout (httpTimeout cf) $ get url $ responseHandler handler
--  let statusF = "failed to fetch %s, status code %d, offset %d, attempt %d"
--  let statusMsg = (printf statusF name statusCode offset currentAttempt)::String
--  when (statusCode /= 200) ((liftIO $ putStrLn statusMsg) >> mzero)
--  let bodyF = "failed to fetch %s because body.success = false, offset %d, attempt %d"
--  let bodyMsg = (printf bodyF name offset currentAttempt)::String
--  let (success, results) = if'
--    (name == "ideas")
--    (I.success body, I.results body)
--    (B.success body, B.results body)
--  when (success body /= True) ((liftIO $ putStrLn bodyMsg) >> mzero)
--  liftIO $ printf "finished fetching %s, offset %d, attempt %d" name offset currentAttempt
--  select null (\_ -> mzero) return (results body)

responseHandler :: Body b => (Handler b) -> Response -> InputStream ByteString -> IO (Int, b) -- TODO: Cont
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
