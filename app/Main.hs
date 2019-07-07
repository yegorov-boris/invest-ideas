module Main where

import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T
import qualified Control.Monad.Log as L
import Control.Monad.Log.Label (Label(..))
import Text.Printf (printf)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.Async (mapConcurrently_)
import Flags.Flags (parseCliFlags)
import qualified Brokers.Pipe as B
import qualified Ideas.Pipe as I
import Common (Context(..))

-- TODO: lint
main :: IO ()
main = do
  logger <- L.makeDefaultLogger
    L.simpleTimeFormat'
    (L.LogStdout 4096)
    L.levelDebug
    (Label $ T.pack "main")
  runExceptT parseCliFlags >>= either
    (\msg -> (L.runLogTSafe logger $ L.critical $ T.pack $ printf "failed to parse CLI flags: %s" msg) >> error "")
    (\cf -> mapConcurrently_ (\pipe -> runReaderT pipe Context {flags = cf, logger = logger}) pipes)
  where
    pipes = [
        B.pipe
      , I.pipe
      ]
