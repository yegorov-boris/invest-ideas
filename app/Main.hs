module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Control.Applicative (empty)
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

main :: IO ()
main = do
  tid <- myLogThreadId
  logger <- L.makeDefaultLogger
    L.simpleTimeFormat'
    (L.LogStdout 4096)
    L.levelDebug
    tid
  cf <- runExceptT parseCliFlags >>= either
    (\msg -> L.runLogTSafe logger $ L.error $ T.pack $ printf
      "failed to parse CLI flags: %s" msg) -- (msg::String))
    return
  let ctx = Context {flags = cf, logger = logger}
  mapConcurrently_ (\pipe -> runReaderT pipe ctx) pipes
  where
    pipes = [
        B.pipe
--      , I.pipe
      ]
