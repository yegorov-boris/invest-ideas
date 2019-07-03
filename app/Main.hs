module Main where

import Text.Printf (printf)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.Async (mapConcurrently_)
import Flags.Flags (parseCliFlags)
import qualified Brokers.Pipe as BrokersPipe
import qualified Ideas.Pipe as IdeasPipe

main :: IO ()
main = runExceptT parseCliFlags >>= either
  (printf "failed to parse CLI flags: %s")
  (\cf -> mapConcurrently_ ($ cf) pipes)
  where
    pipes = [
        BrokersPipe.runFetcher
--      , IdeasPipe.runFetcher
      ]
