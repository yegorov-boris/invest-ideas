module Main where

import Control.Error (runExceptT)
import Control.Concurrent.Async (mapConcurrently_)
import Flags.Flags (parseCliFlags)
import qualified Brokers.Pipe as BrokersPipe
import qualified Ideas.Pipe as IdeasPipe
import Utils (printWrap)

main :: IO ()
main = runExceptT parseCliFlags >>= either
  (printWrap "failed to parse CLI flags: ")
  (\cf -> mapConcurrently_ ($ cf) [BrokersPipe.runFetcher, IdeasPipe.runFetcher])
