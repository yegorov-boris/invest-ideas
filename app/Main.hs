{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Control.Monad.Log as L
import Control.Monad.Log.Label (Label(..))
import Text.Printf (printf)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.Async (mapConcurrently_)
import Flags.Flags (parseCliFlags)
import qualified Brokers.Pipe as BrokersPipe
import qualified Ideas.Pipe as IdeasPipe

main :: IO ()
main = do
  logger <- L.makeDefaultLogger
    L.simpleTimeFormat'
    (L.LogStdout 4096)
    L.levelDebug
    (Label "main")

  (return $ Left "foo") >>= either
--  runExceptT parseCliFlags >>= either
    (\msg -> L.runLogTSafe logger $ do
      L.error $ T.pack $ printf "failed to parse CLI flags: %s" (msg::String)
    )
    (\cf -> mapConcurrently_ ($ cf) pipes)
  where
    pipes = [
        BrokersPipe.runFetcher
--      , IdeasPipe.runFetcher
      ]
