module Client
    ( attemptFetch
    , url
    ) where

import Control.Conditional (if')
import Data.ByteString.UTF8 (ByteString, fromString)
import Flags.Flags (CliFlags(..))

attemptFetch :: Int -> (Int -> IO (Maybe a)) -> IO (Maybe a)
attemptFetch maxAttempts fetcher = doAttemptFetch maxAttempts 1 fetcher

doAttemptFetch :: Int -> Int -> (Int -> IO (Maybe a)) -> IO (Maybe a)
doAttemptFetch maxAttempts currentAttempt fetcher = fetcher currentAttempt >>= maybe
  if'
    currentAttempt == maxAttempts
    return Nothing
    doAttemptFetch maxAttempts (succ currentAttempt) fetcher
  return . Just

url :: CliFlags -> [String] -> ByteString
url cf params = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf) ++
