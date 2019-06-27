module Client
    ( attemptFetch
    , url
    ) where

import Control.Conditional (if')
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.List (intercalate)
import Flags.Flags (CliFlags(..))

attemptFetch :: Int -> (Int -> IO (Maybe a)) -> IO (Maybe a)
attemptFetch maxAttempts fetcher = doAttemptFetch maxAttempts 1 fetcher

doAttemptFetch :: Int -> Int -> (Int -> IO (Maybe a)) -> IO (Maybe a)
doAttemptFetch maxAttempts currentAttempt fetcher =
  fetcher currentAttempt >>= maybe tryAgain (return . Just)
  where
    tryAgain = if'
      (currentAttempt == maxAttempts)
      (return Nothing)
      (doAttemptFetch maxAttempts (succ currentAttempt) fetcher)

-- TODO: interpolate strings
url :: CliFlags -> String -> Int -> Int -> ByteString
url cf path offset limit = fromString $ intercalate "" [
    ideasURL cf
  , path
  , "?api_key="
  , token cf
  , "&offset="
  , show offset
  , "&limit="
  , show limit
  ]
