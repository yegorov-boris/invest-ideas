module Common
    ( Context(..)
    , Cache
    ) where

import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags(..))
import Response (Handler)

data Context a = Context {
    flags       :: CliFlags
  , url         :: String
  , httpHandler :: Handler a
  }

type Cache = HashSet.Set T.Text
