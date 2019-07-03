module Common
    ( Context(..)
    ) where

import Flags.Flags (CliFlags(..))
import Response (Handler)

data Context a = Context {
    flags       :: CliFlags
  , url         :: String
  , httpHandler :: Handler a
  }
