module Common
    ( Context(..)
    , Cache
    ) where

import qualified Control.Monad.Log as L
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags(..))
import Response (Handler)

data Context env = Context {
    flags  :: CliFlags
  , logger :: L.Logger env
  }

type Cache = HashSet.Set T.Text
