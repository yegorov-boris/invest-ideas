module Common
    ( Context(..)
    , Cache
    , Pipe
    , askFlags
    ) where

import Control.Monad.Trans.Reader (ReaderT, asks)
import qualified Control.Monad.Log as L
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags(..))
import Response (Handler)

data Context env = Context {
    flags  :: CliFlags
  , logger :: L.Logger env
  }

type Pipe a b = ReaderT (Context a) IO b

askFlags :: (CliFlags -> b) -> Pipe a b
askFlags = (asks $) . (. flags)

type Cache = HashSet.Set T.Text
