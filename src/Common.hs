module Common
    ( Context(..)
    , Cache
    , Pipe
    , askFlags
    ) where

import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Log (Logger)
import Control.Monad.Log.Label (Label)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Flags.Flags (CliFlags(..))
import Response (Handler)

data Context = Context {
    flags  :: CliFlags
  , logger :: Logger Label
  }

type Pipe a b = ReaderT Context IO b

askFlags :: (CliFlags -> b) -> Pipe a b
askFlags = (asks $) . (. flags)

type Cache = HashSet.Set T.Text
