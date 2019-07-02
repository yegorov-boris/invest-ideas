module Response
    ( Body(..)
    , Handler
    ) where

import Data.Aeson (FromJSON)
import Network.Http.Client (Response)
import Data.ByteString.UTF8 (ByteString)
import System.IO.Streams (InputStream)

class FromJSON b => Body b where
  success :: b -> Bool

type Handler a = Response -> InputStream ByteString -> IO a
