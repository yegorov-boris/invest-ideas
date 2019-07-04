module Response
    ( Body(..)
    , Handler
    , parseCustomField
    ) where

import Data.Text (Text)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Aeson (FromJSON, Object, (.:))
import Data.Aeson.Types (Parser)
import Network.Http.Client (Response)
import Data.ByteString.UTF8 (ByteString)
import System.IO.Streams (InputStream)

class FromJSON b => Body b where
  success :: b -> Bool

type Handler a = Response -> InputStream ByteString -> IO a

parseCustomField :: (FromJSON a, Read a) => Object
                                            -> (String -> Maybe a)
                                            -> String
                                            -> Text
                                            -> Parser a
parseCustomField o parser type' name = o .: name >>= maybe fail' return . parser
  where
    fail' = fail $ printf "\"%s\" is expected to be %s" name type'
