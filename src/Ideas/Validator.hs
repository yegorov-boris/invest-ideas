module Ideas.Validator
    ( validate
    ) where

import Data.HashSet (Set)
import qualified Data.Text as T
import qualified Ideas.Response (IdeaResponse(..))

-- TODO: implementation
validate :: Set T.Text -> IdeaResponse -> Either String IdeaResponse
validate stocksCache idea = Right idea
