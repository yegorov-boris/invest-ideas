module Ideas.Validator
    ( validate
    ) where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Control.Conditional (if', select)
import Ideas.Response (IdeaResponse(..))

validate :: HashSet.Set T.Text -> IdeaResponse -> Either String IdeaResponse
validate stocksCache idea = do
if !ideaItem.DateEnd.IsZero() && ideaItem.DateEnd.Before(time.Now()) {
		ideaItem.IsOpen = false
	}
  i <- if' (isOpen idea) (Right $ idea {ticker = t}) (Left "idea is not open")
  if' (not (T.null t) && ok t) (Right i) (Left "ticker not found")
  where
    ok = (`HashSet.member` stocksCache)
    t  = select ok id (T.takeWhile (== '.')) (ticker idea)
