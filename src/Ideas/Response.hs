{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ideas.Response
    ( Body(..)
    , IdeaResponse(..)
    , Tag(..)
    ) where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.Text as T
import Data.Time.LocalTime (ZonedTime)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Utils (parseCustomTime)
import qualified Response as R

data Body = Body {
    success :: Bool
  , results :: [IdeaResponse]
  } deriving (Generic, Show)

instance FromJSON Body

instance R.Body Body
  where success = success

data IdeaResponse = IdeaResponse {
    externalID       :: Int
  , brokerExternalID :: Int
  , isOpen           :: Bool
  , horizon          :: Int
  , dateStart        :: ZonedTime
  , dateEnd          :: Maybe ZonedTime
  , priceStart       :: Double
  , price            :: Double
  , yield            :: Double
  , targetYield      :: Double
  , strategy         :: T.Text
  , title            :: T.Text
  , description      :: T.Text
  , isVisible        :: Bool
  , believe          :: Int
  , notBelieve       :: Int
  , expectedDateEnd  :: ZonedTime
  , ticker           :: T.Text
  , tag              :: Tag
  } deriving Show

instance FromJSON IdeaResponse where
  parseJSON = withObject "IdeaResponse" $ \o -> do
    let toDouble = R.parseCustomField o readMaybe "Double"
    let toDate = R.parseCustomField o parseCustomTime "DD.MM.YYYY"
    externalID       <- o .: "id"
    brokerExternalID <- o .: "broker_id"
    isOpen           <- o .: "is_open"
    horizon          <- o .: "horizon"
    dateStart        <- toDate "date_start"
    priceStart       <- o .: "price_start"
    price            <- o .: "price"
    yield            <- toDouble "yield"
    targetYield      <- toDouble "target_yield"
    strategy         <- o .: "strategy"
    title            <- o .: "title"
    description      <- o .: "description"
    isVisible        <- o .: "visibility"
    believe          <- o .: "believe"
    notBelieve       <- o .: "not_believe"
    ticker           <- o .: "ticker"
    expectedDateEnd  <- toDate "expected_date_end"
    tag              <- o .: "tags"

    dateEnd <- runMaybeT $
      (MaybeT $ o .:? "date_end")
      >>= maybe (fail "\"date_end\" is not a DD.MM.YYYY") return . parseCustomTime

    return IdeaResponse{..}

data Tag = Tag {
    jurisdiction :: T.Text
  , currency     :: T.Text
  } deriving (Generic, Show)

instance FromJSON Tag
