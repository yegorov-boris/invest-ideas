{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ideas.Response
    ( Body(..)
    , IdeaResponse(..)
    ) where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T
import Data.Time.LocalTime (ZonedTime)

data Body = Body {
    success :: Bool
  , results :: [IdeaResponse]
  } deriving (Generic, Show)

instance FromJSON Body

data IdeaResponse = IdeaResponse {
    externalID       :: Int
  , brokerExternalID :: Int
  , isOpen           :: Bool
  , horizon          :: Int
  , dateStart        :: ZonedTime
  , dateEnd          :: ZonedTime
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
    externalID       <- o .: "id"
    brokerExternalID <- o .: "broker_id"
    isOpen           <- o .: "is_open"
    horizon          <- o .: "horizon"
    dateStart        <- o .: "date_start"
    dateEnd          <- o .: "date_end"
    priceStart       <- o .: "price_start"
    price            <- o .: "price"
    strategy         <- o .: "strategy"
    title            <- o .: "title"
    description      <- o .: "description"
    isVisible        <- o .: "visibility"
    believe          <- o .: "believe"
    notBelieve       <- o .: "not_believe"
    expectedDateEnd  <- o .: "expected_date_end"
    ticker           <- o .: "ticker"
    tag              <- o .: "tags"

    yield <- o .: "yield" >>=
      maybe (fail "\"yield\" is not a Double") return . readMaybe

    targetYield <- o .: "target_yield" >>=
      maybe (fail "\"target_yield\" is not a Double") return . readMaybe

    return IdeaResponse{..}

data Tag = Tag {
    jurisdiction :: T.Text
  , currency     :: T.Text
  } deriving (Generic, Show)
