{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Brokers.Response
    ( Body(..)
    , BrokerResponse(..)
    , SpecializationResume(..)
    ) where

import GHC.Generics (Generic)
import Data.Foldable (asum)
import Text.Read (readMaybe)
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as T
import qualified Response as R

data Body = Body {
    success :: Bool
  , results :: [BrokerResponse]
  } deriving (Generic, Show)

instance FromJSON Body

instance R.Body Body
  where success = success

data BrokerResponse = BrokerResponse {
    externalID                :: Int
  , name                      :: T.Text
  , rating                    :: Int
  , ideasCount                :: Int
  , ideasPositive             :: Int
  , description               :: T.Text
  , accuracy                  :: Double
  , profitableIdeasAvgYield   :: Double
  , totalProfitableIdeas      :: Int
  , unprofitableIdeasAvgYield :: Double
  , totalUnprofitableIdeas    :: Int
  , bestIdeaExternalID        :: Maybe Int
  , newIdeasPerMonth          :: Int
  , ideaAvgDaysLong           :: Int
  , specializationResume      :: SpecializationResume
  } deriving Show

instance FromJSON BrokerResponse where
  parseJSON = withObject "BrokerResponse" $ \o -> do
    let toInt = R.parseCustomField o readMaybe "Int"
    let toDouble = R.parseCustomField o readMaybe "Double"
    externalID                <- o .: "id"
    name                      <- o .: "name"
    rating                    <- o .: "rating"
    ideasCount                <- toInt "ideas_count"
    ideasPositive             <- toInt "ideas_positive"
    description               <- o .: "description"
    accuracy                  <- asum [o .: "accuracy", toDouble "accuracy"]
    profitableIdeasAvgYield   <- o .: "profitable_ideas_avg_yield"
    totalProfitableIdeas      <- o .: "total_profitable_ideas"
    unprofitableIdeasAvgYield <- o .: "unprofitable_ideas_avg_yield"
    totalUnprofitableIdeas    <- o .: "total_unprofitable_ideas"
    bestIdeaExternalID        <- o .: "best_idea_id"
    newIdeasPerMonth          <- o .: "new_ideas_per_month"
    ideaAvgDaysLong           <- o .: "idea_avg_days_long"
    specializationResume      <- o .: "specialization_resume"

    return BrokerResponse{..}

data SpecializationResume = SpecializationResume {
    asset    :: Maybe T.Text
  , currency :: Maybe T.Text
  , txt      :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON SpecializationResume
