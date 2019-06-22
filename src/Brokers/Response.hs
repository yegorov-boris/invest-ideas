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

data Body = Body {
    success :: Bool
  , results :: [BrokerResponse]
  } deriving (Generic, Show)

instance FromJSON Body

data BrokerResponse = BrokerResponse { -- TODO: camel case
    id                           :: Int
  , name                         :: T.Text
  , rating                       :: Int
  , ideas_count                  :: Int
  , ideas_positive               :: Int
  , description                  :: T.Text
  , accuracy                     :: Double
  , profitable_ideas_avg_yield   :: Double
  , total_profitable_ideas       :: Int
  , unprofitable_ideas_avg_yield :: Double
  , total_unprofitable_ideas     :: Int
  , best_idea_id                 :: Maybe Int
  , new_ideas_per_month          :: Int
  , idea_avg_days_long           :: Int
  , specialization_resume        :: SpecializationResume
  } deriving Show

instance FromJSON BrokerResponse where
  parseJSON = withObject "BrokerResponse" $ \o -> do
    id                           <- o .: "id"
    name                         <- o .: "name"
    rating                       <- o .: "rating"
    description                  <- o .: "description"
    profitable_ideas_avg_yield   <- o .: "profitable_ideas_avg_yield"
    total_profitable_ideas       <- o .: "total_profitable_ideas"
    unprofitable_ideas_avg_yield <- o .: "unprofitable_ideas_avg_yield"
    total_unprofitable_ideas     <- o .: "total_unprofitable_ideas"
    best_idea_id                 <- o .: "best_idea_id"
    new_ideas_per_month          <- o .: "new_ideas_per_month"
    idea_avg_days_long           <- o .: "idea_avg_days_long"
    specialization_resume        <- o .: "specialization_resume"

    ideas_count <- do
      s <- o .: "ideas_count"
      case readMaybe s of
        Nothing -> fail "\"ideas_count\" is not an Int"
        Just x  -> return x

    ideas_positive <- do
      s <- o .: "ideas_positive"
      case readMaybe s of
        Nothing -> fail "\"ideas_positive\" is not an Int"
        Just x  -> return x

    accuracy <- asum [
        o .: "accuracy",
        do
          s <- o .: "accuracy" -- TODO: shorten
          case readMaybe s of
            Nothing -> fail "\"accuracy\" is not a Double"
            Just x  -> return x
      ]

    return BrokerResponse{..}

data SpecializationResume = SpecializationResume {
    asset    :: Maybe T.Text
  , currency :: Maybe T.Text
  , txt      :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON SpecializationResume
