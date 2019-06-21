{-# LANGUAGE DeriveGeneric #-}

module Brokers.Response
    ( BrokerResponse(..)
    , SpecializationResume(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Text as T

data BrokerResponse = BrokerResponse {
    id                           :: Int
  , name                         :: T.Text
  , rating                       :: Int
  , ideas_count                  :: T.Text
  , ideas_positive               :: T.Text
  , description                  :: T.Text
  , accuracy                     :: T.Text
  , profitable_ideas_avg_yield   :: Double
  , total_profitable_ideas       :: Int
  , unprofitable_ideas_avg_yield :: Double
  , total_unprofitable_ideas     :: Int
  , best_idea_id                 :: Maybe Int
  , new_ideas_per_month          :: Int
  , idea_avg_days_long           :: Int
  , specialization_resume        :: SpecializationResume
  } deriving (Generic, Show)

instance FromJSON BrokerResponse

data SpecializationResume = SpecializationResume {
    asset    :: Maybe T.Text
  , currency :: Maybe T.Text
  , txt      :: Maybe T.Text
  } deriving (Generic, Show)

instance FromJSON SpecializationResume