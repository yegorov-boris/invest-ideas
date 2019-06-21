module Brokers.Broker
    ( Broker(..)
    ) where

import qualified Data.Text as T
import Brokers.Response (SpecializationResume)

data Broker = Broker {
    externalID                :: Int
  , source                    :: String
  , name                      :: T.Text
  , rating                    :: Int
  , ideasCount                :: Int
  , ideasPositive             :: Int
  , description               :: T.Text
  , accuracy                  :: Double -- TODO: Double|T.Text
  , profitableIdeasAvgYield   :: Double
  , totalProfitableIdeas      :: Int
  , unprofitableIdeasAvgYield :: Double
  , totalUnprofitableIdeas    :: Int
  , bestIdeaExternalID        :: Maybe Int
  , newIdeasPerMonth          :: Int
  , ideaAvgDaysLong           :: Int
  , specializationResume      :: SpecializationResume
  , isDeleted                 :: Bool
  , isVisibleMM               :: Bool
  , isVisibleWM               :: Bool
  } deriving Show
