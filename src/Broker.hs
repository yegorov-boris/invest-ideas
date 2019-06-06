module Broker
    (Broker
    ) where

import Data.Time.LocalTime (ZonedTime)

data Broker = Broker { externalID :: String
                     , source :: String
                     , name :: String
                     , rating :: String
                     , ideasCount :: Int
                     , ideasPositive :: Int
                     , description :: String
                     , accuracy :: Double
                     , profitableIdeasAvgYield :: Double
                     , totalProfitableIdeas :: Int
                     , unprofitableIdeasAvgYield :: Double
                     , totalUnprofitableIdeas :: Int
                     , bestIdeaExternalID :: String
                     , newIdeasPerMonth :: Int
                     , ideaAvgDaysLong :: Int
                     , specializationResumeAsset :: String
                     , specializationResumeCurrency :: String
                     , specializationResumeDescription :: String
                     , createdAt :: ZonedTime
                     , updatedAt :: ZonedTime
                     , isDeleted :: Bool
                     , isVisibleMM :: Bool
                     , isVisibleWM :: Bool
                     }
