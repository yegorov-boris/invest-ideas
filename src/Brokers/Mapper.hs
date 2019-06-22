module Brokers.Mapper -- TODO: get rid of
    ( fromResponse
    ) where

import Text.Read (readEither)
import qualified Data.Text as T
import qualified Brokers.Broker as B
import qualified Brokers.Response as R

fromResponse :: R.BrokerResponse -> B.Broker
fromResponse b = B.Broker {
    B.externalID                = R.externalID b
  , B.source                    = "invest-idei.ru"
  , B.name                      = R.name b
  , B.rating                    = R.rating b
  , B.ideasCount                = R.ideasCount b
  , B.ideasPositive             = R.ideasPositive b
  , B.description               = R.description b
  , B.accuracy                  = R.accuracy b
  , B.profitableIdeasAvgYield   = R.profitableIdeasAvgYield b
  , B.totalProfitableIdeas      = R.totalProfitableIdeas b
  , B.unprofitableIdeasAvgYield = R.unprofitableIdeasAvgYield b
  , B.totalUnprofitableIdeas    = R.totalUnprofitableIdeas b
  , B.bestIdeaExternalID        = R.bestIdeaExternalID b
  , B.newIdeasPerMonth          = R.newIdeasPerMonth b
  , B.ideaAvgDaysLong           = R.ideaAvgDaysLong b
  , B.specializationResume      = R.specializationResume b
  , B.isDeleted                 = False
  , B.isVisibleMM               = True
  , B.isVisibleWM               = True
  }
