module Brokers.Mapper -- TODO: get rid of
    ( fromResponse
    ) where

import Text.Read (readEither)
import qualified Data.Text as T
import qualified Brokers.Broker as B
import qualified Brokers.Response as R

fromResponse :: R.BrokerResponse -> Either String B.Broker
fromResponse b = do
  return $ B.Broker {
      B.externalID                = R.id b
    , B.source                    = "invest-idei.ru"
    , B.name                      = R.name b
    , B.rating                    = R.rating b
    , B.ideasCount                = R.ideas_count b
    , B.ideasPositive             = R.ideas_positive b
    , B.description               = R.description b
    , B.accuracy                  = R.accuracy b
    , B.profitableIdeasAvgYield   = R.profitable_ideas_avg_yield b
    , B.totalProfitableIdeas      = R.total_profitable_ideas b
    , B.unprofitableIdeasAvgYield = R.unprofitable_ideas_avg_yield b
    , B.totalUnprofitableIdeas    = R.total_unprofitable_ideas b
    , B.bestIdeaExternalID        = R.best_idea_id b
    , B.newIdeasPerMonth          = R.new_ideas_per_month b
    , B.ideaAvgDaysLong           = R.idea_avg_days_long b
    , B.specializationResume      = R.specialization_resume b
    , B.isDeleted                 = False
    , B.isVisibleMM               = True
    , B.isVisibleWM               = True
    }
