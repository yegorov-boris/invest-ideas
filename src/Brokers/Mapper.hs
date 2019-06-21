module Brokers.Mapper
    ( fromResponse
    ) where

import Text.Read (readEither)
import qualified Data.Text as T
import qualified Brokers.Broker as B
import qualified Brokers.Response as R

fromResponse :: R.BrokerResponse -> Either String B.Broker
fromResponse b = do
  ideasCount <- intFromText $ R.ideas_count b
  ideasPositive <- intFromText $ R.ideas_positive b
  accuracy <- doubleFromText $ R.accuracy b
  return $ B.Broker {
      B.externalID                = R.id b
    , B.source                    = "invest-idei.ru"
    , B.name                      = R.name b
    , B.rating                    = R.rating b
    , B.ideasCount                = ideasCount
    , B.ideasPositive             = ideasPositive
    , B.description               = R.description b
    , B.accuracy                  = accuracy
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

intFromText :: T.Text -> Either String Int
intFromText v = readEither (show v) :: Either String Int

doubleFromText :: T.Text -> Either String Double
doubleFromText v = readEither (show v) :: Either String Double
