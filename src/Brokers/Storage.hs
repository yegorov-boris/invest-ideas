{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Brokers.Storage
    ( batchUpsert
    ) where

import Text.Printf (printf)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, displayException)
import Control.Monad.Trans.Reader (asks)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple (ToRow, close, executeMany)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Brokers.Response as B
import Storage (connect)
import Common (Context(..), Pipe)
import Utils (logInfo, logError)

batchUpsert :: [B.BrokerResponse] -> Pipe ()
batchUpsert brokers = do
  logger' <- asks logger
  logInfo' logger' "started storing brokers"
  (flip catch) onErr $ connect >>= \conn -> do
    liftIO $ executeMany conn query $ map toModel brokers
    liftIO $ close conn
    logInfo' logger' "finished storing brokers"
  where
    query = [sql|
        INSERT INTO brokers (
          external_id,
          source,
          name,
          rating,
          ideas_count,
          ideas_positive,
          description,
          accuracy,
          profitable_ideas_avg_yield,
          total_profitable_ideas,
          unprofitable_ideas_avg_yield,
          total_unprofitable_ideas,
          best_idea_external_id,
          new_ideas_per_month,
          idea_avg_days_long,
          specialization_resume_asset,
          specialization_resume_currency,
          specialization_resume_description,
          created_at,
          updated_at,
          is_deleted,
          is_visible_mm,
          is_visible_wm
        )
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (source, external_id) DO UPDATE SET
          name=EXCLUDED.name,
          rating=EXCLUDED.rating,
          ideas_count=EXCLUDED.ideas_count,
          ideas_positive=EXCLUDED.ideas_positive,
          description=EXCLUDED.description,
        	accuracy=EXCLUDED.accuracy,
        	profitable_ideas_avg_yield=EXCLUDED.profitable_ideas_avg_yield,
        	total_profitable_ideas=EXCLUDED.total_profitable_ideas,
        	unprofitable_ideas_avg_yield=EXCLUDED.unprofitable_ideas_avg_yield,
        	total_unprofitable_ideas=EXCLUDED.total_unprofitable_ideas,
        	best_idea_external_id=EXCLUDED.best_idea_external_id,
        	new_ideas_per_month=EXCLUDED.new_ideas_per_month,
        	idea_avg_days_long=EXCLUDED.idea_avg_days_long,
        	specialization_resume_asset=EXCLUDED.specialization_resume_asset,
        	specialization_resume_currency=EXCLUDED.specialization_resume_currency,
        	specialization_resume_description=EXCLUDED.specialization_resume_description,
        	updated_at=now(),
        	is_visible_mm=EXCLUDED.is_visible_mm,
        	is_visible_wm=EXCLUDED.is_visible_wm
      |]

onErr :: SomeException -> Pipe ()
onErr e = do
  logger' <- asks logger
  logError' logger' $ printf "failed to store brokers: %s" (displayException e)

data BrokerModel = BrokerModel {
    externalID                      :: String
  , source                          :: String
  , name                            :: T.Text
  , rating                          :: Int
  , ideasCount                      :: Int
  , ideasPositive                   :: Int
  , description                     :: T.Text
  , accuracy                        :: Double
  , profitableIdeasAvgYield         :: Double
  , totalProfitableIdeas            :: Int
  , unprofitableIdeasAvgYield       :: Double
  , totalUnprofitableIdeas          :: Int
  , bestIdeaExternalID              :: Maybe String
  , newIdeasPerMonth                :: Int
  , ideaAvgDaysLong                 :: Int
  , specializationResumeAsset       :: T.Text
  , specializationResumeCurrency    :: T.Text
  , specializationResumeDescription :: T.Text
  , createdAt                       :: String
  , updatedAt                       :: String
  , isDeleted                       :: Bool
  , isVisibleMM                     :: Bool
  , isVisibleWM                     :: Bool
  } deriving (Generic, ToRow)

toModel :: B.BrokerResponse -> BrokerModel
toModel b = BrokerModel {
    externalID                      = show $ B.externalID b
  , source                          = "invest-idei.ru"
  , name                            = B.name b
  , rating                          = B.rating b
  , ideasCount                      = B.ideasCount b
  , ideasPositive                   = B.ideasPositive b
  , description                     = B.description b
  , accuracy                        = B.accuracy b
  , profitableIdeasAvgYield         = B.profitableIdeasAvgYield b
  , totalProfitableIdeas            = B.totalProfitableIdeas b
  , unprofitableIdeasAvgYield       = B.unprofitableIdeasAvgYield b
  , totalUnprofitableIdeas          = B.totalUnprofitableIdeas b
  , bestIdeaExternalID              = show <$> B.bestIdeaExternalID b
  , newIdeasPerMonth                = B.newIdeasPerMonth b
  , ideaAvgDaysLong                 = B.ideaAvgDaysLong b
  , specializationResumeAsset       = fromMaybe "" $ B.asset $ B.specializationResume b
  , specializationResumeCurrency    = fromMaybe "" $ B.currency $ B.specializationResume b
  , specializationResumeDescription = fromMaybe "" $ B.txt $ B.specializationResume b
  , createdAt                       = "now()"
  , updatedAt                       = "now()"
  , isDeleted                       = False
  , isVisibleMM                     = True
  , isVisibleWM                     = True
  }

label = "brokers_storage"::String
logInfo' = logInfo label
logError' = logError label
