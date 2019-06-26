-- TODO: dup
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ideas.Storage
    ( batchUpsert
    ) where

import GHC.Generics (Generic)
import Control.Exception (SomeException, handle)
import Database.PostgreSQL.Simple (ConnectInfo(..), ToRow, connect, close, executeMany, withTransaction)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Ideas.Response as I
import Flags.Flags (CliFlags(..))
import Storage (getConnectionInfo)
import Utils (printWrap, defaultErrorHandler)

batchUpsert :: CliFlags -> [I.Idea] -> IO ()
batchUpsert cf ideas = do
  putStrLn "started storing ideas"
  handle (defaultErrorHandler "failed to store ideas: ") (doBatchUpsert cf ideas)

doBatchUpsert :: CliFlags -> [I.Idea] -> IO ()
doBatchUpsert cf ideas = do
  conn <- connect $ getConnectionInfo cf
--  executeMany conn query $ map toModel ideas
  close conn
  putStrLn "finished storing ideas"
--  where
--    query = [sql|
--        INSERT INTO ideas (
--          external_id
--          source,
--          name,
--          rating,
--          ideas_count,
--          ideas_positive,
--          description,
--          accuracy,
--          profitable_ideas_avg_yield,
--          total_profitable_ideas,
--          unprofitable_ideas_avg_yield,
--          total_unprofitable_ideas,
--          best_idea_external_id,
--          new_ideas_per_month,
--          idea_avg_days_long,
--          specialization_resume_asset,
--          specialization_resume_currency,
--          specialization_resume_description,
--          created_at,
--          updated_at,
--          is_deleted,
--          is_visible_mm,
--          is_visible_wm
--        )
--        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
--        ON CONFLICT (source, external_id) DO UPDATE SET
--          name=EXCLUDED.name,
--          rating=EXCLUDED.rating,
--          ideas_count=EXCLUDED.ideas_count,
--          ideas_positive=EXCLUDED.ideas_positive,
--          description=EXCLUDED.description,
--        	accuracy=EXCLUDED.accuracy,
--        	profitable_ideas_avg_yield=EXCLUDED.profitable_ideas_avg_yield,
--        	total_profitable_ideas=EXCLUDED.total_profitable_ideas,
--        	unprofitable_ideas_avg_yield=EXCLUDED.unprofitable_ideas_avg_yield,
--        	total_unprofitable_ideas=EXCLUDED.total_unprofitable_ideas,
--        	best_idea_external_id=EXCLUDED.best_idea_external_id,
--        	new_ideas_per_month=EXCLUDED.new_ideas_per_month,
--        	idea_avg_days_long=EXCLUDED.idea_avg_days_long,
--        	specialization_resume_asset=EXCLUDED.specialization_resume_asset,
--        	specialization_resume_currency=EXCLUDED.specialization_resume_currency,
--        	specialization_resume_description=EXCLUDED.specialization_resume_description,
--        	updated_at=now(),
--        	is_visible_mm=EXCLUDED.is_visible_mm,
--        	is_visible_wm=EXCLUDED.is_visible_wm
--      |]

--data IdeaModel = IdeaModel {
--    externalID                      :: String
--  , source                          :: String
--  , name                            :: T.Text
--  , rating                          :: Int
--  , ideasCount                      :: Int
--  , ideasPositive                   :: Int
--  , description                     :: T.Text
--  , accuracy                        :: Double
--  , profitableIdeasAvgYield         :: Double
--  , totalProfitableIdeas            :: Int
--  , unprofitableIdeasAvgYield       :: Double
--  , totalUnprofitableIdeas          :: Int
--  , bestIdeaExternalID              :: Maybe String
--  , newIdeasPerMonth                :: Int
--  , ideaAvgDaysLong                 :: Int
--  , specializationResumeAsset       :: T.Text
--  , specializationResumeCurrency    :: T.Text
--  , specializationResumeDescription :: T.Text
--  , createdAt                       :: String
--  , updatedAt                       :: String
--  , isDeleted                       :: Bool
--  , isVisibleMM                     :: Bool
--  , isVisibleWM                     :: Bool
--  } deriving (Generic, ToRow)

--toModel :: I.Idea -> IdeaModel
--toModel i = IdeaModel {
--    externalID                      = show $ B.externalID b
--  , source                          = B.source b
--  , name                            = B.name b
--  , rating                          = B.rating b
--  , ideasCount                      = B.ideasCount b
--  , ideasPositive                   = B.ideasPositive b
--  , description                     = B.description b
--  , accuracy                        = B.accuracy b
--  , profitableIdeasAvgYield         = B.profitableIdeasAvgYield b
--  , totalProfitableIdeas            = B.totalProfitableIdeas b
--  , unprofitableIdeasAvgYield       = B.unprofitableIdeasAvgYield b
--  , totalUnprofitableIdeas          = B.totalUnprofitableIdeas b
--  , bestIdeaExternalID              = show <$> B.bestIdeaExternalID b
--  , newIdeasPerMonth                = B.newIdeasPerMonth b
--  , ideaAvgDaysLong                 = B.ideaAvgDaysLong b
--  , specializationResumeAsset       = fromMaybe "" $ asset $ B.specializationResume b
--  , specializationResumeCurrency    = fromMaybe "" $ currency $ B.specializationResume b
--  , specializationResumeDescription = fromMaybe "" $ txt $ B.specializationResume b
--  , createdAt                       = "now()"
--  , updatedAt                       = "now()"
--  , isDeleted                       = B.isDeleted b
--  , isVisibleMM                     = B.isVisibleMM b
--  , isVisibleWM                     = B.isVisibleWM b
--  }
