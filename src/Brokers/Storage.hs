{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Brokers.Storage
    ( batchUpsert
    ) where

import Control.Exception (SomeException, handle)
import Database.PostgreSQL.Simple (ConnectInfo(..), connect, close, executeMany)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Brokers.Broker (Broker(..))
import Brokers.Response (SpecializationResume(..))
import Flags.Flags (CliFlags(..))
import Utils (printWrap)

batchUpsert :: CliFlags -> [Broker] -> IO ()
batchUpsert cf brokers = do
  putStrLn "started storing brokers"
  handle onErr $ doBatchUpsert cf brokers

doBatchUpsert :: CliFlags -> [Broker] -> IO ()
doBatchUpsert cf brokers = do
  conn <- connect $ ConnectInfo {
        connectHost     = dbHost cf
      , connectPort     = dbPort cf
      , connectUser     = dbUser cf
      , connectPassword = dbPassword cf
      , connectDatabase = dbName cf
      }
  executeMany conn query $ map toValues brokers
  close conn
  putStrLn "finished storing brokers"
  where
--    TODO: UPSERT
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
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,?,?,?)
      |]
    toValues broker = (
        externalID broker
      , source broker
      , name broker
      , rating broker
      , ideasCount broker
      , ideasPositive broker
      , description broker
      , accuracy broker
      , profitableIdeasAvgYield broker
      , totalProfitableIdeas broker
      , unprofitableIdeasAvgYield broker
      , totalUnprofitableIdeas broker
      , bestIdeaExternalID broker
      , newIdeasPerMonth broker
      , ideaAvgDaysLong broker
      , asset $ specializationResume broker
      , currency $ specializationResume broker
      , txt $ specializationResume broker
      , isDeleted broker
      , isVisibleMM broker
      , isVisibleWM broker
      )

onErr :: SomeException -> IO ()
onErr = printWrap "failed to store brokers: "
