{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ideas.Storage
    ( batchUpsert
    ) where

import GHC.Generics (Generic)
import Control.Exception (handle)
import Control.Conditional (if')
import Database.PostgreSQL.Simple (Connection, ToRow, connect, close, executeMany, returning, withTransaction)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Data.Text as T
import qualified Ideas.Response as I
import Flags.Flags (CliFlags(..))
import Storage (getConnectionInfo)
import Utils (printWrap, defaultErrorHandler)

batchUpsert :: CliFlags -> [I.IdeaResponse] -> IO ()
batchUpsert cf ideas = putStrLn "started storing ideas" >> handle
  (defaultErrorHandler "failed to store ideas: ")
  (do
    conn <- connect $ getConnectionInfo cf
    brokerExternalIDs <- withTransaction conn $ doBatchUpsert conn ideas
    close conn
    putStrLn "finished storing ideas"
    if'
      (null brokerExternalIDs)
      (return ())
      (printWrap "brokers not found by external ID: " brokerExternalIDs)
  )

doBatchUpsert :: Connection -> [I.IdeaResponse] -> IO [Int]
doBatchUpsert conn -> ideas = do
  results <- returning conn upsertIdeasQuery $ map toModel ideas

  (upsertedIdeas, notUpsertedIdeas) <- partition $ isJust . fst $
    map (\idea -> (I.externalID idea `lookup` results, idea)) ideas

  executeMany conn insertTickersQuery $ map (\(Just id, idea) -> (id, I.ticker idea)) upsertedIdeas

  executeMany conn insertTagsQuery $ map
    (\(Just id, idea) -> let
                           t = I.tag idea
                           c = I.currency t
                         in
                           (id, if' (c == "Рубли") "Russian" "Foreign", c, I.jurisdiction t)
    )
    upsertedIdeas

  return $ map I.brokerExternalID notUpsertedIdeas
  where
    upsertIdeasQuery = [sql|
        INSERT INTO ideas (
          external_id,
          source,
          broker_id,
          is_open,
          horizon,
          date_start,
          date_end,
          price_start,
          price,
          yield,
          target_yield,
          strategy,
          title,
          description,
          believe,
          not_believe,
          is_deleted,
          expected_date_end,
          created_at,
          updated_at,
          recommend,
          is_visible_mm,
          is_visible_wm
        )
        SELECT (
          v.external_id,
          v.source,
          b.id,
          v.is_open,
          v.horizon,
          v.date_start,
          v.date_end,
          v.price_start,
          v.price,
          v.yield,
          v.target_yield,
          v.strategy,
          v.title,
          v.description,
          v.believe,
          v.not_believe,
          v.is_deleted,
          v.expected_date_end,
          v.created_at,
          v.updated_at,
          v.recommend,
          v.is_visible_mm,
          v.is_visible_wm
        )
        FROM (
          VALUES
          (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ) v (
          external_id,
          source,
          broker_external_id,
          is_open,
          horizon,
          date_start,
          date_end,
          price_start,
          price,
          yield,
          target_yield,
          strategy,
          title,
          description,
          believe,
          not_believe,
          is_deleted,
          expected_date_end,
          created_at,
          updated_at,
          recommend,
          is_visible_mm,
          is_visible_wm
        ) inner JOIN brokers b on b.external_id = v.broker_external_id
        ON CONFLICT (source, external_id) DO UPDATE SET
          broker_id=EXCLUDED.broker_id
          is_open=EXCLUDED.is_open
          horizon=EXCLUDED.horizon
          date_start=EXCLUDED.date_start
          date_end=EXCLUDED.date_end
          price_start=EXCLUDED.price_start
          price=EXCLUDED.price
          yield=EXCLUDED.yield
          target_yield=EXCLUDED.target_yield
          strategy=EXCLUDED.strategy
          title=EXCLUDED.title
          description=EXCLUDED.description
          believe=EXCLUDED.believe
          not_believe=EXCLUDED.not_believe
          is_deleted=EXCLUDED.is_deleted
          expected_date_end=EXCLUDED.expected_date_end
        	updated_at=now(),
        	recommend=EXCLUDED.recommend,
        	is_visible_mm=EXCLUDED.is_visible_mm,
        	is_visible_wm=EXCLUDED.is_visible_wm
        RETURNING external_id::INTEGER, id
      |]
    insertTickersQuery = [sql|
        INSERT INTO idea_tickers (idea_id, ticker)
        SELECT v.*
        FROM (
          VALUES
          (?,?)
        ) v (idea_id, ticker) LEFT JOIN idea_tickers t ON v.idea_id = t.idea_id
        WHERE t.id IS NULL
      |]
    insertTagsQuery = [sql|
        INSERT INTO idea_tags (idea_id, tag, currency, jurisdiction)
        SELECT v.*
        FROM (
          VALUES
          (?,?,?,?)
        ) v (idea_id, tag, currency, jurisdiction) LEFT JOIN idea_tags t ON v.idea_id = t.idea_id
        WHERE t.id IS NULL
      |]

data IdeaModel = IdeaModel {
    externalID       :: Int
  , brokerExternalID :: Int
  , isOpen           :: Bool
  , horizon          :: Int
  , dateStart        :: ZonedTime
  , dateEnd          :: Maybe ZonedTime
  , priceStart       :: Double
  , price            :: Double
  , yield            :: Double
  , targetYield      :: Double
  , strategy         :: T.Text
  , title            :: T.Text
  , description      :: T.Text
  , isVisible        :: Bool
  , believe          :: Int
  , notBelieve       :: Int
  , isDeleted        :: Bool
  , expectedDateEnd  :: ZonedTime
  , createdAt        :: ZonedTime
  , updatedAt        :: ZonedTime
  , recommend        :: String
  , isVisibleMM      :: Bool
  , isVisibleWM      :: Bool
  } deriving (Generic, ToRow)

toModel :: I.IdeaResponse -> IdeaModel
toModel i = IdeaModel {
    externalID       = I.externalID i
  , brokerExternalID = I.brokerExternalID i
  , isOpen           = I.isOpen i
  , horizon          = if' (horizon' == 0 && isJust dateEnd') () horizon'
  , dateStart        = I.dateStart i
  , dateEnd          = if' (horizon' == 0) () () `fromMaybe` dateEnd'
  , priceStart       = I.priceStart i
  , price            = I.price i
  , yield            = I.yield i
  , targetYield      = I.targetYield i
  , strategy         = strategy'
  , title            = I.title i
  , description      = I.description i
  , isVisible        = visible
  , believe          = I.believe i
  , notBelieve       = I.notBelieve i
  , isDeleted        = False
  , expectedDateEnd  = I.expectedDateEnd i
  , createdAt        = "now()"
  , updatedAt        = "now()"
  , recommend        = ""
  , isVisibleMM      = visible && strategy'
  , isVisibleWM      = visible && strategy'
  }
  where
    horizon' = I.horizon i
    dateEnd' = I.dateEnd i
    strategy' = I.strategy i /= "Падение"
    visible = I.isVisible i
