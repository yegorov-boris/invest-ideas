module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (get, getStatusCode)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as B
import Flags (CliFlags, ideasURL, token)

-- TODO: cancel on timeout
-- TODO: retry
fetch :: CliFlags -> IO ()
fetch cf = do
  putStrLn "started fetching brokers"
  get url (\response inputStream -> if
    (getStatusCode response) == 200
    then putStrLn "OK"
    else putStrLn "failed to fetch brokers")
  putStrLn "finished fetching brokers"
  where
    url = fromString $ (ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)

--import Data.Time.LocalTime (ZonedTime)
--
--data Broker = Broker { externalID :: String
--                     , source :: String
--                     , name :: String
--                     , rating :: String
--                     , ideasCount :: Int
--                     , ideasPositive :: Int
--                     , description :: String
--                     , accuracy :: Double
--                     , profitableIdeasAvgYield :: Double
--                     , totalProfitableIdeas :: Int
--                     , unprofitableIdeasAvgYield :: Double
--                     , totalUnprofitableIdeas :: Int
--                     , bestIdeaExternalID :: String
--                     , newIdeasPerMonth :: Int
--                     , ideaAvgDaysLong :: Int
--                     , specializationResumeAsset :: String
--                     , specializationResumeCurrency :: String
--                     , specializationResumeDescription :: String
--                     , createdAt :: ZonedTime
--                     , updatedAt :: ZonedTime
--                     , isDeleted :: Bool
--                     , isVisibleMM :: Bool
--                     , isVisibleWM :: Bool
--                     }
