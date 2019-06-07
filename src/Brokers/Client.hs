module Brokers.Client
    (fetch
    ) where

import Network.Http.Client (get, concatHandler')
import Flags (CliFlags, ideasURL, token)

fetch :: CliFlags -> IO ()
fetch cf = do
  putStrLn "started fetching brokers"
  x <- get ((ideasURL cf) ++ "/brokers?api_key=" ++ (token cf)) concatHandler'
  putStr x
  putStrLn "finished fetching brokers"

--  c <- openConnection "www.example.com" 80
--
--  let q = buildRequest1 $ do
--              http GET "/"
--              setAccept "text/html"
--
--  sendRequest c q emptyBody
--
--  receiveResponse c (\p i -> do
--      putStr $ show p
--
--      x <- Streams.read i
--      S.putStr $ fromMaybe "" x)
--
--  closeConnection c


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
