module Client.Brokers
    (fetch
    ) where

import Network.HTTP.Client
import Broker (Broker)

fetch :: IO Broker
fetch = do
  c <- openConnection "www.example.com" 80

  let q = buildRequest1 $ do
              http GET "/"
              setAccept "text/html"

  sendRequest c q emptyBody

  receiveResponse c (\p i -> do
      putStr $ show p

      x <- Streams.read i
      S.putStr $ fromMaybe "" x)

  closeConnection c
