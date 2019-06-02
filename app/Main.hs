module Main where

import Flags (parseCliFlags, ideasUrl, token, ideasPollingInterval)

main :: IO ()
main = do
  cliFlags <- parseCliFlags
  putStrLn $ ideasUrl cliFlags
  putStrLn $ token cliFlags
  putStrLn $ ideasPollingInterval cliFlags
