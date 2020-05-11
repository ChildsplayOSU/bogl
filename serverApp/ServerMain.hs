module Main where
import API.Server

-- boots up the server by default
main :: IO ()
main = do
  startServer
