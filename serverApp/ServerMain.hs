--
-- ServerMain.hs
-- Main entry point for the backend language server
--

module Main where

import API.Server
import System.Environment

-- boots up the server by default
main :: IO ()
main = do
  -- get and check if 1st arg is an int
  args <- getArgs
  case length args of
    0     -> startServer 8080      -- default to port 8080
    1     -> startServer (read (head args)) -- bind to 1st port # instad
    _     -> putStrLn "\n\nError: Can only pass one arg!\nUsage\n./spielserver\n./spielserver 8080\n\n"
