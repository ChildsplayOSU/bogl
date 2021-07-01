--
-- ServerMain.hs
-- Main entry point for the backend language server
--

module Main where

import API.Server
import System.Environment
import Control.Exception.Base

handleExceptions :: SomeException -> IO ()
handleExceptions ex = putStrLn ("Server Exception Caught: " ++ (show ex))

-- boots up the server by default
main :: IO ()
main = do
  -- get and check if 1st arg is an int
  args <- getArgs
  case length args of
    0     -> (startServer 8080) `catch` handleExceptions      -- default to port 8080
    1     -> (startServer (read (head args))) `catch` handleExceptions -- bind to 1st port # instad
    _     -> putStrLn "\n\nError: Can only pass one arg!\nUsage\n./boglserver\n./boglserver 8080\n\n"
