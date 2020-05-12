{-# LANGUAGE OverloadedStrings #-}
--
-- RunFileWithCommands.hs
--
-- Endpoint for running a given file with a command, input buffer,
-- and returning a list of responses
--

module API.RunFileWithCommands (handleRunFileWithCommands) where

import API.Run
import API.JSONData
import Servant
import Control.Monad.IO.Class


-- |Runs a file with given commands, lifting it into Handler
handleRunFileWithCommands :: SpielCommand -> Handler SpielResponses
handleRunFileWithCommands sc = do
  -- calls API.Run._runFileWithCommands to interpret these files
  (liftIO (_runFileWithCommands sc))
