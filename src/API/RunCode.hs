{-# LANGUAGE OverloadedStrings #-}
--
-- RunCode.hs
--
-- Endpoint for running literal BoGL code without a file (for the Prelude as well)
--

module API.RunCode (handleRunCode) where

import API.Run
import API.JSONData
import Servant

import Control.Monad.IO.Class


-- | Runs literal code, lifting it into Handler
-- Creates a temporary tmp_prelude.bgl and tmp_tmp.bgl to write contents into.
-- Once these both succeed, the new temporary files are parsed
-- , and the response is returned
handleRunCode :: SpielCommand -> Handler SpielResponses
-- calls API.Run._runCodeWithCommands to interpret this raw BoGL code
handleRunCode sc = liftIO (_runCodeWithCommands sc)
