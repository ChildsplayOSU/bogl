{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : API.RunCode
Description : Endpoint for running literal BoGL code without a file (for the Prelude as well)
Copyright   : (c)
License     : BSD-3
-}

module API.RunCode (handleRunCode) where

import API.Run
import API.JSONData
import Servant

-- | Runs literal code, lifting it into Handler
-- Runs the contents of the prelude and gamefile without writing them out into files
-- Once these both succeed, the new temporary files are parsed
-- , and the response is returned
handleRunCode :: SpielCommand -> Handler SpielResponses
-- calls API.Run._runCodeWithCommands to interpret this raw BoGL code
handleRunCode sc = return $ _runCodeWithCommands sc
