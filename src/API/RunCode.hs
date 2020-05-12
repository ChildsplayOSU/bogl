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
import Control.Exception hiding (Handler)

import Control.Monad.IO.Class


-- |Runs literal code, lifting it into Handler
-- Creates a temporary tmp_prelude.bgl and tmp_tmp.bgl to write contents into.
-- Once these both succeed, the new temporary files are parsed
-- , and the response is returned
handleRunCode :: SpielCommand -> Handler SpielResponses
handleRunCode (SpielCommand preludeContents gameFileContents inpt buf) = liftIO $ do
  -- attempt to write temporary prelude & game files
  success1 <- try $ writeFile ("tmp_prelude.bgl") preludeContents :: IO (Either IOException ())
  success2 <- try $ writeFile ("tmp_tmp.bgl") gameFileContents :: IO (Either IOException ())
  -- verify they both succeeded before proceeding
  case success1 of
    Right _ -> case success2 of
                  Right _ -> (liftIO (_runFileWithCommands (SpielCommand "tmp_prelude.bgl" "tmp_tmp.bgl" inpt buf))) -- valid, calls API.Run._runFileWithCommands to interpret these files
                  Left e2  -> return [(Log ("Game File Exception: " ++ displayException e2))]
    Left e1 -> return [(Log ("Prelude Exception: " ++ displayException e1))]
