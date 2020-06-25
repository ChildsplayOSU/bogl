--
-- Load.hs
--
-- API Endpoint that allows loading a prelude & gamefile at the same time
--

module API.Load (handleLoad) where

import API.JSONData
import Servant
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class

-- | handles reading the prelude and file
handleLoad :: SpielRead -> Handler SpielResponse
handleLoad fileBase = do
  (liftIO (_handleLoad fileBase))


-- internally attempts to load the prelude & gamefile to return
_handleLoad :: SpielRead -> IO SpielResponse
_handleLoad (SpielRead fn) = do
  preludeResult   <- try $ readFile ("shared/"++fn++".bglp") :: IO (Either IOException String)
  gamefileResult  <- try $ readFile ("shared/"++fn++".bgl") :: IO (Either IOException String)
  case preludeResult of
    Right preludeContent -> case gamefileResult of
      Right gameContent       -> return (SpielLoadResult preludeContent gameContent)
      Left e2                 -> return (SpielError ("Could load gamefile: " ++ (show e2)))
    Left e1              -> return (SpielError ("Couldn't load prelude: " ++ (show e1)))
