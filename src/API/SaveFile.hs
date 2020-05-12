{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
--
-- SaveFile.hs
--
-- Endpoint to handle saving of a file
--

module API.SaveFile (handleSaveFile) where

import API.JSONData
import Servant
import Control.Monad.IO.Class
import Control.Exception hiding (Handler)


-- |Handles saving a file to the server space
handleSaveFile :: SpielFile -> Handler SpielResponses
handleSaveFile (SpielFile fn contents) = liftIO $ do
    -- verifies the file is actually able to be written to
    success <- try $ writeFile (fn) contents :: IO (Either IOException ())
    case success of
      Right _ -> return [(Log (fn ++ " written successfully"))]
      Left e -> return [(Log ("Exception: " ++ displayException e))]
