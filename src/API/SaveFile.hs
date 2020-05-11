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

-- TODO Change SpielResponse to SpielOK/SpielError

handleSaveFile :: SpielFile -> Handler SpielResponses
handleSaveFile (SpielFile fn contents) = liftIO $ do
    -- TODO needs to check if this file was successfully written, rather than just assuming it is (@montymxb)
    success <- try $ writeFile (fn) contents :: IO (Either IOException ())
    case success of
      Right _ -> return [(Log (fn ++ " written successfully"))]
      Left e -> return [(Log ("Exception: " ++ displayException e))]
