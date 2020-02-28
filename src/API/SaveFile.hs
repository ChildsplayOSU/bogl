{-# LANGUAGE DataKinds       #-}

--
-- SaveFile.hs
--
-- Endpoint to handle saving of a file
--

module API.SaveFile (handleSaveFile) where

import API.JSONData
import Servant
import Control.Monad.IO.Class
import Control.Exception(try,Exception)

-- TODO Change SpielResponse to SpielOK/SpielError

handleSaveFile :: SpielFile -> Handler SpielResponse
handleSaveFile (SpielFile fn contents) = liftIO (do
    -- TODO needs to check if this file was successfully written
    writeFile (fn ++ ".bgl") contents
    return (SpielResponse [fn ++ ".bgl" ++ " written successfully"])
  )
