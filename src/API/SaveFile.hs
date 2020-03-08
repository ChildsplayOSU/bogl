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

-- TODO Change SpielResponse to SpielOK/SpielError

handleSaveFile :: SpielFile -> Handler SpielResponses
handleSaveFile (SpielFile fn contents) = liftIO(do
    -- TODO needs to check if this file was successfully written, rather than just assuming it is (@montymxb)
    writeFile (fn ++ ".bgl") contents
    return (SpielResponses [(SpielValue (fn ++ ".bgl" ++ " written successfully"))]))
