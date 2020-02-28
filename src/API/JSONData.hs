{-# LANGUAGE TemplateHaskell #-}

--
-- JSONData.hs
--
-- Defines JSON data for the API
--

module API.JSONData where


import Data.Aeson
import Data.Aeson.TH


-- representation of a file that will be saved by the user
data SpielFile = SpielFile {
  fileName  :: String,
  content   :: String
} deriving (Eq, Show)


-- representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    file   :: String,
    inputs :: [String]
  } deriving (Eq, Show)


-- representation of the response from the Repl
data SpielResponse = SpielResponse {
    -- TODO implment as [Either Exception Val] instead of [String]
    responses :: [String]
  } deriving (Eq, Show)


-- derive JSON for cmd & response
$(deriveJSON defaultOptions ''SpielFile)
$(deriveJSON defaultOptions ''SpielCommand)
$(deriveJSON defaultOptions ''SpielResponse)
