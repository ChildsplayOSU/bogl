{-# LANGUAGE TemplateHaskell #-}

--
-- JSONData.hs
--
-- Defines JSON data for the API
--

module API.JSONData where


import Data.Aeson
import Data.Aeson.TH

-- representation of a request to read a BoGL file
data SpielRead = SpielRead {
  path :: String
} deriving (Eq, Show)


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


-- representation of a collection of responses from Spiel
data SpielResponse =
  SpielOK String |
  SpielError String
  deriving(Eq)

instance Show SpielResponse where
  show (SpielOK s)     = show s
  show (SpielError s)  = "ERROR: " ++ show s

data SpielResponses = SpielResponses {
    -- TODO implement as [Either Exception Val] instead of [String]
    responses :: [SpielResponse]
  } deriving (Eq, Show)


-- derive JSON for cmd & response
$(deriveJSON defaultOptions ''SpielRead)
$(deriveJSON defaultOptions ''SpielFile)
$(deriveJSON defaultOptions ''SpielCommand)
$(deriveJSON defaultOptions ''SpielResponse)
$(deriveJSON defaultOptions ''SpielResponses)
