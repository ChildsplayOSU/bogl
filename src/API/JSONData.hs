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


-- for errors
type Message = String
type LineNum = Int
type ColNum  = Int
type FileName= String

-- | Represents possible response categories from the server
-- These are then parsed accordingly on the front-end
data SpielResponse =
  -- represents a board that will be printed
  SpielBoard String |
  -- represents a win/lose result
  SpielGameResult String |
  -- represents a type error
  SpielTypeError LineNum ColNum FileName Message |
  -- represents a parse error
  SpielParseError LineNum ColNum FileName Message |
  -- represents a runtime error in spiel
  SpielRuntimeError String |
  -- represents a spiel value, which is a standard reply
  -- when the Repl is done evaluating expressions
  SpielValue String |
  -- represents a typed hole that can be filled
  SpielTypeHole String |
  -- fallback standard error, something went wrong in spiel
  SpielError String
  deriving(Eq)


instance Show SpielResponse where
  -- shows a board in JSON
  show (SpielBoard b)                 = show b
  -- indicates that the game is over and a player has won
  show (SpielGameResult gr)           = show gr
  -- shows a type error to the user
  show (SpielTypeError ln cn fn m)    = "{tag: \"typeError\", lineNum: "++show ln++", colNum: "++ show cn ++", fileName: \""++ fn ++"\", message: \""++ m ++"\"}"
  -- shows a parse error to the user
  show (SpielParseError ln cn fn m)   = "{tag: \"parseError\", lineNum: "++show ln++", colNum: "++ show cn ++", fileName: \""++ fn ++"\", message: \""++ m ++"\"}"
  -- shows a runtime error in spiel
  show (SpielRuntimeError m)          = show m
  -- show a spiel value
  show (SpielValue m)                 = show m
  -- show a typed hole
  -- TODO this one needs cleaning up
  show (SpielTypeHole m)              = show m
  -- show a fallback error, such as reading a bad-file
  show (SpielError m)                 = show m




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
