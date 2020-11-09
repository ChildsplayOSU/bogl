{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : API.JSONData
Description : Defines JSON data for the Spiel API
Copyright   : (c)
License     : BSD-3
-}

module API.JSONData where

import Language.Types


import GHC.Generics
import Data.Aeson
import Runtime.Values
import Error.Error

import Text.Parsec (errorPos, sourceLine, sourceColumn, ParseError)

-- | Representation of a request to read a BoGL file
data SpielRead = SpielRead {
  path :: String
} deriving (Eq, Show, Generic)

instance FromJSON SpielRead where
  parseJSON (Object v) = SpielRead <$> v .: "fileName"
  parseJSON _          = undefined -- fallback


-- | Representation of a request to share a prelude & gamefile
data SpielShare = SpielShare {
  preludeContent :: String,
  gameContent :: String
} deriving (Eq, Show, Generic)

instance ToJSON SpielShare where

instance FromJSON SpielShare where
  parseJSON (Object v) = SpielShare <$> v .: "preludeContent" <*> v.: "gameContent"
  parseJSON _          = fail "Unable to parse Share option" -- fallback

-- | Representation of a file that will be saved by the user
data SpielFile = SpielFile {
  fileName  :: String,
  content   :: String
} deriving (Eq, Show, Generic)

instance ToJSON SpielFile where

instance FromJSON SpielFile where
  parseJSON (Object v) = SpielFile <$> v .: "fileName" <*> v .: "content"
  parseJSON _          = fail "Unable to parse File option" -- fallback

-- | Representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    prelude     :: String,
    file        :: String,
    input       :: String,
    buffer      :: [String],
    programName :: String
  } deriving (Eq, Show, Generic)

instance ToJSON SpielCommand
instance FromJSON SpielCommand

-- | Error message
type Message = String
-- | Error line number
type LineNum = Int
-- | Error column number
type ColNum  = Int
-- | Error file name
type FileName= String

-- | A buffer of boards which should be printed with the value of executing the expression
type BufferedBoards = [Val]
-- | An execution value
type ExecutionValue = Val

-- | Represents possible response categories from the server
-- These are then parsed accordingly on the front-end
data SpielResponse =
  -- | represents a prompt for input and buffered boards
  SpielPrompt [Val] |
  -- | represents a successful operation
  SpielSuccess String |
  -- | represents a successful load
  -- 1st prelude, 2nd gamefile
  SpielLoadResult String String |
  -- | represents a win/lose result
  SpielGameResult String |
  -- | represents a type error
  SpielTypeError Error  |
  -- | represents a parse error
  SpielParseError LineNum ColNum FileName Message |
  -- | represents a runtime error in spiel
  SpielValue BufferedBoards ExecutionValue |
  -- | represents a typed hole that can be filled
  SpielTypeHole LineNum ColNum Xtype |
  -- | fallback standard error, something went wrong in spiel
  SpielError String |
  -- | generic runtime error
  SpielRuntimeError String |
  -- | List of typechecked types
  SpielTypes [(String, Type)] |
  -- | String
  Log String
  deriving(Eq, Generic)

-- | Smart constructor for a SpielParseError
spielParseError :: String -> ParseError -> SpielResponse
spielParseError fn er = SpielParseError l c fn (show er)
   where
      pos = errorPos er
      l   = sourceLine pos
      c   = sourceColumn pos

instance ToJSON SpielResponse where

instance Show SpielResponse where
  -- shows a board in JSON
  show (SpielPrompt s)                  = show s
  -- shows a success response
  show (SpielSuccess result)            = "{success: 1, result: \""++result++"\"}"
  -- indicates that the game is over and a player has won
  show (SpielGameResult gr)             = show gr
  -- shows a parse error to the user
  show (SpielParseError ln cn fn m)     = "{tag: \"parseError\", lineNum: "++show ln++", colNum: "++ show cn ++", fileName: \""++ fn ++"\", message: \""++ m ++"\"}"
  -- shows a runtime error in spiel
  -- show a spiel value
  show (SpielValue bs v)                = show bs ++ " " ++ show v
  -- show a typed hole
  -- TODO this one needs cleaning up
  show (SpielTypeHole m _ _)            = show m
  -- show a fallback error, such as reading a bad-file
  show (SpielError m)                   = show m
  show (SpielTypeError e)               = show e
  show _                                = "unused SpielResponse" -- todo: clean these out

-- | List of spiel responses
type SpielResponses = [SpielResponse]
