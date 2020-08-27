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
import Runtime.Eval
import Parser.Parser
import Runtime.Monad
import Typechecker.Monad (TypeError)

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

instance FromJSON Val where
  parseJSON (Object v) = do
    t <- v .: "input"
    case parseLine t of
      Right x -> case runWithBuffer (emptyEnv (0,0)) ([], [], 1) x of
        Right (_, v') -> return v'
        Left _ -> fail "failed to parse..." -- FIXME
      Left _ -> fail "failed to parse..."
  parseJSON _ = fail "Unable to parse Val"


-- | Representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    prelude :: String,
    file   :: String,
    input :: String,
    buffer :: [Val]
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
  SpielTypeError TypeError |
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
  show x                                = show x


-- | List of spiel responses
type SpielResponses = [SpielResponse]
