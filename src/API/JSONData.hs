{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- JSONData.hs
--
-- Defines JSON data for the API
--

module API.JSONData where

import Language.Types


import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Runtime.Values
import Runtime.Eval
import Parser.Parser
import Runtime.Monad
import Typechecker.Monad (TypeError)
import Data.List
import Data.Array

-- | representation of a request to read a BoGL file
-- TODO: Use Path type
data SpielRead = SpielRead {
  path :: String
} deriving (Eq, Show, Generic)

instance FromJSON SpielRead where
  parseJSON (Object v) = SpielRead <$> v .: "fileName"


-- | representation of a request to share a prelude & gamefile
data SpielShare = SpielShare {
  preludeContent :: String,
  gameContent :: String
} deriving (Eq, Show, Generic)

instance ToJSON SpielShare where

instance FromJSON SpielShare where
  parseJSON (Object v) = SpielShare <$> v .: "preludeContent" <*> v.: "gameContent"

-- | representation of a file that will be saved by the user
-- TODO Probably want to use a lock or something
data SpielFile = SpielFile {
  fileName  :: String,
  content   :: String
} deriving (Eq, Show, Generic)

instance ToJSON SpielFile where

instance FromJSON SpielFile where
  parseJSON (Object v) = SpielFile <$> v .: "fileName" <*> v .: "content"

instance FromJSON Val where
  parseJSON (Object v) = do
    t <- v .: "input"
    case parseLine t of
      Right x -> case runWithBuffer (emptyEnv (0,0)) ([], [], 1) x of
        Right (_, v') -> return v'
        Left err -> fail "failed to parse..." -- FIXME
      Left err -> fail "failed to parse..."
  parseJSON _ = fail "FAILURE?"


-- | representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    prelude :: String,
    file   :: String,
    input :: String,
    buffer :: [Val]
  } deriving (Eq, Show, Generic)


instance ToJSON SpielCommand
instance FromJSON SpielCommand


-- | for errors
type Message = String
type LineNum = Int
type ColNum  = Int
type FileName= String

-- a buffer of boards which should be printed with the value of executing the expression
type BufferedBoards = [Val]
type ExecutionValue = Val

-- | Represents possible response categories from the server
-- These are then parsed accordingly on the front-end
data SpielResponse =
  -- represents a prompt for input and buffered boards
  SpielPrompt [Val] |
  -- represents a successful operation
  SpielSuccess String |
  -- represents a successful load
  -- 1st prelude, 2nd gamefile
  SpielLoadResult String String |
  -- represents a win/lose result
  SpielGameResult String |
  -- represents a type error
  SpielTypeError TypeError |
  -- represents a parse error
  SpielParseError LineNum ColNum FileName Message |
  -- represents a runtime error in spiel
  SpielValue BufferedBoards ExecutionValue |
  SpielTypeHole LineNum ColNum Xtype | -- ^ represents a typed hole that can be filled
  SpielError String | -- ^ fallback standard error, something went wrong in spiel
  SpielRuntimeError String |
  SpielTypes [(String, Type)] | -- ^ List of typechecked types
  Log String -- ^ String
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
  show (SpielTypeHole m x y)            = show m
  -- show a fallback error, such as reading a bad-file
  show (SpielError m)                   = show m



type SpielResponses = [SpielResponse]
