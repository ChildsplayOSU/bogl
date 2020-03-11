{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Typechecker.Monad (TypeError)
import Data.List
import Data.Array

-- | representation of a request to read a BoGL file
-- TODO: Use Path type
data SpielRead = SpielRead {
  path :: String
} deriving (Eq, Show, Generic)

instance FromJSON SpielRead


-- | representation of a file that will be saved by the user
-- TODO Probably want to use a lock or something
data SpielFile = SpielFile {
  fileName  :: String,
  content   :: String
} deriving (Eq, Show, Generic)

instance ToJSON SpielFile where

instance FromJSON SpielFile where


-- | representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    file   :: String,
    inputs :: [String]
  } deriving (Eq, Show, Generic)

instance ToJSON SpielCommand
instance FromJSON SpielCommand

-- | for errors
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
  SpielTypeError TypeError |
  -- represents a parse error
  SpielParseError LineNum ColNum FileName Message |
  -- represents a runtime error in spiel
  SpielValue Val |
  SpielTypeHole LineNum ColNum Xtype | -- ^ represents a typed hole that can be filled
  SpielError String | -- ^ fallback standard error, something went wrong in spiel
  SpielRuntimeError String |
  SpielTypes [(String, Type)] | -- ^ List of typechecked types
  Log String -- ^ String
  deriving(Eq, Generic)

instance ToJSON SpielResponse where

instance Show SpielResponse where
  -- shows a board in JSON
  show (SpielBoard s)                 = show s
  -- indicates that the game is over and a player has won
  show (SpielGameResult gr)           = show gr
  -- shows a parse error to the user
  show (SpielParseError ln cn fn m)   = "{tag: \"parseError\", lineNum: "++show ln++", colNum: "++ show cn ++", fileName: \""++ fn ++"\", message: \""++ m ++"\"}"
  -- shows a runtime error in spiel
  -- show a spiel value
  show (SpielValue m)                 = show m
  -- show a typed hole
  -- TODO this one needs cleaning up
  show (SpielTypeHole m x y)              = show m
  -- show a fallback error, such as reading a bad-file
  show (SpielError m)                 = show m



type SpielResponses = [SpielResponse]


encode1DArray :: [((Int,Int),Val)] -> String
encode1DArray [] = ""
encode1DArray ((_,val):ls) = "\"" ++ (show val) ++ "\"" ++ (if length ls > 0 then "," else "") ++ (encode1DArray ls)

encode2DArray :: [[((Int, Int), Val)]] -> String
encode2DArray [] = ""
encode2DArray (ar:ls) = "[" ++ (encode1DArray ar) ++ "]" ++ (if length ls > 0 then "," else "") ++ (encode2DArray ls)

toGrid x = (groupBy (\x y -> (fst . fst) x == (fst . fst) y) (assocs x))

encodeBoard :: Val -> String
encodeBoard v@(Vboard b) = "{\"board\": ["++(encode2DArray (toGrid b))++"]}"
encodeBoard _ = "Cannot encode non-board!"


