{-# LANGUAGE DeriveDataTypeable #-}
-- Serializer.hs
--
-- Handles un/serializing boards and related data,
-- primarily for stateless communication with the Spiel frontend
---- Serializer.hs
--
-- Handles un/serializing boards and related data,
-- primarily for stateless communication with the Spiel frontend
--

module Runtime.Serializer where

import Parser.Parser

import Text.JSON.Generic
import Data.Typeable
import Language.Syntax

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: String
    , zip    :: Integer
    } deriving (Show, Data, Typeable)

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Data, Typeable)

aa :: String
aa = "{\"name\": \"some body\", \"age\" : 23, \"address\" : {\"house\" : 285, \"street\" : \"7th Ave.\", \"city\" : \"New York\", \"state\" : \"New York\", \"zip\" : 10001}}"

--main = print (decodeJSON aa :: Person)

--data GameJSON = GameJSON
  --{
  --  name  :: Name
    --board :: BoardDefJSON
    --input :: InputDefJSON
    --vals  :: ValDefJSON
  --}

--encode :: Board -> String

--decodeGame :: String -> Name
--decodeGame jsonStr = decodeJSON jsonStr :: Board


ex :: IO ()
ex = do
  parsed <- parseGameFile "src/Runtime/example1.bgl"
  case parsed of
    Just g  -> do
      print (encodeJSON g)
      return ()
    Nothing -> return () -- do nothing...
