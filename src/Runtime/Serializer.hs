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
import Language.Syntax


-- encodes the game as a JSON object
encodeGame :: (Data a) => (Game a) -> String
encodeGame g = encodeJSON g


-- decodes a JSON object into a game
decodeGame :: (Data a) => String -> (Game a)
decodeGame s = decodeJSON s


-- test Game encode functionality
encode_test :: IO ()
encode_test = do
  parsed <- parseGameFile "src/Runtime/example1.bgl"
  case parsed of
    Just g  -> do
      print (encodeGame g)
      return ()
    Nothing -> return () -- do nothing...


-- test Game decode functionality
{-
decode_test :: IO ()
decode_test = do
  parsed <- parseGameFile "src/Runtime/example1.bgl"
  case parsed of
    Just g  -> do
      print (decodeGame (encodeGame (clearAnn g)))
      return ()
    Nothing -> return () -- do nothing... -}
