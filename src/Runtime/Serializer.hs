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

module Runtime.Serializer (encodeBoard) where

import Text.JSON.Generic hiding(encode)
import Parser.Parser
import Language.Syntax
import Data.Aeson
import Runtime.Values
import Data.Array
import Data.Aeson.TH
import Data.List

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
