-- | an example

module Runtime.Examples where

import Language.Syntax

import Runtime.Eval
import Runtime.Typechecker
import Runtime.Repl

import Parser.Parser

ex :: IO ()
ex = do
  parsed <- parseGameFile "example1.bgl"
  case parsed of
    Just g -> do
      check <- tc g
      if check then repl g else return ()
    Nothing -> return ()
