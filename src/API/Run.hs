--
-- Run.hs
--
-- Holds utility run code for both /runCmds and /runCode.
-- Holds the routines for parsing and interpreting a BoGL Prelude and Game file
--

module API.Run (_runFileWithCommands) where

import API.JSONData
import Servant
import Data.Bifunctor
import Parser.Parser
import Language.Syntax
import Language.Types
import Text.Parsec.Pos
import Text.Parsec (errorPos)
import Text.Parsec.Error

import Typechecker.Typechecker
import Runtime.Eval
import Control.Monad.IO.Class
import Runtime.Values


-- |Runs this code under the IO monad
_runFileWithCommands :: SpielCommand -> IO SpielResponses
_runFileWithCommands (SpielCommand prelude gameFile inpt buf) = do
  parsed <- parsePreludeAndGameFiles prelude gameFile
  case parsed of
    Right game -> do
      let checked = tc game
      if success checked
        then return $ [SpielTypes (rtypes checked), (serverRepl game gameFile inpt buf)]
        else return $ SpielTypes (rtypes checked) : map (SpielTypeError . snd) (errors checked)

    Left err -> do
      let position = errorPos err
          l = sourceLine position
          c = sourceColumn position
          in
        return $ [SpielParseError l c gameFile (show err)]


-- |Handles running a command in the repl from the server
serverRepl :: (Game SourcePos) -> String -> String -> [Val] -> SpielResponse
serverRepl g@(Game _ i@(BoardDef (szx,szy) _) b vs) fn inpt buf = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) buf x of

            -- program terminated normally with a value
            Right (val) -> SpielValue val

            -- board and tape returned, returns the board for displaying on the frontend
            Left (v, t) -> SpielPrompt v

            -- runtime error encountered
            Left err -> SpielRuntimeError (show err)

        -- typechecker encountered an error in the expression
        Left err -> (SpielTypeError err)
    -- bad parse
    Left err ->
      let position = errorPos err
          l = sourceLine position
          c = sourceColumn position in
      (SpielParseError l c fn (show err))
