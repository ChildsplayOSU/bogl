--
-- Run.hs
--
-- Holds utility run code for both /runCmds and /runCode.
-- Holds the routines for parsing and interpreting a BoGL Prelude and Game file
--

module API.Run (_runCodeWithCommands) where

import API.JSONData
import Parser.Parser
import Language.Syntax
import Language.Types
import Text.Parsec.Pos
import Text.Parsec (errorPos)
import Text.Parsec.Error

import Typechecker.Typechecker
import Runtime.Eval
import Runtime.Values


-- | Runs BoGL code from raw text with the given commands
-- Similar to the above '_runFileWithCommands', but instead
-- utilizes parsePreludeAndGameText to parse the code directly,
-- without reading it from a file first
_runCodeWithCommands :: SpielCommand -> IO SpielResponses
_runCodeWithCommands sc@(SpielCommand _prelude gameFile _ _) =
  _handleParsed sc $ parsePreludeAndGameText _prelude gameFile


-- | Handles result of parsing a prelude and game
_handleParsed :: SpielCommand -> IO (Either ParseError (Game SourcePos)) -> IO SpielResponses
_handleParsed (SpielCommand _ gameFile inpt buf) parsed = do
  pparsed <- parsed
  case pparsed of
    Right game -> do
      let checked = tc game
      if success checked
        then return $ [SpielTypes (rtypes checked), (serverRepl game gameFile inpt (buf, []))]
        else return $ SpielTypes (rtypes checked) : map (SpielTypeError . snd) (errors checked)
    Left err -> do
      let position = errorPos err
          l = sourceLine position
          c = sourceColumn position
          in
        return $ [SpielParseError l c gameFile (show err)]


-- |Handles running a command in the repl from the server
serverRepl :: (Game SourcePos) -> String -> String -> ([Val], [Val]) -> SpielResponse
serverRepl (Game _ i@(BoardDef (szx,szy) _) b vs) fn inpt buf = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) buf x of

            -- program terminated normally with a value
            Right (bs, val) -> SpielValue bs val

            -- boards and tape returned, returns the boards for displaying on the frontend
            Left (bs, _) -> SpielPrompt bs

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
