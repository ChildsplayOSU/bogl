{-|
Module      : API.Run
Description : Allows running of commands via the API
Copyright   : (c)
License     : BSD-3

Holds utility run code for both /runCmds and /runCode.
Holds the routines for parsing and interpreting a BoGL Prelude and Game file.
-}

module API.Run (_runCodeWithCommands) where

import API.JSONData
import Parser.Parser
import Language.Syntax
import Language.Types
import Text.Parsec.Pos
import Text.Parsec.Error

import Typechecker.Typechecker
import Typechecker.Monad (Env(types))
import Runtime.Eval
import Runtime.Values
import Runtime.Monad

import Utils.General

-- | Runs BoGL code from raw text with the given commands
-- utilizes parsePreludeAndGameText to parse the code directly,
-- without reading it from a file first
_runCodeWithCommands :: SpielCommand -> IO SpielResponses
_runCodeWithCommands sc@(SpielCommand _prelude gameFile _ _ filename) =
  (_handleParsed sc $ parsePreludeAndGameText _prelude gameFile filename)

-- | Handles result of parsing a prelude and game
_handleParsed :: SpielCommand -> IO (Either ParseError (Game SourcePos)) -> IO SpielResponses
_handleParsed (SpielCommand _ gameFile inpt buf _) parsed = do
  pparsed <- parsed
  case pparsed of
    Right game -> do
      let progTCRes  = tc game
          goodResp   = [SpielTypes (rtypes progTCRes), (serverRepl game gameFile inpt (buf, [], 1))]
          progErrTC  = SpielTypes (rtypes progTCRes) : map (SpielTypeError . snd) (errors progTCRes)

          -- this is bad. We grab the last Val off the buffer, which has not been typechecked
          -- then we do: Val => String => Expr so that we can type check it
          -- it would be much better to receive it as a Maybe Expr separate from the buffer
      if success progTCRes
        then return $
           maybe goodResp (\i -> handleInputStr (show i) progTCRes goodResp) (safeLast buf)
        else return progErrTC
    Left progParseError -> return [spielParseError gameFile progParseError]

-- | Propogates a good response if a given input string parses and typechecks
--   Returns the appropriate SpielError otherwise
handleInputStr :: String -> TcResult -> SpielResponses -> SpielResponses
handleInputStr inputStr progTCRes goodResp =
   case parseResult of
      (Left pe) -> [SpielParseError 1 1 "user input" (show pe)]
      (Right x) -> handleInputExpr x progTCRes goodResp
      where
         -- note: the use of literal is a bit of a hack
         -- this will work for now, but there is more work to be done here
         parseResult = parseFromText literal "user input" inputStr

-- | Propogates a good response if a given input 'Expr' is a valid Input type
--   Returns the appropriate SpielTypeError otherwise
handleInputExpr :: (Expr SourcePos) -> TcResult -> SpielResponses -> SpielResponses
handleInputExpr inputExpr progTCRes goodResp =
   case inputTyp of
      (Right _)         -> goodResp
      (Left inputTCErr) -> [SpielTypes (rtypes progTCRes), SpielTypeError inputTCErr]
      where
         inputTyp = exprHasInputType inputEnv inputExpr
         inputEnv = (e progTCRes) { types = [] } -- keep the input type, discard other bindings

-- |Handles running a command in the repl from the server
serverRepl :: (Game SourcePos) -> String -> String -> Buffer -> SpielResponse
serverRepl (Game _ i@(BoardDef (szx,szy) _) b vs) fn inpt buf = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) buf x of
            -- with a runtime error
            Right (_, (Err s)) -> (SpielRuntimeError (show s))

            -- program terminated normally with a value
            Right (bs, val) -> (SpielValue bs val)

            -- boards and tape returned, returns the boards for displaying on the frontend
            Left (bs, _) -> (SpielPrompt bs)

        -- typechecker encountered an error in the expression
        Left _err -> (SpielTypeError _err)
    -- bad parse
    Left progParseError -> spielParseError fn progParseError
