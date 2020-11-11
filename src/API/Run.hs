{-|
Module      : API.Run
Description : Allows running of commands via the API
Copyright   : (c)
License     : BSD-3

Holds utility run code for both /runCmds and /runCode.
Holds the routines for parsing and interpreting a BoGL Prelude and Game file.
-}

module API.Run (_runCodeWithCommands) where

import API.JSONData (SpielResponse(..), SpielCommand(..), SpielResponses, spielParseError)
import Parser.Parser
import Language.Syntax
import Language.Types
import Text.Parsec.Pos

import Typechecker.Typechecker
import Typechecker.Monad (Env(types))
import Runtime.Eval (runWithBuffer, bindings_)
import Runtime.Values
import Runtime.Monad (Buffer, emptyEnv)

import Control.Monad(liftM, ap)

-- | Runs BoGL code from raw text with the given commands
-- utilizes parsePreludeAndGameText to parse the code directly,
-- without reading it from a file first
_runCodeWithCommands :: SpielCommand -> SpielResponses
_runCodeWithCommands sc@(SpielCommand _prelude gameFile _ _ filename) =
   case parsePreludeAndGameText _prelude gameFile filename of
      Right game          -> _handleParsed sc game
      Left progParseError -> [spielParseError gameFile progParseError]

-- | Typechecks and evaluates a parsed prelude and game
_handleParsed :: SpielCommand -> (Game SourcePos) -> SpielResponses
_handleParsed (SpielCommand _ gameFile replExpr buf _) game =
   if success progTCRes then
      case handleInput inputEnv buf of
         H (Left er)    -> [er]
         H (Right buf') -> [progTypes, (serverRepl game gameFile replExpr (buf', [], 1))]
   else
      progTypes : map (SpielTypeError . snd) (errors progTCRes)
   where
      progTCRes = tc game
      inputEnv  = (e progTCRes) { types = [] } -- keep the input type, discard other bindings
      progTypes = SpielTypes (rtypes progTCRes)

-- | Parses, typecheckes, and evaluates input strings so they can be used in evaluation
--   as a 'Val' buffer
--
--   Note: if anything other than the first input string results in failure, then something is
--   wrong on the front end. These have all been verified in previous calls to _runCodeWithCommands
handleInput :: Env -> [String] -> SpielHandler [Val]
handleInput inputEnv xs = do
   exprs  <- mapM parseInput xs
   exprs' <- mapM (tcInput inputEnv) exprs
   mapM evalInput exprs'

-- | Parses input strings so that they can be typechecked and evaluated
parseInput :: String -> SpielHandler (Expr SourcePos)
parseInput s = case parseFromText literal "user input" s of
                  (Left er)  -> hFail $ spielParseError "user input" er
                  (Right ex) -> return ex

-- | Typechecks input expressions
tcInput :: Env -> (Expr SourcePos) -> SpielHandler (Expr SourcePos)
tcInput inputEnv x = case exprHasInputType inputEnv x of
                        (Left er) -> hFail $ SpielTypeError er
                        Right _ -> return x

-- | Evaluates typechecker input expressions.
--   If a literal fails to evaluate, then something is wrong with the evaluator,
--   hence the internal error
evalInput :: (Expr SourcePos) -> SpielHandler Val
evalInput x = case run x of
                  Left _ -> hFail $ SpielError "input evaluation internal error"
                  Right (_, v') -> return v'
   where
      run = runWithBuffer (emptyEnv (0,0)) ([], [], 1)

-- | Handles running a command in the repl from the server
serverRepl :: (Game SourcePos) -> String -> String -> Buffer -> SpielResponse
serverRepl (Game _ i@(BoardDef (szx,szy) _) b vs) fn replExpr buf = do
  case parseLine replExpr of
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

newtype SpielHandler a = H (Either SpielResponse a)

instance Show a => Show (SpielHandler a) where
   show (H (Left s))  = show s
   show (H (Right x)) = show x

-- Monad for handling an operation in which failure is represented as an appropriate SpielResponse
instance Monad SpielHandler where
   return res = H $ Right res
   (H h) >>= k = case h of
                    Left er   -> hFail er
                    Right res -> k res

instance Functor SpielHandler where
   fmap = liftM

instance Applicative SpielHandler where
   pure  = return
   (<*>) = ap

-- Smart constructor for a failure case in SpielHandler
hFail :: SpielResponse -> SpielHandler a
hFail = H . Left
