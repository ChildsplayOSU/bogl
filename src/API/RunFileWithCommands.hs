{-# LANGUAGE OverloadedStrings #-}
--
-- RunFileWithCommands.hs
--
-- Endpoint for running a given file with a list of commands
-- and returning a list of responses
--

module API.RunFileWithCommands (handleRunFileWithCommands) where

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


-- runs a file with given commands, lifting it into Handler
handleRunFileWithCommands :: SpielCommand -> Handler SpielResponses
handleRunFileWithCommands sc = do
  (liftIO (_runFileWithCommands sc))

-- runs command as IO
_runFileWithCommands :: SpielCommand -> IO SpielResponses
_runFileWithCommands (SpielCommand gameFile inpt buf) = do
  parsed <- parseGameFile (gameFile)
  case parsed of
    Right game -> do
      let checked = tc game
      if success checked
        then return $ (SpielTypes (rtypes checked):(serverRepl game gameFile inpt buf))
        else return $ (SpielTypes (rtypes checked)):map (SpielTypeError . snd) (errors checked)

    Left err -> do
      let position = errorPos err
          l = sourceLine position
          c = sourceColumn position
          msg = concatMap (messageString) (errorMessages err) in
        return $ [SpielParseError l c gameFile (show msg)]


-- handles running a command in the repl from the server
serverRepl :: (Game SourcePos) -> String -> [String] -> [Val] -> [SpielResponse]
serverRepl _ _ [] _ = []
serverRepl g@(Game _ i@(BoardDef (szx,szy) _) b vs) fn (inpt:ils) buf = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) buf x of

            -- program terminated normally with a value
            Right (val) -> ((SpielValue val):(serverRepl g fn ils buf)) -- FIXME FIXME FIXME

            -- board and tape returned, returns the board for displaying on the frontend
            Left (board@(Vboard _), _) -> (SpielValue board:(serverRepl g fn ils buf)) -- used to be ((Vboard b'), t')

            -- runtime error encountered
            Left err -> ((SpielRuntimeError (show err)):(serverRepl g fn ils buf))

        -- typechecker encountered an error in the environment
        Left err -> ((SpielTypeError err):(serverRepl g fn ils buf))

    -- bad parse
    Left err ->
      let position = errorPos err
          l = sourceLine position
          c = sourceColumn position
          msg = concatMap messageString $ errorMessages err in
      (SpielParseError l c fn msg:(serverRepl g fn ils buf))


-- Orphaned instance here (discuss):

