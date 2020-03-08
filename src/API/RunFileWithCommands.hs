--
-- RunFileWithCommands.hs
--
-- Endpoint for running a given file with a list of commands
-- and returning a list of responses
--

module API.RunFileWithCommands (handleRunFileWithCommands) where

import API.JSONData
import Servant
import Parser.Parser
import Language.Syntax
import Language.Types
import Runtime.Values
import Typechecker.Typechecker
import Runtime.Eval
import Control.Monad.IO.Class


-- runs a file with given commands, lifting it into Handler
handleRunFileWithCommands :: SpielCommand -> Handler SpielResponses
handleRunFileWithCommands sc = do
  (liftIO (_runFileWithCommands sc))


-- runs command as IO
_runFileWithCommands :: SpielCommand -> IO SpielResponses
_runFileWithCommands (SpielCommand gameFile inpt) = do
  parsed <- parseGameFile (gameFile ++ ".bgl")
  case parsed of
    Just game -> do
      let check = success (tc game)
      if check then return (SpielResponses (serverRepl game gameFile inpt)) else return (SpielResponses [SpielTypeError 0 0 gameFile "Type error in the game file."])
    Nothing -> do
      return (SpielResponses [SpielParseError 0 0 gameFile "There is some error in the game file code."])


-- handles running a command in the repl from the server
serverRepl :: Game -> String -> [String] -> [SpielResponse]
serverRepl _ _ [] = []
serverRepl g@(Game _ i@(BoardDef (szx,szy) _) b vs) fn (inpt:ils) = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) [] x of

            -- program terminated normally with a value
            Right (val) -> ((SpielValue (show val)):(serverRepl g fn ils))

            -- board and tape returned, returns the board for displaying on the frontend
            Left ((Vboard b'), _) -> ((SpielBoard (show b')):(serverRepl g fn ils)) -- used to be ((Vboard b'), t')

            -- runtime error encountered
            Left err -> ((SpielRuntimeError (show err)):(serverRepl g fn ils))

        -- typechecker encountered an error in the environment
        Left err -> ((SpielTypeError 0 0 fn (show err)):(serverRepl g fn ils))

    -- bad parse
    Left err -> ((SpielParseError 0 0 fn (show err)):(serverRepl g fn ils))
