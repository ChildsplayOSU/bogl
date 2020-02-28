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
import Runtime.Values
import Typechecker.Typechecker
import Runtime.Eval
import Control.Monad.IO.Class


-- runs a file with given commands, lifting it into Handler
handleRunFileWithCommands :: SpielCommand -> Handler SpielResponse
handleRunFileWithCommands sc = do
  (liftIO (_runFileWithCommands sc))


-- runs command as IO
_runFileWithCommands :: SpielCommand -> IO SpielResponse
_runFileWithCommands (SpielCommand gameFile inpt) = do
  Just game <- parseGameFile gameFile
  let check = success (tc game)
  -- (Typechecker.Monad.Env, [Either (ValDef, Typechecker.Monad.TypeError) (Name, Type)])
  -- (Holes, [Either (Value , and associated error (Name, Type))])
  if check then return (SpielResponse (serverRepl game inpt)) else return (SpielResponse ["ERR: Could not parse game file!"])


-- handles running a command in the repl from the server
serverRepl :: Game -> [String] -> [String]
serverRepl _ [] = []
serverRepl g@(Game _ i@(BoardDef (szx,szy) _) b vs) (inpt:ils) = do
  case parseLine inpt of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right _ -> do -- Right t
          case runWithBuffer (bindings_ (szx, szy) vs) [] x of

            -- TODO program terminated, potentially more data desired
            Right (terminated) -> ((show terminated):(serverRepl g ils))

            -- TODO, board and tape returned, program needs more input
            Left ((Vboard b'), _) -> ((show b'):(serverRepl g ils)) -- used to be ((Vboard b'), t')

            -- TODO runtime error encountered
            Left err -> ((show err):(serverRepl g ils))

        -- TODO typechecker encountered an error in the environment (unlikely not to happen)
        Left err -> ((show err):(serverRepl g ils))

    -- bad parse
    Left err -> ((show err):(serverRepl g ils))
