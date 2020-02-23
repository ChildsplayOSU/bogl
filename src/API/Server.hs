{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- {-# LANGUAGE OverloadedStrings #-}

--
-- Server.hs
--
-- Handles starting up an instance of Servant
-- Returns encoded board data on request, and computes
-- boards when input data is taken
--
module API.Server (startServer, serverApp) where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Parser.Parser
import Language.Syntax
import Runtime.Repl
import Runtime.Typechecker
import Runtime.Eval

-- for liftIO apparently????
import qualified Graphics.UI.Threepenny as UI

-- representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    file   :: String,
    inputs :: [String]
  } deriving (Eq, Show)

-- representation of the response from the Repl
data SpielResponse = SpielResponse {
    responses :: [String]
  } deriving (Eq, Show)

{--
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)
--}

-- $(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''SpielCommand)
$(deriveJSON defaultOptions ''SpielResponse)

-- setup a command api, for talking to the Repl
-- represents, 'POST/cmd' w/ a JSON object
-- describes a Command in the request body
-- and returns an encoded response
-- can test with this:
-- curl --verbose --request POST --header "Content-Type: application/json"
-- --data '{"file":"examples/example1.bgl","input":"succ(1)"}' http://localhost:8080/spiel

-- another example
{--
curl --verbose --request POST --header "Content-Type: application/json" --data '{"file":"examples/Notakto.bgl","inputs":["gameLoop(empty)","1","2"]}' http://localhost:8080/spiel
--}
type SpielApi = "spiel" :> ReqBody '[JSON] SpielCommand :> Post '[JSON] SpielResponse
  :<|> "spiel" :> Get '[JSON] SpielResponse

startServer :: IO ()
startServer = do
  putStrLn "Spiel Backend listening for POST/GET on http://localhost:8080/spiel"
  (run 8080 serverApp)

serverApp :: Application
serverApp = serve api handler

api :: Proxy SpielApi
api = Proxy

handler :: Server SpielApi
handler = runCommand :<|> handleTestResponse

-- runs a command, lifting it into Handler
runCommand :: SpielCommand -> Handler SpielResponse
runCommand sc = do
  (UI.liftIO (_runCommand sc))

-- runs command as IO
_runCommand :: SpielCommand -> IO SpielResponse
_runCommand (SpielCommand file input) = do
  Just game <- parseGameFile file
  check <- tc game
  if check then return (SpielResponse (serverRepl game input)) else return (SpielResponse ["ERR: Could not parse game file!"])

-- handles running a command in the repl from the server
serverRepl :: Game -> [String] -> [String]
serverRepl g [] = []
serverRepl g@(Game n i@(BoardDef szx szy p) b vs) (input:ils) = do
  case parseLine input of
    Right x -> do
      case tcexpr (environment i b vs) x of
        Right t -> do
          case runWithTape (bindings (szx, szy) vs) [] x of
            -- ??? right val is good?
            Right (x) -> ((show x):(serverRepl g ils))
            -- board back
            Left ((Vboard b'), t') -> ((show b'):(serverRepl g ils))
            -- error
            Left err -> ((show err):(serverRepl g ils))
        -- bad expression in environment?
        Left err -> ((show err):(serverRepl g ils))
    -- bad parse
    Left err -> ((show err):(serverRepl g ils))

-- returns a test reponse to the GET test endpoint, to ensure this is running
handleTestResponse :: Handler SpielResponse
handleTestResponse = return (SpielResponse ["Spiel is Running!"])


-- standard test response
testResponse :: SpielResponse
testResponse = SpielResponse ["this is from Spiel's API"]
