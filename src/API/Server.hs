{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

--
-- Server.hs
--
-- Handles starting up an instance of Servant
-- Returns encoded board data on request, and computes
-- boards when input data is taken
--
module API.Server (startServer, serverApp, SpielApi) where

import API.JSONData
import API.RunFileWithCommands
import API.Test
import API.SaveFile
import API.ReadFile
import API.CORSMiddleware

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.RequestLogger (logStdoutDev)



-- (Headers '[Header "Access-Control-Allow-Origin" String] SpielResponses)
type SpielApi = "runCmds" :> ReqBody '[JSON] SpielCommand :> Post '[JSON] SpielResponses
    :<|> "save" :> ReqBody '[JSON] SpielFile :> Post '[JSON] SpielResponses
    :<|> "read" :> ReqBody '[JSON] SpielRead :> Post '[JSON] SpielFile
    :<|> "test" :> Get '[JSON] SpielResponses


-- defining the api
api :: Proxy SpielApi
api = Proxy


-- handler for actual requests
-- performs mapping from endpoints to functions and responses
-- in order by which they are defined  for the API
handler :: Server SpielApi
handler = handleRunFileWithCommands
  :<|> handleSaveFile
  :<|> handleReadFile
  :<|> return (handleTest)


-- | Prepares a server application
serverApp :: Application
serverApp = logStdoutDev . allowCsrf . corsified $ serve api handler


{--
-- setup a command api, for talking to the Repl
-- represents, 'POST/cmd' w/ a JSON object
-- describes a Command in the request body
-- and returns an encoded response
-- can test with the following CURL examples:
> /runCmds
curl --verbose -H "Origin: localhost" --request POST --header "Content-Type: application/json" --data '{"file":"examples/example1","inputs":["succ(1)"]}' http://localhost:8080/runCmds

> /runCmds
curl --verbose -H "Origin: localhost" --request POST --header "Content-Type: application/json" --data '{"file":"examples/TicTacToe","inputs":["gameLoop(empty)","1","2"]}' http://localhost:8080/runCmds

> /save
curl --verbose -H "Origin: localhost" --request POST --header "Content-Type: application/json" --data '{"fileName":"TEST_FILE","content":"2 + 3 * 3"}' http://localhost:8080/save

> /read
curl --verbose -H "Origin: localhost" --request POST --header "Content-Type: application/json" --data '{"path":"examples/TicTacToe"}' http://localhost:8080/read

> /test
curl --verbose -H "Origin: localhost" http://localhost:8080/test
--}

startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn "Spiel Backend listening for POST/GET with endpoints:"
  putStrLn "http://localhost:8080/runCmds (POST)"
  putStrLn "http://localhost:8080/save (POST)"
  putStrLn "http://localhost:8080/read (POST)"
  putStrLn "http://localhost:8080/test (GET)"
  putStrLn "Ready..."
  (run port serverApp)
