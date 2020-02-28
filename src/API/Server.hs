{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds       #-}
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

import API.JSONData
import API.RunFileWithCommands
import API.Test
import API.SaveFile

import Network.Wai
import Network.Wai.Handler.Warp
import Servant



type SpielApi = "runFileWithCommands" :> ReqBody '[JSON] SpielCommand :> Post '[JSON] SpielResponses
  :<|> "file" :> ReqBody '[JSON] SpielFile :> Post '[JSON] SpielResponse
  :<|> "test" :> Get '[JSON] SpielResponse


-- defining the api
api :: Proxy SpielApi
api = Proxy


-- handler for actual requests
-- performs mapping from endpoints to functions and responses
-- in order by which they are defined  for the API
handler :: Server SpielApi
handler = handleRunFileWithCommands
  :<|> handleSaveFile
  :<|> handleTest


serverApp :: Application
serverApp = serve api handler


-- setup a command api, for talking to the Repl
-- represents, 'POST/cmd' w/ a JSON object
-- describes a Command in the request body
-- and returns an encoded response
-- can test with this:
-- curl --verbose --request POST --header "Content-Type: application/json"
-- --data '{"file":"examples/example1.bgl","input":"succ(1)"}' http://localhost:8080/spiel

-- > /runFileWithCommands
-- curl --request POST --header "Content-Type: application/json" --data '{"file":"examples/Notakto.bgl","inputs":["gameLoop(empty)","1","2"]}' http://localhost:8080/runFileWithCommands

-- > /file
-- curl --request POST --header "Content-Type: application/json" --data '{"fileName":"TEST_FILE.bgl","content":"2 + 3 * 3"}' http://localhost:8080/file

-- > /test
-- curl http://locahost:8080/test
startServer :: IO ()
startServer = do
  let port = 8080
  putStrLn "Spiel Backend listening for POST/GET with endpoints:"
  putStrLn "http://localhost:8080/runFileWithCommands (POST)"
  putStrLn "http://localhost:8080/saveFile (POST)"
  putStrLn "http://localhost:8080/test (GET)"
  putStrLn "Ready..."
  (run port serverApp)
