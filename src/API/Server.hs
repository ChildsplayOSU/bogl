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
import API.Test
import API.ShareFile
import API.Load
import API.RunCode
import API.ReadFile
import API.CORSMiddleware

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.RequestLogger (logStdoutDev)



-- (Headers '[Header "Access-Control-Allow-Origin" String] SpielResponses)
type SpielApi = "share" :> ReqBody '[JSON] SpielShare :> Post '[JSON] SpielResponses      -- /share (SpielShare -> SpielResponses) shares a url for files
    :<|> "load" :> ReqBody '[JSON] SpielRead :> Post '[JSON] SpielResponse                -- /load (SpielRead -> SpielShare), loads prelude & gamefile content
    :<|> "runCode"  :> ReqBody '[JSON] SpielCommand :> Post '[JSON] SpielResponses        -- /runCode (SpielCommand -> SpielResponses) runs w/out a file, using Prelude and File code provided in the request
    :<|> "read" :> ReqBody '[JSON] SpielRead :> Post '[JSON] SpielFile                    -- /read (SpielRead -> SpielFile) reads from a file
    :<|> "test" :> Get '[JSON] SpielResponses                                             -- /test () nothing, just indicates it's running, good uptime-checker


-- defining the api
api :: Proxy SpielApi
api = Proxy


-- |handler for actual requests
-- performs mapping from endpoints to functions and responses
-- in order by which they are defined for the API above ^^^
handler :: Server SpielApi
handler = handleShareFile           -- /share handler
  :<|> handleLoad                   -- /load handler
  :<|> handleRunCode                -- /runCode handler
  :<|> handleReadFile               -- /read handler
  :<|> return (handleTest)          -- /test handler


-- | Prepares a server application
serverApp :: Application
serverApp = logStdoutDev . allowCsrf . corsified $ serve api handler


{--
-- setup a command api, for talking to the Repl
-- represents, 'POST/cmd' w/ a JSON object
-- describes a Command in the request body
-- and returns an encoded response
-- can test with the following CURL examples:

> /share
curl --verbose --request POST --header "Content-Type: application/json" --data '{"prelude":"prelude content","gamefile":"game file content"}' http://localhost:8080/share

> /runCode
curl --verbose --request POST --header "Content-Type: application/json" --data '{"file":"game Test\ntype Board = Array (10,10) of Int\ntype Input = Int\nhello : Int\nhello = 32","prelude":"testVal : Int\ntestVal = 2","input":"hello","buffer":[]}' http://localhost:8080/runCmds

> /read
curl --verbose --request POST --header "Content-Type: application/json" --data '{"path":"examples/TicTacToe"}' http://localhost:8080/read

> /test
curl --verbose http://localhost:8080/test
--}

startServer :: Int -> IO ()
startServer port = do
  putStrLn "Spiel Backend listening for POST/GET with endpoints:"
  putStrLn ("http://localhost:" ++ show port ++ "/share (POST)")
  putStrLn ("http://localhost:" ++ show port ++ "/load (POST)")
  putStrLn ("http://localhost:" ++ show port ++ "/runCode (POST)")
  putStrLn ("http://localhost:" ++ show port ++ "/read (POST)")
  putStrLn ("http://localhost:" ++ show port ++ "/test (GET)")
  putStrLn "Ready..."
  (run port serverApp)
