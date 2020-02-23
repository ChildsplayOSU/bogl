{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Server.hs
--
-- Handles starting up an instance of Servant
-- Returns encoded board data on request, and computes
-- boards when input data is taken
--
module API.Server (startServer, serverApp) where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Parser.Parser

-- representation of input to the repl, from the user
data SpielCommand = SpielCommand {
    input :: String
  } deriving (Eq, Show)

-- representation of the response from the Repl
data SpielResponse = SpielResponse {
    response :: String
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
-- curl --verbose --request POST --header "Content-Type: application/json" --data '{"input":"arbitrary data"}' http://localhost:8080/spiel
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

-- runs a command
runCommand :: SpielCommand -> Handler SpielResponse
runCommand (SpielCommand cmd) = return (testResponse)
  -- TODO API is ready, the actual hooking in of what is sent should be done, here
  --case parseGame cmd of
  --Right x -> do
  --  case tcexpr (environment i b vs) x

-- returns a test reponse to the GET test endpoint, to ensure this is running
handleTestResponse :: Handler SpielResponse
handleTestResponse = return (SpielResponse "Spiel is Running!")


-- standard test response
testResponse :: SpielResponse
testResponse = SpielResponse "this is from Spiel's API"
