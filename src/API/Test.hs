--
-- Test.hs
--
-- Endpoint for testing that the server is running
--

module API.Test (handleTest) where

import API.JSONData

-- returns a test reponse to the GET test endpoint, to ensure this is running
handleTest :: SpielResponses
handleTest = [(Log "Spiel-Lang is Running Normally")]
