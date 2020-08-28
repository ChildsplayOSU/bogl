{-|
Module      : API.Test
Description : Endpoint for testing that the server is running
Copyright   : (c)
License     : BSD-3
-}

module API.Test (handleTest) where

import API.JSONData

-- | Returns a test reponse to the GET test endpoint, to ensure this is running
handleTest :: SpielResponses
handleTest = [(Log "Spiel-Lang is Running Normally")]
