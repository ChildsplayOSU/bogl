{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : API.CORSMiddleware
Description : CORS Middleware for the Spiel Server API
Copyright   : (c)
License     : BSD-3

Based on @https://stackoverflow.com/questions/41399055/haskell-yesod-cors-problems-with-browsers-options-requests-when-doing-post-req
Enables CORS requests to be made to this server by accepting all origins

-}

module API.CORSMiddleware where

import Network.Wai                       (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors)

-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)


--unused for the moment
--allowAll :: Origin
--allowAll = "http://localhost:3000"

-- | Cors resource policy to be used with 'corsified' middleware.
--
-- This policy will set the following:
--
-- * RequestHeaders: @Content-Type@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy {
    corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "POST"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = True
  , corsRequireOrigin  = False
  , corsIgnoreFailures = True
}
