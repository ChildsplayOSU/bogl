--
-- ReadFile.hs
--
-- API Endpoint that allows reading a .bgl file
--

module API.ReadFile (handleReadFile) where

import API.JSONData
import Servant
import Control.Monad.IO.Class

-- | handles reading a file and returning it's
-- contents to the requester
handleReadFile :: SpielRead -> Handler SpielFile
handleReadFile rf = do
  (liftIO (_handleReadFile rf))


-- internally attempts to read and return a file
_handleReadFile :: SpielRead -> IO SpielFile
_handleReadFile (SpielRead fn) = do
  contents <- readFile (fn ++ ".bgl")
  return (SpielFile fn contents)
