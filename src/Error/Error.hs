{-# LANGUAGE OverloadedStrings #-}

{-|
Description : The top-level error data types. Specific error types are nested within these.
-}

module Error.Error where

import Error.TypeError
import Data.Aeson hiding (Error)

import Text.Parsec.Pos

-- | A top-level error data type that contains information that all errors should have
data Error = Error {
                     err  :: Err          -- ^ more specific error information
                   , eid  :: Int          -- ^ error id; for future use
                   , epos :: SourcePos    -- ^ error position
                   }
   deriving (Eq)

-- Note: the error code functionality works, but only for type errors
-- the code below can be uncommented when it is introduced holistically
instance Show Error where
   show (Error (TE e) i p) = {-ec ++ "TE" ++ show i ++ "\n" ++-} "Type error in " ++ name ++ show e
      where
         -- ec = "Error Code "
         name = case sourceName p of
                  "" -> "the interpreter input " ++ show p ++ "\n"
                  _  -> show p ++ "\n"

instance ToJSON Error where
   toJSON e@(Error _ _ p) = object [
                                     "message" .= show e
                                   , "line"    .= sourceLine p
                                   , "col"     .= sourceColumn p
                                   ]

-- | A combination of the different error categories
data Err = TE TypeError {- PE ParseError | RE RuntimeError -- to be added in the future -}
   deriving (Eq)

instance Show Err where
   show (TE t) = show t

-- | Smart constructor for a type error that assigns an id
cterr :: TypeError -> SourcePos -> Error
cterr e = Error (TE e) (assign e)
   where
      assign (Mismatch _ _ _)      = 0
      assign (AppMismatch _ _ _ _) = 1
      assign (NotBound _)          = 2
      assign (SigMismatch _ _ _)   = 3
      assign (Unknown _ )          = 4
      assign (BadOp _ _ _ _)       = 5
      assign (OutOfBounds _ _)     = 6
      assign (BadApp _ _)          = 7
      assign (Dereff _ _)          = 8
      assign (Uninitialized _)     = 9
