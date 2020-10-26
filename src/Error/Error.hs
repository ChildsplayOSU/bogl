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

-- Note: the error code functionality works, but only for type errors so they are not displayed
instance Show Error where
   show (Error (TE e) _ p) = "Type error in " ++ srcname ++ show e
      where
         srcname = case sourceName p of
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
cterr terr = Error (TE terr) (assign terr)
   where
      assign te = case te of
                    (Mismatch _ _ _)      -> 0
                    (AppMismatch _ _ _ _) -> 1
                    (NotBound _)          -> 2
                    (SigMismatch _ _ _)   -> 3
                    (Unknown _ )          -> 4
                    (BadOp _ _ _ _)       -> 5
                    (OutOfBounds _ _)     -> 6
                    (BadApp _ _)          -> 7
                    (Dereff _ _)          -> 8
                    (Uninitialized _)     -> 10
                    (SigBadFeq _ _ _)     -> 11
                    (InputMismatch _ _)   -> 12
