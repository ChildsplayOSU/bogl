{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Values in spiel

module Runtime.Values where

import Language.Syntax
import Data.Array
import Data.List
import Data.Aeson hiding (Array)
import GHC.Generics

type Board = Array (Int, Int) Val

-- | Values
data Val = Vi Int                      -- ^ Integer value
         | Vb Bool                     -- ^ Boolean value
         | Vboard Board                -- ^ Board value (displayed to user)
         | Vt [Val]                    -- ^ Tuple value
         | Vs Name                     -- ^ Symbol value
         | Vf [Name] EvalEnv (Expr ()) -- ^ Function value (annotations discarded)
         | Err String                  -- ^ Runtime error (caught by typechecker) 
         | Deferred                    -- ^ This needs an input.

instance ToJSON Val where
  toJSON (Vi i) = object ["type" .= String "Int", "value" .= i]
  toJSON (Vb b) = object ["type" .= String "Bool", "value" .= b]
  toJSON (Vboard b) = object ["type" .= String "Board", "value" .= assocs b]
  toJSON (Vt vs) = object ["type" .= String "Tuple", "value" .= map toJSON vs]
  toJSON (Vs n) = object ["type" .= String "Symbol", "value" .= n]
  toJSON (Vf args _ e) = object ["type" .= String "Function", "value" .= Null]
  toJSON (Err s) = object ["type" .= String "ERROR", "value" .= Null] -- null or something




-- | Can't compare two functions.
instance Eq Val where
  (Vi x) == (Vi y) = x == y
  (Vb b1) == (Vb b2) = b1 == b2
  (Vboard b1) == (Vboard b2) = b1 == b2
  (Vt x) == (Vt y) = x == y
  (Vs n) == (Vs n2) = n == n2
  _ == _ = False

type EvalEnv = [(Name, Val)]

instance Show Val where
  show (Vi i) = show i
  show (Vb b) = show b
  show (Vboard b) = "Board: " ++ show b
  show (Vt xs) = "(" ++ (intercalate ", " $ map show xs) ++ ")"
  show (Vs s) = s
  show (Vf xs env' e) = "\\" ++ show xs ++ " -> " ++ show e
  show (Err s) = "ERR: " ++ s
