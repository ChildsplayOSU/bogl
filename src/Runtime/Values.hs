{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Runtime.Values
Description : Values in BoGL
Copyright   : (c)
License     : BSD-3
-}

module Runtime.Values where

import Language.Syntax hiding (ValDef())
import Data.Array
import Data.List
import Data.Aeson hiding (Array)
import GHC.Generics

-- | Representation of a Board in BoGL,
-- composed of an NxM array of 'Val'
type Board = Array (Int, Int) Val

-- | Evaluation environment
type EvalEnv = [(Name, Val)]

-- | Runtime values that can be encountered
data Val = Vi Int                      -- ^ Integer value
         | Vb Bool                     -- ^ Boolean value
         | Vboard Board                -- ^ Board value (displayed to user)
         | Vt [Val]                    -- ^ Tuple value
         | Vs Name                     -- ^ Symbol value
         | Vf [Name] EvalEnv (Expr ()) -- ^ Function value (annotations discarded)
         | Pv EvalEnv (Expr ())        -- ^ Deannotated pending value allows for 'input' in Val Eqs
         | Err String                  -- ^ Runtime error (caught by typechecker)
         | Deferred                    -- ^ This needs an input.
         deriving Generic

-- | Encodes a 1D board array as a string
encode1DArray :: [((Int,Int),Val)] -> String
encode1DArray [] = ""
encode1DArray ((_,val):ls) = "\"" ++ (show val) ++ "\"" ++
                                     (if length ls > 0 then "," else "") ++ (encode1DArray ls)

-- | Encodes a 2D board array as a string
encode2DArray :: [[((Int, Int), Val)]] -> String
encode2DArray [] = ""
encode2DArray (ar:ls) = "[" ++ (encode1DArray ar) ++ "]"
                            ++ (if length ls > 0 then "," else "") ++ (encode2DArray ls)

-- | Convert an array to a grid
toGrid x = transpose (groupBy (\x y -> (fst . fst) x == (fst . fst) y) (assocs x))

instance ToJSON Val where
  toJSON (Vi i)        = object ["type" .= String "Int", "value" .= i]
  toJSON (Vb b)        = object ["type" .= String "Bool", "value" .= b]
  toJSON (Vboard b)    = object ["type" .= String "Board", "value" .= toGrid b]
  toJSON (Vt vs)       = object ["type" .= String "Tuple", "value" .= map toJSON vs]
  toJSON (Vs n)        = object ["type" .= String "Symbol", "value" .= n]
  toJSON (Vf args _ e) = object ["type" .= String "Function", "value" .= Null]
  toJSON (Pv _ e)      = object ["type" .= String "PendingValue", "value" .= Null]
  toJSON (Err s)       = object ["type" .= String "Error", "value" .= s] -- null or something

-- | Can't compare two functions.
instance Eq Val where
  (Vi x) == (Vi y)           = x == y
  (Vb b1) == (Vb b2)         = b1 == b2
  (Vboard b1) == (Vboard b2) = b1 == b2
  (Vt x) == (Vt y)           = x == y
  (Vs n) == (Vs n2)          = n == n2
  _ == _                     = False

instance Show Val where
  show (Vi i)         = show i
  show (Vb b)         = show b
  show (Vboard b)     = "Board: " ++ show b
  show (Vt xs)        = "(" ++ (intercalate ", " $ map show xs) ++ ")"
  show (Vs s)         = s
  show (Vf xs env' e) = "\\" ++ show xs ++ " -> " ++ show e
  show (Pv env' e)    = show e
  show (Err s)        = "ERR: " ++ s
