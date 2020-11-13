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
import qualified Data.Map.Strict as Map

-- | Representation of a Board in BoGL,
-- composed of an NxM array of 'Val'
type Board = Array (Int, Int) Val


-- | Typeclass for a runtime environment
class RuntimeEnv a where
  -- insert single key/val pair into the env
  insertEvalEnv :: (String,Val) -> a -> a
  -- extend an env with a tuple of keys and vals to be combined
  extendEvalEnv :: ([String],[Val]) -> a -> a
  -- combine two environments, producing a new env
  unionEvalEnv  :: a -> a -> a
  -- looks up a value in an env
  lookupEvalEnv :: String -> a -> Maybe Val


-- | Different kinds of evaluation environments
data EvalEnv = ListEvalEnv [(String,Val)] -- env represented with a list
  | MapEvalEnv (Map.Map String Val) -- env represented with a map
  deriving(Show)


-- | RuntimeEnv instance for EvalEnv
instance RuntimeEnv EvalEnv where
  insertEvalEnv pair (ListEvalEnv env) = ListEvalEnv $ pair : env
  insertEvalEnv (k,v) (MapEvalEnv env) = MapEvalEnv $ Map.insert k v env

  extendEvalEnv (keys,vals) (ListEvalEnv env) = ListEvalEnv $ (zip keys vals) ++ env
  extendEvalEnv (keys,vals) (MapEvalEnv env) = MapEvalEnv $ Map.union (Map.fromList (zip keys vals)) env

  unionEvalEnv (ListEvalEnv e1) (ListEvalEnv e2) = (ListEvalEnv $ e1 ++ e2)
  unionEvalEnv (MapEvalEnv e1)  (MapEvalEnv e2) = MapEvalEnv $ Map.union e1 e2
  unionEvalEnv _ _ = error "Cannot union environments of different values!"

  lookupEvalEnv n (ListEvalEnv e) = lookup n e
  lookupEvalEnv n (MapEvalEnv e) = Map.lookup n e


-- The base environment type that all other environments are built around
emptyEvalEnv :: EvalEnv
emptyEvalEnv = (MapEvalEnv Map.empty)


-- Produces an evaluation env from the base env type
evalEnvFromList :: [(String,Val)] -> EvalEnv
evalEnvFromList ls = extendEvalEnv (unzip ls) emptyEvalEnv


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
toGrid :: (Ix c, Ix b1) => Array (c, b1) b2 -> [[((c, b1), b2)]]
toGrid x = transpose (groupBy (\z y -> (fst . fst) z == (fst . fst) y) (assocs x))

instance ToJSON Val where
  toJSON (Vi i)        = object ["type" .= String "Int", "value" .= i]
  toJSON (Vb b)        = object ["type" .= String "Bool", "value" .= b]
  toJSON (Vboard b)    = object ["type" .= String "Board", "value" .= toGrid b]
  toJSON (Vt vs)       = object ["type" .= String "Tuple", "value" .= map toJSON vs]
  toJSON (Vs n)        = object ["type" .= String "Symbol", "value" .= n]
  toJSON (Vf _ _ _)    = object ["type" .= String "Function", "value" .= Null]
  toJSON (Pv _ _)      = object ["type" .= String "PendingValue", "value" .= Null]
  toJSON (Err s)       = object ["type" .= String "Error", "value" .= s] -- null or something
  toJSON (Deferred)    = object ["type" .= String "Deferred", "value" .= Null] -- deferred value needs an input

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
  show (Vf xs _ e)    = "\\" ++ show xs ++ " -> " ++ show e
  show (Pv _ e)       = show e
  show (Err s)        = "ERR: " ++ s
  show (Deferred)     = "Deferred"
