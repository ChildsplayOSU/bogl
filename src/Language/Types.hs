{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Language.Types
Description : BoGL Types
Copyright   : (c)
License     : BSD-3
-}

module Language.Types where

import Data.List
import Data.Aeson
import GHC.Generics

import qualified Data.Set as S

-- | Board definition
data BoardDef = BoardDef
  {
    size  :: (Int, Int) -- ^ size of the board
  , content :: Xtype    -- ^ type of values that can be on the board
  }
  deriving (Generic)

-- | Input definition
data InputDef = InputDef
  {
    inputType :: Xtype -- ^ Type of input, Player inputs must be an accepted type
  }
  deriving (Generic)

-- | Atomic types
data Btype = Booltype      -- ^ Boolean
           | Itype         -- ^ Int
           | AnySymbol     -- ^ this is the type all symbols live in
           | Input         -- ^ The input type specified at the top of the program
           | Board         -- ^ A game board
           | Top           -- ^ Really this is bottom FIXME
           | Undef         -- ^ Not definable by a user (only occurs when typechecking)
           | Named String
   deriving (Generic, Eq)

instance Ord Btype where
  Top <= _ = True
  x <= y   = x == y

-- | Xtypes are sum types (or tuples of sum types)
--   but restricted by the semantics to only contain Symbols after the atomic type.
--   Note: ttypes are subsumed by xtypes in our implementation
data Xtype = X Btype (S.Set String)
           | Tup [Xtype]
           | Hole String
  deriving (Generic, Eq)

instance Ord Xtype where
  (X Top _) <= (X AnySymbol _) = True -- A set of symbols is the subtype of AnySymbols
  (X k x)   <= (X k' x')       = (k <= k') && (x `S.isSubsetOf` x')
  (Tup xs)  <= (Tup xs') | length xs == length xs' = and (zipWith (<=) xs xs')
  _ <= _ = False

-- | A function type can be from a plain type to a plain type (no curried functions)
data Ftype = Ft Xtype Xtype
   deriving (Eq, Generic)

instance Ord Ftype where
  (Ft x y) <= (Ft z w) = x <= z && y <= w

-- | A type is either a plain type or a function.
data Type = Plain Xtype | Function Ftype
   deriving (Eq, Generic)

instance Ord Type where
  (Plain x)    <= (Plain y)    = x <= y
  (Function f) <= (Function g) = f <= g
  _ <= _ = False

-- | Nest a Btype as an Xtype
bnestx :: Btype -> Xtype
bnestx b = X b S.empty

-- | Xtype smart constructor for Booltype
boolxt :: Xtype
boolxt = bnestx Booltype

namedt :: String -> Xtype
namedt = bnestx . Named

-- | Xtype smart constructor for Itype
intxt :: Xtype
intxt = bnestx Itype

-- | Type smart constructor for Board
boardt :: Type
boardt = Plain boardxt

-- | Xtype smart constructor for Board
boardxt :: Xtype
boardxt = bnestx Board

-- | Xtype smart constructor for Input
inputxt :: Xtype
inputxt = bnestx Input

instance Show Xtype where
  show (X b xs) | S.null xs = show b
                | otherwise =
                  case b of
                     Top -> showTypes
                     _   -> show b ++ " & " ++ showTypes
                     where
                        showTypes = "{" ++ intercalate (", ") (S.toList xs) ++ "}"
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"
  show (Hole _) = "?"

instance ToJSON Xtype where

instance Show BoardDef where
  show (BoardDef (i1, i2) t)
    = "Board : Array (" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

instance Show InputDef where
  show (InputDef t) = "Input : " ++ show t

instance Show Btype where
  show Booltype  = "Bool"
  show Itype     = "Int"
  show Top       = "T"
  show Input     = "Input"
  show Board     = "Board"
  show AnySymbol = "AnySymbol"
  show Undef     = "?"
  show (Named s) = s

instance ToJSON Btype where

instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

instance ToJSON Ftype where

instance Show Type where
  show (Plain t) = show t
  show (Function f) = show f

instance ToJSON Type where
