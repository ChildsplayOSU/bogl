{-# LANGUAGE DeriveGeneric #-}

-- | BOGL types

module Language.Types
  (BoardDef(..),
   InputDef(..),
   Btype(..),
   Xtype(..),
   Type(..),
   Ftype(..))
where

import Data.List
import Data.Array
import Data.Aeson
import GHC.Generics

import qualified Data.Set as S

type Name = String

-- | Board definition: mxn board of type Type
data BoardDef = BoardDef
  {
    size  :: (Int, Int)
  , piece :: Xtype
  }
  deriving (Generic)

instance Show BoardDef where
  show (BoardDef (i1, i2) t)
    = "Board : Grid(" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

-- | Input definition: Player inputs must be an accepted type
data InputDef = InputDef {inputType :: Xtype}
  deriving (Generic)

instance Show InputDef where
  show (InputDef t) = "Input : " ++ show t



-- Types
-- | Atomic types
data Btype = Booltype      -- ^ Boolean
           | Itype         -- ^ Int
           | AnySymbol     -- ^ this is the type all symbols live in
           | Input         -- ^ The input type specified at the top of the program
           | Board         -- ^ A game board
           | Player        -- ^ A player
           | Top           -- ^ Really this is bottom FIXME
           | Undef         -- ^ Only occurs when typechecking. The user cannot define anything of this type.
   deriving (Generic, Eq)


instance Ord Btype where
  Top <= _ = True
  x <= y   = x == y



instance Show Btype where
  show Booltype  = "Bool"
  show Itype     = "Int"
  show Top       = "T"
  show Input     = "Input"
  show Board     = "Board"
  show Player    = "Player"
  show AnySymbol = "AnySymbol"
  show Undef     = "?"

instance ToJSON Btype where


-- | Xtypes are sum types (or tuples of sum types), but restricted by the semantics to only contain Symbols after the atomic type.
--   Note: ttypes are subsumed by xtypes in our implementation
data Xtype = X Btype (S.Set Name)
           | Tup [Xtype]
           | Hole Name
  deriving (Generic, Eq)

instance Ord Xtype where
  (X Top _) <= (X AnySymbol _) = True -- A set of symbols is the subtype of AnySymbols
  (X k x) <= (X k' x') = (k <= k') && (x `S.isSubsetOf` x') --
  (Tup xs) <= (Tup xs') | length xs == length xs' = all (id) (zipWith (<=) xs xs')
  _ <= _ = False


instance Show Xtype where
  show (X b xs) | S.null xs = show b
                | otherwise =
                  case b of
                     Top -> showTypes
                     _   -> show b ++ " & " ++ showTypes
                     where
                        showTypes = "{" ++ intercalate (", ") (S.toList xs) ++ "}"
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"
  show (Hole n) = "?"
  show _ = undefined

instance ToJSON Xtype where

-- | A function type can be from a plain type to a plain type (no curried functions)
data Ftype = Ft Xtype Xtype
   deriving (Eq, Generic)
instance Ord Ftype where
  (Ft x y) <= (Ft z w) = x <= z && y <= w

instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

instance ToJSON Ftype where

-- | A type is either a plain type or a function.
data Type = Plain Xtype | Function Ftype
   deriving (Eq, Generic)

instance Ord Type where
  (Plain x) <= (Plain y) = x <= y
  (Function f) <= (Function g) = f <= g
  _ <= _ = False

p :: Btype -> Type
p b = Plain $ X b S.empty

instance Show Type where
  show (Plain t) = show t
  show (Function f) = show f

instance ToJSON Type where
