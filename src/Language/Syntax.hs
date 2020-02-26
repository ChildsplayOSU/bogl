{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax for BOGL

module Language.Syntax where

import Data.List
import Text.JSON.Generic
import Data.Array
import qualified Data.Set as S
type Name = String

-- | Game datatype
data Game = Game
  {
    name  :: Name
  , board :: BoardDef
  , input :: InputDef
  , defns ::[ValDef]
  }
  deriving (Data)

instance Show Game where
  show (Game n b i vs) = "Game : " ++ n ++ "\n"
                         ++ show b ++ "\n"
                         ++ show i ++ "\n"
                         ++ intercalate ("\n\n\n") (map show vs)

-- | Board definition: mxn board of type Type
data BoardDef = BoardDef
  {
    size  :: (Int, Int)
  , piece :: Xtype
  }
  deriving (Data)

instance Show BoardDef where
  show (BoardDef (i1, i2) t)
    = "Board : Grid(" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

-- | Input definition: Player inputs must be an accepted type
data InputDef = InputDef Xtype
  deriving (Data)

instance Show InputDef where
  show (InputDef t) = "Input : " ++ show t

-- | Top level values are signatures paired with either an ordinary 'Equation'
data ValDef = Val Signature Equation
  | BVal Signature BoardEq -- ^ Or a 'BoardEq'
   deriving (Eq, Data)

instance Show ValDef where
  show (Val s e) = show s ++ "\n" ++ show e
  show (BVal s e) = show s ++ "\n" ++ show e

ident :: ValDef -> Name
ident (Val (Sig n _) _) = n
ident (BVal (Sig n _) _) = n

-- | Signatures are a product of name and type.
data Signature = Sig Name Type
   deriving (Eq, Data)

instance Show Signature where
  show (Sig n t) = n ++ " : " ++ show t

-- | Parameter lists are lists of 'Name'
data Parlist = Pars [Name]
   deriving (Eq, Data)

instance Show Parlist where
  show (Pars xs) = "(" ++ intercalate (" , ") (xs) ++ ")"

-- | Equations can either be
data Equation = Veq Name Expr         -- ^ Value equations (a mapping from 'Name' to 'Expr')
              | Feq Name Parlist Expr -- ^ Function equations (a 'Name', list of params 'Parlist', and the 'Expr' that may use them
   deriving (Eq, Data)

instance Show Equation where
  show (Veq n e) = n ++ " = " ++ show e
  show (Feq n p e) = n ++ show p ++ " = " ++ show e

-- | Board equations can either be
--data BoardEq = PosDef Name Expr Expr Expr -- ^ Position defition: an assignment to a specific position
--             | RegDef Name Expr Expr -- ^ A region definition, an assignment to multiple positions
data BoardEq = PosDef Name Pos Pos Expr
   deriving (Eq, Data)

instance Show BoardEq where 
   show (PosDef n x y e) = n ++ "(" ++ show x ++ ", " ++ show y ++ ")" ++ " = " ++ show e 

-- Types
-- | Atomic types
data Btype = Booltype      -- ^ Boolean
           | Itype         -- ^ Integer
           | AnySymbol     -- ^ this is the type all symbols live in
           | Input         -- ^ The input type specified at the top of the program
           | Board         -- ^ A game board
           | Player        -- ^ A player
           | Position      -- ^ A position, specified by the board description
           | Positions     -- ^ The list of all positions
           | Top           -- ^ Really this is bottom FIXME
           | Undef         -- ^ Only occurs when typechecking. The user cannot define anything of this type.
   deriving (Data, Eq)


instance Ord Btype where
  Top <= _ = True
  x <= y   = x == y



instance Show Btype where
  show Booltype = "Bool"
  show Itype = "Int"
  show Top = "T"
  show Input = "Input"
  show Board = "Board"
  show Player = "Player"
  show Position = "Position"
  show Positions = "Positions"
  show AnySymbol = "AnySymbol"
  show Undef = "?"


-- | Convert a btype to an unextended Xtype
ext :: Btype -> Xtype
ext b = (X b S.empty)

-- | Xtypes are sum types (or tuples of sum types), but restricted by the semantics to only contain Symbols after the atomic type.
data Xtype = X Btype (S.Set Name) | Tup [Xtype] | X_ Xtype
  deriving (Data, Eq)

instance Ord Xtype where
  (X Top _) <= (X AnySymbol _) = True -- A set of symbols is the subtype of AnySymbols
  (X k x) <= (X k' x') = (k <= k') && (x `S.isSubsetOf` x') -- If k is a subtype of k' (they either match or one is Top), and x is a subset
  (Tup xs) <= (Tup xs') = all (id) (zipWith (<=) xs xs')
  _ <= _ = False


instance Show Xtype where
  show (X b xs) | S.null xs = show b ++ "(no extension)"
                | otherwise = show b ++ "|" ++ intercalate ("|") (map show (S.toList xs))
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"
  show _ = undefined


-- | A function type can be from a plain type to a plain type (no curried functions)
data Ftype = Ft Xtype Xtype
   deriving (Eq, Data)
instance Ord Ftype where
  (Ft x y) <= (Ft z w) = x <= z && y <= w

instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

-- | A type is either a plain type or a function.
data Type = Plain Xtype | Function Ftype
   deriving (Eq, Data)

instance Ord Type where
  (Plain x) <= (Plain y) = x <= y
  (Function f) <= (Function g) = f <= g
  _ <= _ = False

p :: Btype -> Type
p b = Plain $ X b S.empty

instance Show Type where
  show (Plain t) = show t
  show (Function f) = show f

-- | Positions are either
data Pos = Index Int 
         | ForAll      
         deriving (Eq, Show, Data)

instance Ord Pos where
  compare (Index i) (Index j) = compare i j
  compare (ForAll) (_) = LT
  compare (_) (ForAll) = GT

 -- | Expressions
data Expr = I Integer                     -- ^ Integer expression
          | S Name                        -- ^ Symbol
          | B Bool                        -- ^ Boolean
          | Ref Name                      -- ^ Reference to a variable
          | Tuple [Expr]                  -- ^ Tuple of 'Expr'
          | App Name [Expr]                -- ^ Application of the function called Name to the list of arguments
          | Binop Op Expr Expr            -- ^ Binary operation of two expressions
          | Let Name Expr Expr            -- ^ Let binding
          | If Expr Expr Expr             -- ^ Conditional expression
          | Abs [Name] Expr
          | AppAbs [Expr] Expr
          | Case Name [(Name, Expr)] Expr -- ^ case expression: the final pair is if we have the atomic type, and then we downcast the Xtype back to its regular form.
          | While Expr Expr [Name] Expr   -- ^ While: condition, body, names of arguments from the wrapper function, (tuple of) expression(s) which referenc(es) the name(s).
          | Hole
          -- the last Expr can always be constructed from the [Name], but it makes the code cleaner to do that only once while parsing 
   deriving (Eq, Data)

instance Show Expr where
  show (I i) = show i
  show (S s) = s
  show (B b) = show b
  show (Ref n) = n
  show (Tuple e) = "(" ++ intercalate " , " (map show e) ++ ")"
  show (App n es) = n ++ "(" ++ intercalate "," (map show es) ++ ")"
  show (Binop o e1 e2) = show e1 ++ show o ++ show e2
  show (Let n e1 e2) = "Let " ++ n ++ " = " ++ show e1 ++ " in " ++ show e2
  show (If e1 e2 e3) = "If " ++ show e1 ++ " Then " ++ show e2 ++ " Else " ++ show e3
  show (While c b n e ) = "While " ++ show c ++ " do " ++ show b ++ "(with names, values from wrapper: " ++ show n ++ ", " ++ show e ++ ")" 
  show (Case n xs e) = "case " ++ n ++ " of" ++ (intercalate "\n" (map show xs)) ++ "otherwise: " ++ show e

-- | Binary operations
data Op = Plus
        | Minus
        | Times
        | Div
        | Mod
        | Equiv
        | Or
        | And
        | Less
        | Xor
        | Greater
        | Get           -- Gets contents from a position on a board 
   deriving (Eq, Data)

instance Show Op where
  show Plus     = " + "
  show Minus    = " - "
  show Times    = " * "
  show Div      = " / "
  show Mod      = " % "
  show Equiv    = " == "
  show Or       = " or "
  show And      = " and "
  show Less     = " < "
  show Xor      = " xor "
  show Greater  = " > "
  show Get      = " ! " 
