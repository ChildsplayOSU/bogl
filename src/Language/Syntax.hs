{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax for BOGL

module Language.Syntax where
import Data.List
import Text.JSON.Generic
import qualified Data.Set as S
type Name = String

-- | Game datatype
data Game = Game Name BoardDef InputDef [ValDef]
  deriving (Data)

instance Show Game where
  show (Game n b i vs) = "Game : " ++ n ++ "\n"
                         ++ show b ++ "\n"
                         ++ show i ++ "\n"
                         ++ intercalate ("\n\n\n") (map show vs)

-- | Board definition: mxn board of type Type
data BoardDef = BoardDef Integer Integer Type
  deriving (Data)

instance Show BoardDef where
  show (BoardDef i1 i2 t) = "Board : Grid(" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

-- | Input definition: Player inputs must be an accepted type
data InputDef = InputDef Type
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
data Equation = Veq Name Expr -- ^ Value equations (a mapping from 'Name' to 'Expr')
              | Feq Name Parlist Expr -- ^ Function equations (a 'Name', list of params 'Parlist', and the 'Expr' that may possibly use those parameters.
   deriving (Eq, Data)

instance Show Equation where
  show (Veq n e) = n ++ " = " ++ show e
  show (Feq n p e) = n ++ show p ++ " = " ++ show e

-- | Board equations can either be
data BoardEq = PosDef Name Expr Expr Expr -- ^ Position defition: an assignment to a specific position
             | RegDef Name Expr Expr -- ^ A region definition, an assignment to multiple positions
   deriving (Eq, Data)

instance Show BoardEq where
  show (PosDef n i1 i2 e) = n ++ "(" ++ show i1 ++ ", " ++ show i2 ++ ")" ++ "=" ++ show e
  show (RegDef n e1 e2) = n ++ "(" ++ show e1 ++ ")" ++ "=" ++ show e2

-- Types
-- | Atomic types
data Btype = Booltype -- ^ Boolean
           | Itype -- ^ Integer
           | Symbol Name -- ^ Symbols, or nullary constructors. Each symbol lives in its own unique type.
           | Input -- ^ The input type specified at the top of the program
           | Board -- ^ A game board
           | Player -- ^ A player
           | Position -- ^ A position, specified by the board description
           | Positions -- ^ The list of all positions
           | Undef -- ^ Only occurs when typechecking. The user cannot define anything of this type. (I could use 'undefined' everywhere I use this, but one false move and the whole program crashes)
   deriving (Eq, Data)

instance Show Btype where
  show Booltype = "Bool"
  show Itype = "Int"
  show (Symbol s) = s
  show Input = "Input"
  show Board = "Board"
  show Player = "Player"
  show Position = "Position"
  show Positions = "Positions"
  show Undef = "?"

-- | Xtypes are sum types, but restricted by the semantics to only contain Symbols after the atomic type.
data Xtype = X Btype (S.Set Name)
  deriving (Data)

instance Eq Xtype where
  -- (X (Symbol s) bs) == (X t1 xs) | not . S.null $ xs= s `S.member` xs
  -- (X t1 xs) == (X (Symbol s) bs) | not . S.null $ xs= s `S.member` xs
  -- (X t1 empty) == (X t2 bs) | S.null empty = t2 == t1 -- type promotion (maybe remove?)
  -- (X t2 bs) == (X t1 empty) | S.null empty = t2 == t1 -- type demotion
  (X a1 b1) == (X a2 b2) = a1 == a2 && b1 == b2

instance Show Xtype where
  show (X b xs) | S.null xs = show b
                | otherwise = show b ++ "|" ++ intercalate ("|") (map show (S.toList xs))

-- | Tuples can only contain Xtypes (no sub-tuples)
data Tuptype = Tup [Xtype]
   deriving (Eq, Data)

instance Show Tuptype where
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"

-- | A plain type is either a tuple, or an extended type
data Ptype = Pext Xtype | Pt Tuptype
   deriving (Eq, Data)

instance Show Ptype where
  show (Pext x) = show x
  show (Pt t) = show t

-- | A function type can be from a plain type to a plain type (no curried functions)
data Ftype = Ft Ptype Ptype
   deriving (Eq, Data)

instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

-- | A type is either a plain type or a function.
data Type = Plain Ptype | Function Ftype
   deriving (Eq, Data)

instance Show Type where
  show (Plain t) = show t
  show (Function f) = show f

-- | Expressions
data Expr = I Integer -- ^ Integer expression
          | S Name -- ^ Symbol
          | B Bool -- ^ Boolean
          | Ref Name -- ^ Reference to a variable
          | Tuple [Expr] -- ^ Tuple of 'Expr'
          | App Name [Expr] -- ^ Application of the function called Name to the list of arguments (Note: this could also be App Expr Expr, which would be cleaner.)
          | Binop Op Expr Expr -- ^ Binary operation of two expressions
          | Let Name Expr Expr -- ^ Let binding
          | If Expr Expr Expr -- ^ Conditional expression
          | While Name Name Expr -- ^ While loop (could be While Expr Expr Expr if we make the App change suggested above)
          | Case Name [(Name, Expr)] Expr -- ^ case expression: the final pair is if we have the atomic type, and then we downcast the Xtype back to its regular form.
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
  show (While e1 e2 x) = "While " ++ show e1 ++ " do " ++ show e2 ++ " to " ++ show x
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
   deriving (Eq, Data)
instance Show Op where
  show Plus = " + "
  show Minus = " - "
  show Times = " * "
  show Div = " / "
  show Mod = " % "
  show Equiv = " == "
  show Or = " or "
  show And = " and "
  show Less = " < "
  show Greater = " > "
  show Xor = " xor "
