-- | Abstract syntax for BOGL

module Language.Syntax where
import Data.List
type Name = String

-- | Game datatype
data Game = Game Name BoardDef InputDef [ValDef]

instance Show Game where
  show (Game n b i vs) = "Game : " ++ n ++ "\n"
                         ++ show b ++ "\n"
                         ++ show i ++ "\n"
                         ++ intercalate ("\n\n\n") (map show vs)

-- | Board definition: mxn board of type Type
data BoardDef = BoardDef Integer Integer Type

instance Show BoardDef where
  show (BoardDef i1 i2 t) = "Board : Grid(" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

-- | Input definition: Player inputs must be an accepted type
data InputDef = InputDef Type

instance Show InputDef where
  show (InputDef t) = "Input : " ++ show t

-- | Top level values are signatures paired with either an ordinary 'Equation'
data ValDef = Val Signature Equation
  | BVal Signature BoardEq -- ^ Or a 'BoardEq'
   deriving (Eq)

instance Show ValDef where
  show (Val s e) = show s ++ "\n" ++ show e
  show (BVal s e) = show s ++ "\n" ++ show e

-- | Signatures are a product of name and type.
data Signature = Sig Name Type
   deriving (Eq) 

instance Show Signature where
  show (Sig n t) = n ++ " : " ++ show t

-- | Parameter lists are lists of 'Name'
data Parlist = Pars [Name]
   deriving (Eq) 

instance Show Parlist where
  show (Pars xs) = "(" ++ intercalate (" , ") (xs) ++ ")"

-- | Equations can either be
data Equation = Veq Name Expr -- ^ Value equations (a mapping from 'Name' to 'Expr')
              | Feq Name Parlist Expr -- ^ Function equations (a 'Name', list of params 'Parlist', and the 'Expr' that may possibly use those parameters.
   deriving (Eq)              

instance Show Equation where
  show (Veq n e) = n ++ " = " ++ show e
  show (Feq n p e) = n ++ show p ++ " = " ++ show e

-- | Board equations can either be
data BoardEq = PosDef Name Integer Integer Expr -- ^ Position defition: an assignment to a specific position
             | RegDef Name Expr Expr -- ^ A region definition, an assignment to multiple positions
   deriving (Eq)
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
   deriving (Eq) 

instance Show Btype where
  show Booltype = "Bool"
  show Itype = "Int"
  show (Symbol s) = "Symbol: " ++ s
  show Input = "Input"
  show Board = "Board"
  show Player = "Player"
  show Position = "Position"
  show Positions = "Positions"

-- | Xtypes are sum types, but restricted by the semantics to only contain Symbols after the atomic type.
data Xtype = X Btype [Btype]
   deriving (Eq) 

instance Show Xtype where
  show (X b []) = show b
  show (X b xs) = show b ++ "|" ++ intercalate ("|") (map show xs)

-- | Tuples can only contain Xtypes (no sub-tuples)
data Tuptype = Tup [Xtype]
   deriving (Eq) 

instance Show Tuptype where
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"

-- | A plain type is either a tuples, or an extended type
data Ptype = Pext Xtype | Pt Tuptype
   deriving (Eq) 

instance Show Ptype where
  show (Pext x) = show x
  show (Pt t) = show t

-- | A function type can be from a plain type to a plain type (no curried functions)
data Ftype = Ft Ptype Ptype
   deriving (Eq) 

instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

-- | A type is either a plain type or a function.
data Type = Plain Ptype | Function Ftype
   deriving (Eq) 

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
   deriving (Eq)
instance Show Expr where
  show (I i) = show i
  show (S s) = show s
  show (B b) = show b
  show (Ref n) = "ref " ++ n
  show (Tuple e) = "(" ++ intercalate " , " (map show e) ++ ")"
  show (App n es) = n ++ "(" ++ intercalate "," (map show es) ++ ")"
  show (Binop o e1 e2) = show e1 ++ show o ++ show e2
  show (Let n e1 e2) = "Let " ++ n ++ " = " ++ show e1 ++ " in " ++ show e2
  show (If e1 e2 e3) = "If " ++ show e1 ++ " Then " ++ show e2 ++ " Else " ++ show e3
  show (While e1 e2 x) = "While " ++ show e1 ++ " do " ++ show e2 ++ " to " ++ show x
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
   deriving (Eq) 
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
  show Xor = " xor "
