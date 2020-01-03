-- | Syntax for BOGL

module Language.Syntax where
import Data.List
type Name = String

data Game = Game Name BoardDef InputDef [ValDef]

instance Show Game where
  show (Game n b i vs) = "Game : " ++ n ++ "\n"
                         ++ show b ++ "\n"
                         ++ show i ++ "\n"
                         ++ intercalate ("\n\n\n") (map show vs)

data BoardDef = BoardDef Integer Integer Type

instance Show BoardDef where
  show (BoardDef i1 i2 t) = "Board : Grid(" ++ show i1 ++ "," ++ show i2 ++ ") of " ++ show t

data InputDef = InputDef Type

instance Show InputDef where
  show (InputDef t) = "Input : " ++ show t

data ValDef = Val Signature Equation | BVal Signature BoardEq

instance Show ValDef where
  show (Val s e) = show s ++ "\n" ++ show e
  show (BVal s e) = show s ++ "\n" ++ show e

data Signature = Sig Name Type

instance Show Signature where
  show (Sig n t) = n ++ " : " ++ show t

data Parlist = Pars [Name]

instance Show Parlist where
  show (Pars xs) = "(" ++ intercalate (" , ") (xs) ++ ")"

data Equation = Veq Name Expr
              | Feq Name Parlist Expr
             
instance Show Equation where
  show (Veq n e) = n ++ " = " ++ show e
  show (Feq n p e) = n ++ show p ++ " = " ++ show e


data BoardEq = PosDef Name Integer Integer Expr
             | RegDef Name Expr Expr
             -- How does this work?
instance Show BoardEq where
  show (PosDef n i1 i2 e) = n ++ "(" ++ show i1 ++ ", " ++ show i2 ++ ")" ++ "=" ++ show e
  show (RegDef n e1 e2) = n ++ "(" ++ show e1 ++ ")" ++ "=" ++ show e2

-- Types

data Btype = Booltype
           | Itype
           | Symbol
           | Input
           | Board
           | Player
           | Position
           | Positions
           deriving Eq
instance Show Btype where
  show Booltype = "Bool"
  show Itype = "Int"
  show Symbol= "Symbol"
  show Input = "Input"
  show Board = "Board"
  show Player = "Player"
  show Position = "Position"
  show Positions = "Positions"

data Xtype = X Btype [Name]
  deriving Eq
instance Show Xtype where
  show (X b []) = show b
  show (X b xs) = show b ++ "|" ++ intercalate ("|") (xs)

data Tuptype = Tup [Xtype]
  deriving Eq
instance Show Tuptype where
  show (Tup xs) = "(" ++ intercalate (",") (map show xs) ++ ")"

data Ptype = Pext Xtype | Pt Tuptype
  deriving Eq
instance Show Ptype where
  show (Pext x) = show x
  show (Pt t) = show t

data Ftype = Ft Ptype Ptype
  deriving Eq
instance Show Ftype where
  show (Ft t1 t2) = show t1 ++ " -> " ++ show t2

data Type = Plain Ptype | Function Ftype
  deriving Eq
instance Show Type where
  show (Plain t) = show t
  show (Function f) = show f

data Expr = I Integer
          | S String
          | B Bool
          | Ref Name
          | Tuple [Expr]
          | App Name [Expr]
          | Binop Op Expr Expr
          | Let Name Expr Expr
          | If Expr Expr Expr
          | While Expr Expr
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
  show (While e1 e2) = "While " ++ show e1 ++ " do " ++ show e2

data Op = Plus
        | Minus
        | Times
        | Div
        | Mod
        | Equiv
        | Or
        | And
        | Xor
instance Show Op where
  show Plus = " + "
  show Minus = " - "
  show Times = " * "
  show Div = " / "
  show Mod = " % "
  show Equiv = " == "
  show Or = " or "
  show And = " and "
  show Xor = " xor "
