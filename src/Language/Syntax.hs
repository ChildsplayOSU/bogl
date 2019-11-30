-- | Syntax for BOGL

module Language.Syntax where

type Name = String

data Game = Game Name BoardDef InputDef ValDef

data BoardDef = BoardDef Int Int Type

data InputDef = InputDef Type

data ValDef = Val Signature Equation

data Signature = Sig Name Type

type Parlist = [Name]

data Equation = Veq Name Expr
              | Feq Name Parlist Expr
              | Beq [BoardEq]

data BoardEq = PosDef Name Int Int Expr
             | RegDef Name Expr Expr

-- Types

data Btype = Bool
           | Int
           | Symbol
           | Input Type
           | Board Int Int Type
           | Player
           | Position
           | Positions

data Xtype = BType | X [Name] -- Not sure what this is

data Tuptype = Tup [Xtype]

data Ptype = Xtype | Tuptype

data Ftype = Ft Ptype Ptype

data Type = Plain Ptype | Function Ftype

data Expr = I Int
          | S String
          | N Name
          | Tuple [Expr]
          | App Name [Expr]
          | Binop Expr Op Expr
          | Let Name Expr Expr
          | If Expr Expr Expr
          | While Expr Expr

data Op = Plus
        | Minus
        | Times
        | Div
        | Mod
        | Equiv
        | Or
        | And
        | Xor
