-- | Syntax for BOGL

module Language.Syntax where

type Name = String

data Game = Game Name BoardDef InputDef [ValDef]

data BoardDef = BoardDef Integer Integer Type

data InputDef = InputDef Type

data ValDef = Val Signature Equation

data Signature = Sig Name Type

data Parlist = Pars [Name]

data Equation = Veq Name Expr
              | Feq Name Parlist Expr
             

data BoardEq = PosDef Name Int Int Expr
             | RegDef Name Expr Expr
             -- How does this work?

-- Types

data Btype = Booltype
           | Itype
           | Symbol
           | Input
           | Board
           | Player
           | Position
           | Positions

data Xtype = X Btype [Name] -- Not sure what this is

data Tuptype = Tup [Xtype]

data Ptype = Pext Xtype | Pt Tuptype

data Ftype = Ft Ptype Ptype

data Type = Plain Ptype | Function Ftype

data Expr = I Integer
          | S String
          | B Bool
          | N Name
          | Tuple [Expr]
          | App Name [Expr]
          | Binop Op Expr Expr
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
