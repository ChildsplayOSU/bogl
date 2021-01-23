{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Language.Syntax
Description : BoGL Syntax
Copyright   : (c)
License     : BSD-3

Spiel language AST. This slightly deviates from the specified syntax in spots.

-}

module Language.Syntax where

import Language.Types
import Data.List
import GHC.Generics

import Utils.String

-- | Names of games, signatures, etc.
type Name = String

-- | An unparsed expression
type ExprS = String

-- | A type definition or synonym
type TypeDef = (Name, Xtype)

-- | Game datatype
data Game a = Game
  {
    name  :: Name       -- ^ The name of the game
  , board :: BoardDef   -- ^ Size and type of the board
  , input :: InputDef   -- ^ Type of input
  , defns :: [ValDef a] -- ^ List of value definitions
  , tdefs :: [TypeDef]
  }
  deriving (Generic)

-- | Signatures are a product of name and type.
data Signature = Sig Name Type
   deriving (Eq)

-- | Parameters are lists of 'Name'
data Parlist = Pars [Name]
   deriving (Eq, Generic)

-- | Top level values are signatures paired with either an ordinary Equation or a list of Board Equations
data ValDef a = Val Signature (Equation a) a -- ^ Regular Value
              | BVal Signature [BoardEq a] a -- ^ Board value
   deriving (Eq, Generic)

instance Functor ValDef where
  fmap f (Val s e a)  = Val s (fmap f e) (f a)
  fmap f (BVal s e a) = BVal s ((fmap . fmap) f e) (f a)

-- | Equations
data Equation a = Veq Name (Expr a)          -- ^ Value equations (a mapping from 'Name' to 'Expr')
                | Feq Name Parlist (Expr a)  -- ^ Function equations
   deriving (Eq, Generic)

instance Functor Equation where
  fmap f (Veq n e)    = Veq n (fmap f e)
  fmap f (Feq n ps e) = Feq n ps (fmap f e)

-- | Board equations are used to set positions on the board to an expression
data BoardEq a = PosDef
   {
    boardEqName :: Name,
    xpos :: Pos,
    ypos :: Pos,
    boardExpr :: (Expr a)
   }
   deriving (Eq, Generic)

instance Functor BoardEq where
  fmap f (PosDef n p1 p2 e) = PosDef n p1 p2 (fmap f e)

-- | Types of individual positions for the x and y in a board equation
data Pos = Index Int    -- ^ Singular index as
         | ForAll Name  -- ^ All indices with a given name
         deriving (Eq, Generic)

instance Ord Pos where
  compare (Index i) (Index j)       = compare i j
  compare (ForAll _) (Index i)      = if i <= 0 then GT else LT
  compare ix@(Index _) f@(ForAll _) = compare f ix
  compare (ForAll _) (ForAll _)     = EQ

-- | Expressions
data Expr a = I Int                                 -- ^ Integer
          | S Name                                  -- ^ Symbol
          | B Bool                                  -- ^ Boolean
          | Ref Name                                -- ^ Reference to a variable
          | Tuple [Expr a]                          -- ^ Tuple of 'Expr'
          | App Name (Expr a)                       -- ^ Function application with argument tuple
          | Binop Op (Expr a) (Expr a)              -- ^ Binary operation
          | Let Name (Expr a) (Expr a)              -- ^ Let binding
          | While (Expr a) (Expr a) [Name] (Expr a) -- ^ While loop
          | If (Expr a) (Expr a) (Expr a)           -- ^ Conditional
          | Annotation a (Expr a)                   -- ^ Parameterized by the type of an annotation
          | HE Name                                 -- ^ Type hole
   deriving (Eq, Generic)

-- Description of arguments to While:
-- condition, body, names of arguments from the wrapper function,
-- expressions which reference the names.
-- the last Expr can always be constructed from the [Name], but it makes the code cleaner to do that
-- only once while parsing

-- | this is just "deriving functor"
instance Functor Expr where
  fmap _ (B x)               = (B x)
  fmap _ (HE n)              = (HE n)
  fmap f (Annotation a e)    = Annotation (f a) (fmap f e)
  fmap f (While e1 e2 ns e3) = While (fmap f e1) (fmap f e2) ns (fmap f e3)
  fmap f (If e1 e2 e3)       = If (fmap f e1) (fmap f e2) (fmap f e3)
  fmap f (Let n e1 e2)       = Let n (fmap f e1) (fmap f e2)
  fmap f (Binop o e1 e2)     = Binop o (fmap f e1) (fmap f e2)
  fmap f (App n es)          = App n (fmap f es)
  fmap f (Tuple xs)          = Tuple (fmap (fmap f) xs)
  fmap _ (Ref n)             = (Ref n)
  fmap _ (S n)               = (S n)
  fmap _ (I x)               = (I x)

-- | Binary operations
data Op = Plus      -- ^ Addition (+)
        | Minus     -- ^ Subtraction (-)
        | Times     -- ^ Multiplication (*)
        | Div       -- ^ Division (/)
        | Mod       -- ^ Modulus (%)
        | Less      -- ^ Less than comparison (<)
        | Leq       -- ^ Less than equal to comparison (<=)
        | Equiv     -- ^ Equivalent comparison (==)
        | NotEquiv  -- ^ Not equivalent comparison (/=)
        | Geq       -- ^ Greater than equal to comparison (>=)
        | Greater   -- ^ Greater than comparison (>)
        | Get       -- ^ Gets contents from a position on a board (!)
   deriving (Eq, Generic)

-- Note: the three predicates below are used in the type checker

-- | Check if a binary operator is arithmetic
arithmetic :: Op -> Bool
arithmetic o = o `elem` [Plus, Minus, Times, Div, Mod]

-- | Check if a binary operator is relational (excluding == and /=)
relational :: Op -> Bool
relational o = o `elem` [Less, Leq, Geq, Greater]

-- | Check if a binary operator is == or /=
equiv :: Op -> Bool
equiv o = o `elem` [Equiv, NotEquiv]

-- | Deannotate an expression
deAnnotate :: Expr a -> Expr a
deAnnotate (Annotation _ e) = e
deAnnotate x = x

-- | Clear the annotation from an expression
clearAnn :: Expr a -> Expr ()
clearAnn = (() <$)

-- | Clear the annotation from an equation
clearAnnEq :: Equation a -> Equation ()
clearAnnEq = (() <$)

-- | Get the identifier from a ValDef
ident :: (ValDef a) -> Name
ident (Val (Sig n _) _ _)  = n
ident (BVal (Sig n _) _ _) = n

instance Show (BoardEq a) where
   show (PosDef n x y e) = n ++ "(" ++ show x ++ ", " ++ show y ++ ")" ++ " = " ++ show e

instance Show Pos where
   show (Index i)  = show i
   show (ForAll n) = n

instance Show (Equation a) where
  show (Veq n e)   = n ++ " = " ++ show e
  show (Feq n pl e) = n ++ show pl ++ " = " ++ show e

instance Show (Expr a) where
  show (Annotation _ e)     = show e -- can refactor
  show (HE n)               = "?" ++ n
  show (I i)                = show i
  show (S s)                = s
  show (B b)                = show b
  show (Ref n)              = n
  show (Tuple e)            = showAsTuple (map show e)
  show (App n e@(Tuple _))  = n ++ show e
  show (App n e)            = n ++ parenthesize (show e)
  show (Binop o e1 e2)      = show e1 ++ show o ++ show e2
  show (Let n e1 e2)        = "let " ++ n ++ " = " ++ show e1 ++ " in " ++ show e2
  show (If e1 e2 e3)        = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (While c b _ _)      = "while " ++ show c ++ " do " ++ show b

instance Show (ValDef a) where
  show (Val s e _)  = show s ++ "\n" ++ show e
  show (BVal s e _) = show s ++ "\n" ++ show e

instance Show Parlist where
  show (Pars xs) = showAsTuple xs

instance Show Signature where
  show (Sig n t) = n ++ " : " ++ show t

-- | TODO! show typedefs
instance Show (Game a) where
  show (Game n b i vs _) = "Game : " ++ n ++ "\n"
                           ++ show b ++ "\n"
                           ++ show i ++ "\n"
                           ++ intercalate ("\n\n\n") (map show vs)

instance Show Op where
  show Plus     = " + "
  show Minus    = " - "
  show Times    = " * "
  show Div      = " / "
  show Mod      = " % "
  show Less     = " < "
  show Leq      = " <= "
  show Equiv    = " == "
  show NotEquiv = " /= "
  show Geq      = " >= "
  show Greater  = " > "
  show Get      = " ! "
