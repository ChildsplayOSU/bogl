{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract syntax for BOGL

module Language.Syntax where
import Language.Types
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

-- | Equations:
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
          | HE Name
          -- the last Expr can always be constructed from the [Name], but it makes the code cleaner to do that only once while parsing
   deriving (Eq, Data)

instance Show Expr where
  show (HE n) = "?" ++ n
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
