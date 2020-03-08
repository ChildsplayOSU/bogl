{-# LANGUAGE DeriveDataTypeable #-}
-- | Spiel language AST.
-- this slightly deviates from the specified syntax in spots.

module Language.Syntax where
import Language.Types
import Data.List
import Text.JSON.Generic
import Data.Array
import qualified Data.Set as S
type Name = String

-- | Game datatype
data Game a = Game
  {
    name  :: Name -- ^ The name of the game
  , board :: BoardDef -- ^ Size and type of the board
  , input :: InputDef -- ^ Type of input
  , defns ::[ValDef a] -- ^ List of value definitions
  }
  deriving (Data)

instance Show (Game a) where
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





data ValDef a = Val Signature (Equation a) a
  | BVal Signature [BoardEq a] a
   deriving (Eq, Data)

instance Functor ValDef where
  fmap f (Val s e a) = Val s (fmap f e) (f a)
  fmap f (BVal s e a) = BVal s ((fmap . fmap) f e) (f a)

instance Show (ValDef a) where
  show (Val s e _) = show s ++ "\n" ++ show e
  show (BVal s e _) = show s ++ "\n" ++ show e

ident :: (ValDef a) -> Name
ident (Val (Sig n _) _ _) = n
ident (BVal (Sig n _) _ _) = n

-- | Equations:
data Equation a = Veq Name (Expr a)        -- ^ Value equations (a mapping from 'Name' to 'Expr')
              | Feq Name Parlist (Expr a) -- ^ Function equations (a 'Name', list of params 'Parlist', and the 'Expr' that may use them
   deriving (Eq, Data)

instance Functor Equation where
  fmap f (Veq n e) = Veq n (fmap f e)
  fmap f (Feq n ps e) = Feq n ps (fmap f e)

instance Show (Equation a) where
  show (Veq n e) = n ++ " = " ++ show e
  show (Feq n p e) = n ++ show p ++ " = " ++ show e

-- | Board equations are used to set positions on the board to an expression
data BoardEq a = PosDef
   {
    boardEqName :: Name,
    xpos :: Pos, 
    ypos :: Pos,
    boardExpr :: (Expr a)
   }
   deriving (Eq, Data)

instance Functor BoardEq where
  fmap f (PosDef n p1 p2 e) = PosDef n p1 p2 (fmap f e)

instance Show (BoardEq a) where
   show (PosDef n x y e) = n ++ "(" ++ show x ++ ", " ++ show y ++ ")" ++ " = " ++ show e 

data Pos = Index Int 
         | ForAll Name  
         deriving (Eq, Data)

instance Show Pos where 
   show (Index i) = show i 
   show (ForAll n) = n 

instance Ord Pos where
  compare (Index i) (Index j) = compare i j
  compare (ForAll _) (_) = LT
  compare (_) (ForAll _) = GT

 -- | Expressions
data Expr a = I Int                     -- ^ Integer expression
          | S Name                        -- ^ Symbol
          | B Bool                        -- ^ Boolean
          | Ref Name                      -- ^ Reference to a variable
          | Tuple [Expr a]                  -- ^ Tuple of 'Expr'
          | App Name [Expr a]                -- ^ Application of the function called Name to the list of arguments
          | Binop Op (Expr a) (Expr a)            -- ^ Binary operation of two expressions
          | Let Name (Expr a) (Expr a)           -- ^ Let binding
          | If (Expr a) (Expr a) (Expr a)             -- ^ Conditional expression
          | While (Expr a) (Expr a) [Name] (Expr a)   -- ^ While: condition, body, names of arguments from the wrapper function, (tuple of) expression(s) which referenc(es) the name(s).
          -- the last Expr can always be constructed from the [Name], but it makes the code cleaner to do that only once while parsing
          | Annotation a (Expr a) -- ^ Parameterized by the type of an annotation (could use a cofree comonad, but also this)
          | HE Name -- ^ Type hole
   deriving (Eq, Data)

-- | this is just "deriving functor"
instance Functor Expr where
  fmap f (B x) = (B x)
  fmap f (HE n) = (HE n)
  fmap f (Annotation a e) = Annotation (f a) (fmap f e)
  fmap f (While e1 e2 ns e3) = While (fmap f e1) (fmap f e2) ns (fmap f e3)
  fmap f (If e1 e2 e3) = If (fmap f e1) (fmap f e2) (fmap f e3)
  fmap f (Let n e1 e2) = Let n (fmap f e1) (fmap f e2)
  fmap f (Binop o e1 e2) = Binop o (fmap f e1) (fmap f e2)
  fmap f (App n es) = App n (fmap (fmap f) es)
  fmap f (Tuple xs) = Tuple (fmap (fmap f) xs)
  fmap f (Ref n) = (Ref n)
  fmap f (S n) = (S n)
  fmap f (I x) = (I x)


deAnnotate :: Expr a -> Expr a
deAnnotate (Annotation a e) = e
deAnnotate x = x

clearAnn :: Expr a -> Expr ()
clearAnn = (() <$)



instance Show (Expr a) where
  show (Annotation _ e) = show e -- can refactor
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
